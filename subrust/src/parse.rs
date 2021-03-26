//! Parse a subset of Rust using `syn`, so we don't need our parser for now.

use syn::parse::Parser as _;

// HACK(eddyb) using leaked `Box<T>` as `&'static T` references for convenience.
pub type NodeRef = &'static Node;

#[derive(Debug)]
pub enum Node {
    // Definitions.
    /// `mod { ... }`
    Mod(&'static [NodeRef]),
    /// `fn name() { body }`
    Fn {
        name: &'static str,
        body: NodeRef,
    },

    // Expressions.
    /// `()`
    EUnit,
    /// `a; b`
    ESeq(NodeRef, NodeRef),
    /// `name!(...args)`
    EMacCall {
        name: &'static str,
        args: &'static [NodeRef],
    },

    // Literals.
    LStr(&'static str),

    // Errors.
    ErrUnsupportedSyntax,
}

#[derive(Debug)]
pub struct Unsupported {
    pub span: proc_macro2::Span,
    pub reason: String,
}

#[derive(Debug)]
pub enum ParseError {
    Syn(syn::Error),
    Unsupported(Vec<Unsupported>),
}

impl Node {
    pub fn read_and_parse_file(path: impl AsRef<std::path::Path>) -> Result<NodeRef, ParseError> {
        let LowerResult { value, errors } =
            syn::parse_file(&std::fs::read_to_string(path).unwrap())
                .map_err(ParseError::Syn)?
                .lower();
        if errors.is_empty() {
            Ok(value)
        } else {
            Err(ParseError::Unsupported(errors))
        }
    }
}

#[must_use]
struct LowerResult<T> {
    value: T,
    errors: Vec<Unsupported>,
}

impl<T> LowerResult<T> {
    fn ok(value: T) -> Self {
        LowerResult {
            value,
            errors: vec![],
        }
    }
}

// NOTE(eddyb) this is not simply `impl From<syn::X> for Node` so that it can't
// be called elsehwere, and also if we ever want to introduce some context type.
trait Lower {
    type Lowered: ?Sized + 'static;
    fn lower(self) -> LowerResult<&'static Self::Lowered>;
}

impl<T: Lower<Lowered = L>, L: 'static> Lower for Vec<T> {
    type Lowered = [&'static L];
    fn lower(self) -> LowerResult<&'static [&'static L]> {
        let mut all_errors = vec![];
        LowerResult {
            value: Box::leak(
                self.into_iter()
                    .map(T::lower)
                    .map(|LowerResult { value, errors }| {
                        all_errors.extend(errors);
                        value
                    })
                    .collect(),
            ),
            errors: all_errors,
        }
    }
}

impl Lower for syn::Ident {
    type Lowered = str;
    fn lower(self) -> LowerResult<&'static str> {
        LowerResult::ok(Box::leak(self.to_string().into()))
    }
}

macro_rules! lower_syn_enums {
    ($($name:ident { $($variant:ident),* $(,)? }),* $(,)?) => {
        $(impl Lower for syn::$name {
            type Lowered = Node;
            fn lower(self) -> LowerResult<NodeRef> {
                match self {
                    $(syn::$name::$variant(x) => x.lower(),)*
                    _ => LowerResult {
                        value: &Node::ErrUnsupportedSyntax,
                        errors: vec![Unsupported {
                            span: syn::spanned::Spanned::span(&self),
                            reason: format!(
                                "{}::{}",
                                stringify!($name),
                                format!("{:?}", self).split('(').next().unwrap(),
                            ),
                        }],
                    },
                }
            }
        })*
    }
}

lower_syn_enums! {
    Item { Fn },
    Expr { Lit, Macro },
    Lit { Str },
}

/// Helper used by the `unsupported(...)` feature in `lower_syn_structs!`.
// HACK(eddyb) split into two traits for coherence reasons.
trait IsAbsentViaEq {
    fn is_absent(&self) -> bool;
}
trait IsAbsentCustom {
    fn is_absent(&self) -> bool;
}

impl<T: Default + Eq> IsAbsentViaEq for T {
    fn is_absent(&self) -> bool {
        *self == Self::default()
    }
}

impl IsAbsentCustom for syn::Visibility {
    fn is_absent(&self) -> bool {
        match self {
            syn::Visibility::Inherited => true,
            _ => false,
        }
    }
}

impl IsAbsentCustom for syn::ReturnType {
    fn is_absent(&self) -> bool {
        match self {
            syn::ReturnType::Default => true,
            _ => false,
        }
    }
}

macro_rules! lower_syn_structs {
    (
        $(
            $name:ident { $($fields:tt)* }
            $(= $this:ident)?
            $(@ $span:ident)?
            $(unsupported($($unsupported:expr),+ $(,)?))?
            => $node:expr
        ),* $(,)?
    ) => {
        $(impl Lower for syn::$name {
            type Lowered = Node;
            fn lower(self) -> LowerResult<NodeRef> {
                let mut all_errors = vec![];

                let _span = syn::spanned::Spanned::span(&self);
                #[deny(unused_variables)]
                let syn::$name { $($fields)* } = self;
                $(#[deny(unused_variables)] let $this = self;)?
                $(#[deny(unused_variables)] let $span = _span;)?
                $($(if !$unsupported.is_absent() {
                    all_errors.push(Unsupported {
                        span: _span,
                        reason: format!(
                            "{}: has `{}`",
                            stringify!($name),
                            stringify!($unsupported),
                        ),
                    });
                })+)?

                // HACK(eddyb) since `x.lower()?` can't work with `LowerResult`,
                // `lower!(x)` is the closest we can get.
                #[allow(unused_macros)]
                macro_rules! lower {
                    ($e:expr) => {
                        match $e.lower() {
                            LowerResult { value, errors } => {
                                all_errors.extend(errors);
                                value
                            }
                        }
                    };
                }

                let result: Result<_, Unsupported> = (|| {
                    #[allow(unused_imports)]
                    use self::Node::*;
                    Ok($node)
                })();

                let result = result.map_err(|error| {
                    all_errors.push(error);
                });

                LowerResult {
                    value: match result {
                        Ok(node) => Box::leak(Box::new(node)),
                        Err(()) => &Node::ErrUnsupportedSyntax,
                    },
                    errors: all_errors,
                }
            }
        })*
    };
}

lower_syn_structs! {
    File { shebang: _, attrs, items } unsupported(attrs) => Mod(lower!(items)),

    ItemFn {
        attrs,
        vis,
        sig: syn::Signature {
            constness,
            asyncness,
            unsafety,
            abi,
            fn_token: _,
            ident,
            generics,
            paren_token: _,
            inputs,
            variadic,
            output,
        },
        block,
    } unsupported(
        attrs, vis,
        // FIXME(eddyb) shold probably lower `Signature` separately.
        constness, asyncness, unsafety, abi, generics, inputs, variadic, output,
    )
    => Fn { name: lower!(ident), body: lower!(block) },

    // FIXME(eddyb) this doesn't seem to fit the rest that well.
    Block { brace_token: _, mut stmts } @ span => {
        let mut expr = if let Some(syn::Stmt::Expr(_)) = stmts.last() {
            match stmts.pop().unwrap() {
                syn::Stmt::Expr(e) => lower!(e),
                _ => unreachable!(),
            }
        } else {
            Box::leak(Box::new(EUnit))
        };
        for stmt in stmts.into_iter().rev() {
            match stmt {
                syn::Stmt::Local(_) => return Err(Unsupported {
                    span,
                    reason: "Stmt::Local".to_string(),
                }),
                syn::Stmt::Item(_) => return Err(Unsupported {
                    span,
                    reason: "Stmt::Item".to_string(),
                }),

                // FIXME(eddyb) technically `a; b` and `c d` can differ, in that
                // `c` must have type `()` but `a` can have any type, and you can
                // observe this with e.g. `{ 0 } {}` vs `{ 0 }; {}`, but for now
                // it's simpler to just assume that they're all the same.
                syn::Stmt::Expr(e) | syn::Stmt::Semi(e, _) => {
                    expr = Box::leak(Box::new(ESeq(lower!(e), expr)));
                }
            }
        }
        expr
    },

    ExprLit { attrs, lit } unsupported(attrs) => lower!(lit),
    ExprMacro {
        attrs,
        mac: syn::Macro {
            path: syn::Path { leading_colon, segments },
            bang_token: _,
            delimiter: _,
            tokens,
        },
    } unsupported(
        attrs,
        // FIXME(eddyb) shold probably lower `Path` separately.
        leading_colon, segments.iter().skip(1).collect::<Vec<_>>(), &segments[0].arguments,
    )
    => EMacCall {
        name: lower!(segments.into_iter().next().unwrap().ident),
        args: {
            let span = syn::spanned::Spanned::span(&tokens);
            let args =
                syn::punctuated::Punctuated::<syn::Expr, syn::Token![,]>::parse_terminated
                    .parse2(tokens).map_err(|_| {
                        Unsupported {
                            span,
                            reason: "ExprMacro with non-Expr inputs".to_string(),
                        }
                    })?;
            // FIXME(eddyb) this is a bit expensive but it avoid replicating all
            // of the error handling logic in the `Lower` impl for `Vec<T>`.
            lower!(args.into_iter().collect::<Vec<_>>())
        },
    },

    LitStr { .. } = lit unsupported(lit.suffix()) => LStr(Box::leak(lit.value().into())),
}
