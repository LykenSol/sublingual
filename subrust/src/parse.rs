//! Parse a subset of Rust using `syn`, so we don't need our parser for now.

use std::rc::Rc;
use syn::parse::Parser as _;

#[derive(Debug)]
pub enum Node {
    // Definitions.
    /// `mod { ... }`
    Mod(Rc<[Rc<Node>]>),
    /// `fn name() { body }`
    Fn {
        name: Rc<str>,
        body: Rc<Node>,
    },

    // Expressions.
    /// `()`
    EUnit,
    /// `a; b`
    ESeq(Rc<Node>, Rc<Node>),
    /// `name!(...args)`
    EMacCall {
        name: Rc<str>,
        args: Rc<[Rc<Node>]>,
    },

    // Literals.
    LStr(Rc<str>),
}

#[derive(Debug)]
pub struct Unsupported {
    span: proc_macro2::Span,
    reason: String,
}

#[derive(Debug)]
pub enum ParseError {
    Syn(syn::Error),
    Unsupported(Unsupported),
}

impl Node {
    pub fn read_and_parse_file(path: impl AsRef<std::path::Path>) -> Result<Rc<Self>, ParseError> {
        syn::parse_file(&std::fs::read_to_string(path).unwrap())
            .map_err(ParseError::Syn)?
            .lower()
            .map_err(ParseError::Unsupported)
    }
}

// NOTE(eddyb) this is not simply `impl From<syn::X> for Node` so that it can't
// be called elsehwere, and also if we ever want to introduce some context type.
trait Lower {
    type Lowered: ?Sized;
    fn lower(self) -> Result<Rc<Self::Lowered>, Unsupported>;
}

impl<T: Lower<Lowered = L>, L> Lower for Vec<T> {
    type Lowered = [Rc<L>];
    fn lower(self) -> Result<Rc<[Rc<L>]>, Unsupported> {
        self.into_iter().map(T::lower).collect()
    }
}

impl Lower for syn::Ident {
    type Lowered = str;
    fn lower(self) -> Result<Rc<str>, Unsupported> {
        Ok(self.to_string().into())
    }
}

macro_rules! lower_syn_enums {
    ($($name:ident { $($variant:ident),* $(,)? }),* $(,)?) => {
        $(impl Lower for syn::$name {
            type Lowered = Node;
            fn lower(self) -> Result<Rc<Node>, Unsupported> {
                match self {
                    $(syn::$name::$variant(x) => x.lower(),)*
                    _ => Err(Unsupported {
                        span: syn::spanned::Spanned::span(&self),
                        reason: format!(
                            "{}::{}",
                            stringify!($name),
                            format!("{:?}", self).split('(').next().unwrap(),
                        ),
                    }),
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

macro_rules! lower_syn_structs {
    ($($name:ident { $($fields:tt)* } $(= $this:ident)? $(@ $span:ident)? $(if $cond:expr)? => $node:expr),* $(,)?) => {
        $(impl Lower for syn::$name {
            type Lowered = Node;
            fn lower(self) -> Result<Rc<Node>, Unsupported> {
                let _span = syn::spanned::Spanned::span(&self);
                match self {
                    syn::$name { $($fields)* } => {
                        $(let $this = self;)?
                        $(let $span = _span;)?
                        $(if !$cond {
                            return Err(Unsupported {
                                span: _span,
                                reason: format!(
                                    "{}: `{}` didn't hold",
                                    stringify!($name),
                                    stringify!($cond),
                                ),
                            });
                        })?
                        let _node = {
                            #[allow(unused_imports)]
                            use self::Node::*;
                            $node
                        };
                        #[allow(unreachable_code)]
                        Ok(Rc::new(_node))
                    }
                    #[allow(unreachable_patterns)]
                    _ => Err(Unsupported {
                        span: _span,
                        reason: format!(
                            "pattern `{}` didn't match",
                            stringify!(syn::$name { $($fields)* }),
                        ),
                    }),
                }
            }
        })*
    };
}

// HACK(eddyb) helper for checking that certain fields are set to default values.
fn is_empty<T: Default + Eq>(x: T) -> bool {
    x == T::default()
}

lower_syn_structs! {
    File { shebang: _, attrs, items } if is_empty(attrs) => Mod(items.lower()?),

    ItemFn {
        attrs,
        vis: syn::Visibility::Inherited,
        sig: syn::Signature {
            constness: None,
            asyncness: None,
            unsafety: None,
            abi: None,
            fn_token: _,
            ident,
            generics,
            paren_token: _,
            inputs,
            variadic: None,
            output: syn::ReturnType::Default,
        },
        block,
    } if is_empty((attrs, generics, inputs))
    => Fn { name: ident.lower()?, body: block.lower()? },

    // FIXME(eddyb) this doesn't seem to fit the rest that well.
    Block { brace_token: _, mut stmts } @ span => {
        let mut expr = if let Some(syn::Stmt::Expr(_)) = stmts.last() {
            match stmts.pop().unwrap() {
                syn::Stmt::Expr(e) => e.lower()?,
                _ => unreachable!(),
            }
        } else {
            Rc::new(EUnit)
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
                    expr = Rc::new(ESeq(e.lower()?, expr));
                }
            }
        }
        return Ok(expr);
    },

    ExprLit { attrs, lit } if is_empty(attrs) => return lit.lower(),
    ExprMacro {
        attrs,
        mac: syn::Macro {
            path: syn::Path { leading_colon: None, segments },
            bang_token: _,
            delimiter: _,
            tokens,
        },
    } if is_empty(attrs) && segments.len() == 1 && segments.first().unwrap().arguments.is_empty()
    => EMacCall {
        name: segments.into_iter().next().unwrap().ident.lower()?,
        args: {
            let span = syn::spanned::Spanned::span(&tokens);
            syn::punctuated::Punctuated::<syn::Expr, syn::Token![,]>::parse_terminated
                .parse2(tokens).map_err(|_| {
                    Unsupported {
                        span,
                        reason: "ExprMacro with non-Expr inputs".to_string(),
                    }
                })?.into_iter().map(|e| e.lower()).collect::<Result<_, _>>()?
        },
    },

    LitStr { .. } = lit if lit.suffix().is_empty() => LStr(lit.value().into()),
}
