use crate::parse::{Node::*, NodeRef};

#[derive(Default)]
pub struct RuntimeEnv {
    pub stdout: String,
}

pub enum Error {
    NoFnMain,
    Unsupported { reason: String },
}

impl RuntimeEnv {
    pub fn eval_fn_main(&mut self, root: NodeRef) -> Result<(), Error> {
        // FIXME(eddyb) make a macro that expects a certain type of `Node`.
        let root_defs = match root {
            Mod(defs) => defs,
            _ => unreachable!(),
        };
        let fn_main = root_defs
            .iter()
            .find(|def| matches!(def, Fn { name: "main", .. }))
            .ok_or(Error::NoFnMain)?;
        self.eval_fn(fn_main, root_defs)
    }

    pub fn eval_fn(&mut self, f: NodeRef, root_defs: &[NodeRef]) -> Result<(), Error> {
        // FIXME(eddyb) make a macro that expects a certain type of `Node`.
        let body = match f {
            Fn { body, .. } => body,
            _ => unreachable!(),
        };
        self.eval_expr(body, root_defs)
    }

    pub fn eval_expr(&mut self, e: NodeRef, root_defs: &[NodeRef]) -> Result<(), Error> {
        match e {
            Mod(_) | Fn { .. } | ErrUnsupportedSyntax => unreachable!(),

            EUnit => Ok(()),
            ESeq(a, b) => {
                self.eval_expr(a, root_defs)?;
                self.eval_expr(b, root_defs)
            }

            ECall {
                func: EPath { name: callee_name },
            } => {
                let f = root_defs
                    .iter()
                    .find(|def| matches!(def, Fn { name, .. } if callee_name == name))
                    .ok_or_else(|| Error::Unsupported {
                        reason: format!("function not found {:?}", callee_name),
                    })?;
                self.eval_fn(f, root_defs)
            }

            // FIXME(eddyb) name resolve (and even expand) macros.
            EMacCall {
                name: "println",
                args: [LStr(s)],
            } => {
                self.stdout += s;
                self.stdout += "\n";
                Ok(())
            }
            EMacCall { name, args } => Err(Error::Unsupported {
                reason: format!("unsupported macro `{}!` (with arguments `{:?})", name, args),
            }),

            EPath { .. } | ECall { .. } | LStr(_) => Err(Error::Unsupported {
                reason: format!("{:?}", e),
            }),
        }
    }
}
