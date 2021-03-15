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
        self.eval_fn(fn_main)
    }

    pub fn eval_fn(&mut self, f: NodeRef) -> Result<(), Error> {
        // FIXME(eddyb) make a macro that expects a certain type of `Node`.
        let body = match f {
            Fn { body, .. } => body,
            _ => unreachable!(),
        };
        self.eval_expr(body)
    }

    pub fn eval_expr(&mut self, e: NodeRef) -> Result<(), Error> {
        match e {
            Mod(_) | Fn { .. } => unreachable!(),

            EUnit => Ok(()),
            ESeq(a, b) => {
                self.eval_expr(a)?;
                self.eval_expr(b)
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

            LStr(_) => Err(Error::Unsupported {
                reason: format!("{:?}", e),
            }),
        }
    }
}
