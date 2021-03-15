use crate::parse::Node::{self, *};

#[derive(Default)]
pub struct RuntimeEnv {
    pub stdout: String,
}

pub enum Error {
    NoFnMain,
    Unsupported { reason: String },
}

impl RuntimeEnv {
    pub fn eval_fn_main(&mut self, root: &Node) -> Result<(), Error> {
        // FIXME(eddyb) make a macro that expects a certain type of `Node`.
        let root_defs = match root {
            Mod(defs) => defs,
            _ => unreachable!(),
        };
        let fn_main = root_defs
            .iter()
            .find(|&def| matches!(&**def, Fn { name, .. } if &name[..] == "main"))
            .ok_or(Error::NoFnMain)?;
        self.eval_fn(fn_main)
    }

    pub fn eval_fn(&mut self, f: &Node) -> Result<(), Error> {
        // FIXME(eddyb) make a macro that expects a certain type of `Node`.
        let body = match f {
            Fn { body, .. } => body,
            _ => unreachable!(),
        };
        self.eval_expr(body)
    }

    pub fn eval_expr(&mut self, e: &Node) -> Result<(), Error> {
        match e {
            Mod(_) | Fn { .. } => unreachable!(),

            EUnit => Ok(()),
            ESeq(a, b) => {
                self.eval_expr(a)?;
                self.eval_expr(b)
            }
            EMacCall { name, args } => {
                // FIXME(eddyb) name resolve (and even expand) macros.
                match &name[..] {
                    "println" => match &args[..] {
                        [lit] => match &**lit {
                            LStr(s) => {
                                self.stdout += s;
                                self.stdout += "\n";
                                Ok(())
                            }
                            _ => Err(Error::Unsupported {
                                reason: format!(
                                    "first argument of `{}!` not a string literal",
                                    name
                                ),
                            }),
                        },
                        _ => Err(Error::Unsupported {
                            reason: format!("formatting in `{}!`", name),
                        }),
                    },
                    _ => Err(Error::Unsupported {
                        reason: format!("unknown macro `{}!`", name),
                    }),
                }
            }

            LStr(_) => Err(Error::Unsupported {
                reason: format!("{:?}", e),
            }),
        }
    }
}
