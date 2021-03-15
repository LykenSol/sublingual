use rayon::prelude::*;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::{env, fmt, fs, thread};
use sublingual_rust as subrust;
use walkdir::WalkDir;

/// Debug formatting for strings that shows them verbatim, sorrounded with
/// "code fence" markdown syntax (i.e. triple backticks on separate lines).
#[derive(Eq, PartialEq)]
struct CodeBlocks<S: AsRef<str>>(S);
impl<S: AsRef<str>> fmt::Debug for CodeBlocks<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("```\n")?;
        f.write_str(self.0.as_ref())?;
        f.write_str("```")
    }
}

/// Compile a single `.rs` file with `rustc` and return a path to the executable,
/// if successful (in case of error, the compilation stderr is returned instead).
fn rustc_file_to_exe(path: &Path) -> Result<PathBuf, String> {
    // HACK(eddyb) generate a known-good path in a more principled way.
    let out_exe_dir = Path::new("../target").join(path);
    let _ = fs::create_dir_all(&out_exe_dir);
    let out_exe = out_exe_dir.join("out".to_string() + env::consts::EXE_SUFFIX);
    let output = Command::new("rustc")
        .arg(path)
        .arg("-o")
        .arg(&out_exe)
        .output()
        .unwrap();
    if !output.status.success() {
        Err(String::from_utf8(output.stderr).unwrap())
    } else {
        Ok(out_exe)
    }
}

struct Unsupported;

#[derive(Debug)]
enum Outcome {
    /// Nothing to run, as there is no `fn main`.
    NoFnMain,

    /// `rustc` failed with an error we didn't detect.
    OnlyRustcFailed(CodeBlocks<String>),

    RanSuccessfully {
        stdout: String,
    },
}

fn test_file(path: &Path) -> Result<Outcome, Unsupported> {
    // HACK(eddyb) bypass some `syn` stack overflows.
    if path.ends_with("src/test/ui/issues/issue-74564-if-expr-stack-overflow.rs") {
        return Err(Unsupported);
    }

    // HACK(eddyb) more `syn` stack overflows, but only in debug mode.
    #[cfg(debug_assertions)]
    {
        let skip_paths = [
            "src/test/ui/super-fast-paren-parsing.rs",
            "src/test/ui/closures/deeply-nested_closures.rs",
            "src/test/ui/weird-exprs.rs",
        ];
        for skip_path in &skip_paths {
            if path.ends_with(skip_path) {
                return Err(Unsupported);
            }
        }
    }

    let path = path.to_path_buf();
    thread::Builder::new()
        .name(path.to_string_lossy().into())
        .spawn(move || {
            let root = subrust::parse::Node::read_and_parse_file(&path).map_err(|_| Unsupported)?;

            let mut runtime_env = subrust::eval::RuntimeEnv::default();
            match runtime_env.eval_fn_main(root) {
                Ok(()) => {
                    // Also run by compiling with `rustc`, and compare outputs.
                    match rustc_file_to_exe(&path) {
                        Ok(exe) => {
                            let exe_output = Command::new(exe).output().unwrap();
                            assert!(exe_output.status.success());
                            pretty_assertions::assert_eq!(
                                CodeBlocks(&String::from_utf8(exe_output.stdout).unwrap()),
                                CodeBlocks(&runtime_env.stdout)
                            );
                            pretty_assertions::assert_eq!(
                                CodeBlocks(&String::from_utf8(exe_output.stderr).unwrap()[..]),
                                CodeBlocks("")
                            );

                            Ok(Outcome::RanSuccessfully {
                                stdout: runtime_env.stdout,
                            })
                        }
                        Err(rustc_stderr) => Ok(Outcome::OnlyRustcFailed(CodeBlocks(rustc_stderr))),
                    }
                }
                Err(subrust::eval::Error::NoFnMain) => Ok(Outcome::NoFnMain),
                Err(subrust::eval::Error::Unsupported { .. }) => Err(Unsupported),
            }
        })
        .unwrap()
        .join()
        .unwrap()
}

#[test]
fn official_testsuite() {
    let data_dir = Path::new("official-testsuite-data");
    let expected_path = data_dir.join("expected");
    let expected = fs::read_to_string(&expected_path).unwrap_or_default();
    let test_dir = data_dir.join("rust/src/test");

    let files = WalkDir::new(&test_dir)
        .into_iter()
        .map(Result::unwrap)
        .filter(|entry| entry.path().extension().map_or(false, |ext| ext == "rs"));

    let found = format!(
        "{:#?}\n",
        files
            .par_bridge()
            .filter_map(|entry| {
                Some((
                    entry.path().strip_prefix(&test_dir).unwrap().to_path_buf(),
                    test_file(entry.path()).ok()?,
                ))
            })
            .collect::<BTreeMap<_, _>>()
    );

    if found != expected {
        if env::var("BLESS").is_ok() {
            fs::write(expected_path, found).unwrap();
        } else {
            panic!(
                "tests results differ from `{}`:\n\n\
                 {}\n\
                 note: if the new results are correct, you can replace `{0}`\n\
                 with them automatically, by re-running the tests with the `BLESS` environment variable set\n\n",
                expected_path.display(),
                pretty_assertions::Comparison::new(
                    &CodeBlocks(expected),
                    &CodeBlocks(found)
                )
            );
        }
    }
}
