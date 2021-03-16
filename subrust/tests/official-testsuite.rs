use indexmap::IndexMap;
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

#[derive(Debug)]
enum Outcome {
    /// Completely ignored, in order to avoid e.g. stack overflows in `syn`.
    Skipped,

    /// Parsing with `syn` failed.
    // FIXME(eddyb) make sure that `rustc` *also* fails to parse the same file.
    SynFailed,

    Unsupported {
        reason: String,
    },

    /// Nothing to run, as there is no `fn main`.
    NoFnMain,

    /// `rustc` failed with an error we didn't detect.
    OnlyRustcFailed(CodeBlocks<String>),

    RanSuccessfully {
        stdout: String,
    },
}

fn test_file(path: &Path) -> Outcome {
    // HACK(eddyb) bypass some `syn` stack overflows.
    if path.ends_with("src/test/ui/issues/issue-74564-if-expr-stack-overflow.rs") {
        return Outcome::Skipped;
    }

    // HACK(eddyb) more `syn` stack overflows, but only in debug mode.
    let skip_only_in_debug_paths = [
        "src/test/ui/super-fast-paren-parsing.rs",
        "src/test/ui/closures/deeply-nested_closures.rs",
        "src/test/ui/weird-exprs.rs",
    ];
    let skip_only_in_debug = skip_only_in_debug_paths
        .iter()
        .any(|skip_path| path.ends_with(skip_path));
    if skip_only_in_debug && cfg!(debug_assertions) {
        return Outcome::Skipped;
    }

    let path_buf = path.to_path_buf();
    let outcome = thread::Builder::new()
        .name(path.to_string_lossy().into())
        .spawn(move || {
            let path = &path_buf;
            let root = match subrust::parse::Node::read_and_parse_file(path) {
                Ok(root) => root,
                Err(subrust::parse::ParseError::Syn(_)) => return Outcome::SynFailed,
                Err(subrust::parse::ParseError::Unsupported(subrust::parse::Unsupported {
                    reason,
                    ..
                })) => return Outcome::Unsupported { reason },
            };

            let mut runtime_env = subrust::eval::RuntimeEnv::default();
            match runtime_env.eval_fn_main(root) {
                Ok(()) => {
                    // Also run by compiling with `rustc`, and compare outputs.
                    match rustc_file_to_exe(path) {
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

                            Outcome::RanSuccessfully {
                                stdout: runtime_env.stdout,
                            }
                        }
                        Err(rustc_stderr) => Outcome::OnlyRustcFailed(CodeBlocks(rustc_stderr)),
                    }
                }
                Err(subrust::eval::Error::NoFnMain) => Outcome::NoFnMain,
                Err(subrust::eval::Error::Unsupported { reason }) => {
                    Outcome::Unsupported { reason }
                }
            }
        })
        .unwrap()
        .join()
        .unwrap();

    // HACK(eddyb) if we'd be skipping this file in debug mode, make sure we can't
    // start supporting it accidentally and cause debug vs release differences.
    if skip_only_in_debug {
        match outcome {
            Outcome::Unsupported { .. } => {}
            _ => panic!(
                "`{}` would be skipped in debug mode, but instead of `Unsupported` it's `{:#?}`",
                path.display(),
                outcome
            ),
        };
        return Outcome::Skipped;
    }

    outcome
}

#[test]
fn official_testsuite() {
    let data_dir = Path::new("official-testsuite-data");
    let test_dir = data_dir.join("rust/src/test");

    let files = WalkDir::new(&test_dir)
        .into_iter()
        .map(Result::unwrap)
        .filter(|entry| entry.path().extension().map_or(false, |ext| ext == "rs"));

    #[derive(Default)]
    struct TestResults {
        reported_outcomes: BTreeMap<PathBuf, Outcome>,
        unsupported_counts: IndexMap<String, usize>,
    }

    let mut results = files
        .par_bridge()
        .fold(TestResults::default, |mut results, entry| {
            let outcome = test_file(entry.path());
            match outcome {
                Outcome::Skipped | Outcome::SynFailed => {}
                Outcome::Unsupported { mut reason } => {
                    // HACK(eddyb) avoid polluting the output with very long AST dumps.
                    if reason.len() > 81 {
                        reason = format!("{}…{}", &reason[..40], &reason[reason.len() - 40..]);
                    }
                    *results.unsupported_counts.entry(reason).or_default() += 1;
                }
                _ => {
                    results.reported_outcomes.insert(
                        entry.path().strip_prefix(&test_dir).unwrap().to_path_buf(),
                        outcome,
                    );
                }
            }
            results
        })
        .reduce(TestResults::default, |mut a, b| {
            a.reported_outcomes.extend(b.reported_outcomes);
            for (reason, count) in b.unsupported_counts {
                *a.unsupported_counts.entry(reason).or_default() += count;
            }
            a
        });

    // Sort unsupported (per-reason) counts, by count, descending.
    results
        .unsupported_counts
        .sort_by(|a_reason, a_count, b_reason, b_count| {
            let (a, b) = ((a_reason, a_count), (b_reason, b_count));
            let key = |(reason, count)| (std::cmp::Reverse(count), reason);
            key(a).cmp(&key(b))
        });

    for (found, expected_path) in &[
        (
            format!("{:#?}\n", results.reported_outcomes),
            data_dir.join("expected"),
        ),
        (
            format!("{:#?}\n", results.unsupported_counts),
            data_dir.join("unsupported"),
        ),
    ] {
        let expected = fs::read_to_string(&expected_path).unwrap_or_default();
        if *found != expected {
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
}
