# sublingual: toy versions of existing programming languages

This is a collection of toy languages created by taking much "larger" languages
(e.g. Rust), and removing various features.

Each such toy language comes with:
* a prototype implementation
  * *always* an interpreter (which const-eval functionality can reuse)
  * optionally a simple backend (e.g. to WASM)
* lists of tests from test suites for the original language, and their behaviors
  * *ideally*, a test either behaves identically, or fails to compile
  * e.g. for Rust, this would involve `rust-lang/rust`'s `src/test`

Because of inherent difficulties in compiling dependencies (e.g. the standard
libraries for each language), some may be "mocked" instead.

## Motivation

The primary usecase for these toy languages is having fixed "test subjects" when
experimenting with different compiler architectures.

Using existing (relatively) "small" languages would seem straight-forward, but
in practice there aren't a lot of options, especially when trying to explore
implementation strategies for particular features that are only found in a few
"large" languages, or even just one.

The insight which allows us to arbitrarily chop off features, and keep testing,
is that the existing test suites of "large" languages can be mostly orthogonal,
be it intentional or by gradual evolution, such that many tests don't require
much beyond what they're explicitly testing.

Note that this is different from trying to compile large codebases, which would
be doomed to hit a lot more features, all at once - this includes libraries used
by tests (e.g. standard library, for Rust), and results in the potential need for
"mocking", as mentioned in the previous section.

Finally, taking this path was cemented by reading the blog post
[*How fast can we compile Rust hello world?*](https://www.jonathanturner.org/how-fast-can-we-compile-rust-hello-world/),
which describes a similar approach, but taken to its extreme - i.e. a compiler
that *only* supports "Hello World"-shaped programs.

## License

Licensed under either of

 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
