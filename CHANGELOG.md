# 0.3.2.1 (25 June 2016)

- Add missing fixture file to distribution, to fix Stackage breakage.

# 0.3.2.0 (21 June 2016)

- Add initial support for processing `@function` annotations.

# 0.3.1.6 (18 June 2016)

- Republish using only lower bounds to avoid getting blacklisted from Stackage.

# 0.3.1.5 (17 June 2016)

- Distribute signed package via Stackage.

# 0.3.1.4 (17 June 2016)

- Add upper and lower bounds to version constraints.

# 0.3.1.3 (16 June 2016)

- More tweaks for the benefit of Travis-CI.

# 0.3.1.2 (16 June 2016)

- Include fixture files so that tests can be run in Travis-CI and Stackage.

# 0.3.1.1 (15 June 2016)

- Relax "base" version constraint after verifying compatibility with GHC 8.

# 0.3.1.0 (12 June 2016)

- Squelch warning about exposed `Paths_docvim` module.

# 0.3.0.0 (12 June 2016)

- Fix explicit links to headings in GitHub Markdown.
- Plug-in link targets  at the top level (eg. `*foo.txt*` and `*foo`) are now downcased.
- Info extracted from the `@plugin` annotation is now centered in the Vim output.
- The `-d`/`--debug` flag now causes a dump of the compiled AST to be logged to the standard error.
- Moved under the `Text.*` namespace, to `Text.Docvim.*`.
- Fix ordering issues with synthesized headings.

# 0.2.0.0 (9 June 2016)

- New version number (testing release process).

# 0.1.0.0 (9 June 2016)

- Initial preview release.
