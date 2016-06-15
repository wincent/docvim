# master (not yet released)

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
