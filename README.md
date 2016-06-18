# docvim: a documentation generator for Vim plug-ins

[![Build Status](https://travis-ci.org/wincent/docvim.svg?branch=master)](https://travis-ci.org/wincent/docvim) [![docvim on Stackage LTS 3](http://stackage.org/package/docvim/badge/lts)](http://stackage.org/lts/package/docvim) [![docvim on Stackage Nightly](http://stackage.org/package/docvim/badge/nightly)](http://stackage.org/nightly/package/docvim)

docvim is a documentation generator for Vim plug-ins, written in Haskell.

## Quickstart

```bash
# Print Markdown-formatted help documentation for files in current directory
docvim

# Write Markdown README + Vim help text file for $PLUGIN
docvim -c ~/code/$PLUGIN ~/code/$PLUGIN/doc/$PLUGIN.txt ~/code/$PLUGIN/README.md
```

## Usage

```
docvim - a documentation generator for Vim plug-ins

Usage: docvim [--version] [OUTFILES...] [-d|--debug] [-c|--directory DIRECTORY]
              [-v|--verbose]
  Generate documentation for a Vim plug-in

Available options:
  -h,--help                Show this help text
  --version                Print version information
  OUTFILES...              Target file(s) for generated output (default:
                           standard output)
  -d,--debug               Print debug information during processing
  -c,--directory DIRECTORY Change to DIRECTORY before processing (default: ".")
  -v,--verbose             Be verbose during processing
```

## Installation

```
# Stack:
stack install docvim

# Cabal:
cabal install docvim
```

## Syntax

```vim
""
" Docblocks start with a pair of double quotes, followed
" by standard Vim comments (with a double quote prefix)
" containing Markdown-like text and optional annotations
" that look like this:
"
" ```
" @command :Ack {pattern} {options}
" ```
```

### Supported Markdown features

    # Top-level heading

    ## Sub-heading

    --- (Horizontal dividers)

    > Blockquote

    `inline code`

    ```
    fenced codeblocks (leading space syntax not supported)
    ```

    ![alt text](http://example.com/image.jpg)
    (becomes a link in vimdoc, but an image in markdown)

    - Lists.

### Unsupported Markdown syntax

```
*foo* (emphasis; turns into Vim doc targets instead)

*,+ (list syntax; just use - instead)

<html> (we don't want ambiguity with things like <leader> and so on)
```

### Annotations

- `@command`
- `@commands`
- `@dedent`
- `@footer`
- `@function`
- `@functions`
- `@indent`
- `@mapping`
- `@mappings`
- `@option`
- `@options`
- `@plugin`

## Development

### Convenience wrappers

```bash
bin/accept  # Accept current "golden" test output.
bin/docvim  # Run the docvim executable.
bin/golden  # Run just the "golden" tests.
bin/haddock # Produce Haddock documentation.
bin/lint    # Run the linter.
bin/tasty   # Run just the Tasty tests.
bin/test    # Run all tests, including lints.
```

These are wrappers for the explicit invocations described below.

### Set-up

You can set-up a development environment using [Stack] (recommended) or [Cabal]:

```bash
# Stack:
brew install haskell-stack
stack build

# Cabal:
brew install cabal-install
cabal sandbox init
cabal install --only-dependencies --enable-tests
cabal build
```

### Running

Run using `stack exec` (or `cabal run`) and passing in docvim-specific `OPTIONS`:

```bash
# Stack:
stack exec docvim [OPTIONS]

# Cabal:
cabal run -- [OPTIONS]
```

You can also run the modules from inside the REPL:

```bash
# Stack:
stack repl
> pp "let l:test=1" -- pretty-prints AST

# Cabal:
cabal repl
> import Text.Docvim.Parse
> pp "let l:test=1" -- pretty-prints AST
```

### Building

```bash
stack build --file-watch
```

### Building and viewing the code-level documentation

```bash
# Stack:
stack haddock
open .stack-work/dist/x86_64-osx/Cabal-1.22.5.0/doc/html/docvim/index.html

# Cabal:
cabal haddock --executables
open dist/doc/html/docvim/docvim/index.html
```

### Testing

```bash
# Stack:
stack test        # Runs all test suites, including linting.
stack test :tasty # Runs just the Tasty test suite.

# Cabal:
cabal test       # Runs all test suites, including linting.
cabal test tasty # Runs just the Tasty test suite.
```

#### Updating "golden" files

```bash
# Stack:
stack test --test-arguments=--accept          # Runs all test suites.
stack test :tasty --test-arguments=--accept   # Runs just the Tasty test suite.

# Cabal:
cabal test --test-options=---accept           # Runs all test suites.
cabal test tasty --test-options=---accept     # Runs just the Tasty test suite.
```

### Linting

```bash
# Stack:
stack test              # Runs linter as part of overall set of suites.
stack test :hlint       # Runs linter alone.

# Cabal:
cabal install hlint     # (First-time only).
cabal test              # Runs linter as part of overall set of suites.
cabal test hlint        # Runs linter alone.

hlint src               # If you have HLint installed under $PATH.
```

### Release process

```bash
vim docvim.cabal # Update version number in two places.
vim CHANGELOG.md # Update, er, changelog.
git commit -p # git tag, git push --follow-tags etc...
stack sdist --pvp-bounds both
stack upload --pvp-bounds both
```

## Links

- [Hackage package](https://hackage.haskell.org/package/docvim)

### Examples of plug-ins using docvim

- [Ferret](https://github.com/wincent/ferret)
- [Pinnacle](https://github.com/wincent/pinnacle)
- [Scalpel](https://github.com/wincent/scalpel)
- [vim-docvim](https://github.com/wincent/vim-docvim)

## FAQ

### Why a new tool and not an existing one like [Vimdoc]?

* I wanted to target multiple output formats (Vim help files and Markdown).
* I wanted total control over the presentation of the output.
* It's fun to build new things from scratch.
* The project is a great fit for my learn-me-a-Haskell goal this year.

### Why is it called "docvim"?

"Vimdoc" was the first name that occurred to me when I started this project, but:

* The number one hit for "vimdoc" is [this online copy of Vim's own documentation](http://vimdoc.sourceforge.net/).
* The name "Vimdoc" is already taken by [a similar project](https://github.com/google/vimdoc).

So, in a remarkable flash of profound creativity, I settled on "docvim" instead, which right now yields this pleasing search result:

> Did you mean: dacvim

[Cabal]: https://www.haskell.org/cabal/
[Stack]: http://haskellstack.org/
[Vimdoc]: https://github.com/google/vimdoc
