# docvim: a documentation generator for Vim plug-ins

docvim is a documentation generator for Vim plug-ins, written in Haskell.

## Development

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
cabal repl`
> import Docvim.Parse
> pp "let l:test=1" -- pretty-prints AST
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
stack test

# Cabal:
cabal test
```

### Linting

```bash
# Stack:
stack test              # Runs linter as part of overall suite.
stack test :hlint       # Runs the linter alone.

# Cabal:
cabal install hlint     # (First-time only).
cabal test              # Runs linter as part of overall suite.
cabal test hlint        # Runs linter alone.

hlint src               # If you have HLint installed under $PATH.
```

[Cabal]: https://www.haskell.org/cabal/
[Stack]: http://haskellstack.org/
