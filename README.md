# docvim: a documentation generator for Vim plug-ins

docvim is a documentation generator for Vim plug-ins, written in Haskell.

## Development

### Set-up

```
cabal sandbox init
cabal install --only-dependencies --enable-tests
cabal build
```

### Running

Run using `cabal run` and passing in docvim-specific `OPTIONS`:

```
cabal run -- [OPTIONS]
```

You can also run the modules from inside the Cabal REPL:

```
cabal repl
> import Docvim.Parse
> let sample = "let l:test=1"
> pp sample -- pretty-prints AST
```

### Building and viewing the code-level documentation

```
cabal haddock --executables
open dist/doc/html/docvim/docvim/index.html
```

### Testing

```
cabal test
```

### Linting

```
cabal install hlint
hlint src # or, alternatively...
cabal test
```
