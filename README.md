# docvim: a documentation generator for Vim plug-ins

docvim is a documentation generator for Vim plug-ins, written in Haskell.

## Development

### Running

Run using `cabal` and passing in docvim-specific `OPTIONS`:

```
cabal run -- [OPTIONS]
```

You can also run the modules from inside the Cabal REPL:

```
cabal repl
> let sample = "let l:test=1"
> l sample -- prints lexed tokens
> p sample -- prints AST
```

### Building and viewing the code-level documentation

```
cabal haddock --executables
open dist/doc/html/docvim/docvim/index.html
```

### Linting

```
cabal install hlint
hlint src
```
