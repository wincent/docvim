# AST modelling

We have a few options for how to represent the AST and the related doc comments. At the time I write this, our lexer lexes comments but the Lua parser effectively ignores them; we produce an AST that is very literally "abstract" and not concrete in the sense that we cannot reconstruct the original source code from AST, but it does contain all the logical and structural information necessary to evaluate (execute) the program. We effectively lose information about whitespace and comments along the way.

For the purposes of Docvim, losing whitespace is ok and not being able to reconstruct the original source is fine too, but losing information about comments is not. After all, comments (and specifically doc comments) are _the_ principal vehicle for conveying the content we wish to include in the final documentation. The program-specific information encoded in the AST is actually only a secondary vehicle, so that we can do things like provide documentation for function signatures even if the author hasn't written doc comments for them. This is of limited utility in the Lua parser, because there is nothing much beyond function signatures that is of interest; in contrast, the Vimscript parser has access to much more pertinent entities, such as `:command` declarations, mappings, global variable assignments, and so on.

The question, then, is where to store information about doc comments. The major options include:

- **Embed comment information inside AST nodes as an additional, optional field.** Note that not all node types really require doc comments, but it still may be tricky to implement this approach because we may have to bake comment-awareness into the parser in a pervasive and verbose/unpleasant manner. Also note that because in Docvim doc comments always precede the thing they apply to, we may have to deal with some kind of nasty lookahead (or otherwise stashing comments in some kind of mutable "pending comment" data structure) in order to actually relate them to AST nodes.
- **Wrap all AST nodes in a generic wrapper that contains the concrete node plus an optional comment.**
- **Store comments in an entirely separate look-up data structure that indicates which AST node a given comment belongs to via some means:**
  - **Via an "XPath"-style mechanism for specifying a location within the tree.**
  - **By associating a comment with a token position in the lexical token stream (and in turn teaching each AST node to record which lexical tokens it originated from).**

Possible simplifying assumptions:

- In the Lua parser, at least, because we only care about function declarations, we can most likely get away with one of the simpler alternatives, deferring dealing with this problem in a more comprehensive fashion for when we get to the Vimscript parser[^never].

[^never]: Which may never happen, because I'm not that interested in writing Vimscript plug-ins in the future.

## Appendix

Node types that could meaningfully have comments attached in Docvim (based on [current contents of `docvim_parser/src/lua.rs`](https://github.com/wincent/docvim/blob/36abbf6201724cce9cf8c83fe010032713764211/libs/docvim_parser/src/lua.rs)):

### Expressions

```rust
pub enum Exp<'a> {
    Binary {
        lexp: Box<Exp<'a>>,
        op: BinOp,
        rexp: Box<Exp<'a>>,
    },
    CookedStr(Box<String>),
    False,
    FunctionCall {
        /// The "prefix expression" (ie. LHS in `foo(bar)`; that is, `foo`), which is the function
        /// to be called.
        pexp: Box<Exp<'a>>,
        args: Vec<Exp<'a>>,
    },
    Function {
        parlist: Vec<Name<'a>>,
        varargs: bool,
        block: Block<'a>,
    },
    Index {
        /// The "prefix expression" (ie. LHS in `foo[bar]; that is, `foo`), which is the table to
        /// be indexed.
        // TODO: decide whether we should type this more narrowly (bc prefixexp is a subset of Exp)...
        pexp: Box<Exp<'a>>,
        /// The "key expression" (ie. RHS in `foo[bar]`; that is, `bar`).
        kexp: Box<Exp<'a>>,
    },
    MethodCall {
        /// The "prefix expression" (ie. LHS in `foo:bar(baz)`; that is, `foo`), which is the table
        /// containing the method to be called.
        pexp: Box<Exp<'a>>,

        /// The method name (ie. the `bar` in `foo:bar(baz`) to be called.
        name: &'a str,
        args: Vec<Exp<'a>>,
    },
    NamedVar(&'a str),
    Nil,
    Number(&'a str),
    RawStr(&'a str),
    Table(Vec<Field<'a>>),
    True,
    Unary {
        exp: Box<Exp<'a>>,
        op: UnOp,
    },
    Varargs,
}
```

Nothing really of note in there, except:

- `Function`: Probably not interesting as an expression; more interesting will be the statement forms below (`FunctionDeclaration` and `LocalFunctionDeclaration`).
- `MethodCall`: Might be useful for detecting calls to `vim.opt:append()` and similar; again, this may typically be found in the form of a `MethodCallStatement`.
- `FunctionCall`: For similar reasons, would be interesting to detect calls to `vim.keymap.set()` and friends. Note that things like `vim.keymap` and `vim.keymap.set` are nested `Index` expressions, and that a `FunctionCallStatement` may be the more useful and frequently encountered form of this.

### Statements

```rust
pub enum Statement<'a> {
    Break,
    DoBlock(Block<'a>),
    ForIn {
        namelist: Vec<Name<'a>>,
        explist: Vec<Exp<'a>>,
        block: Block<'a>,
    },
    For {
        name: Name<'a>,
        startexp: Box<Exp<'a>>,
        endexp: Box<Exp<'a>>,
        stepexp: Option<Box<Exp<'a>>>,
        block: Block<'a>,
    },
    FunctionCallStatement {
        pexp: Box<Exp<'a>>,
        args: Vec<Exp<'a>>,
    },
    FunctionDeclaration {
        /// A name may consist of a single Name (eg. "foo") or multiple via property access (eg.
        /// "foo.bar", "foo.bar.baz" etc).
        name: Vec<Name<'a>>,

        /// A name may optionally terminate with a final "method" component (eg. "bar" in
        /// "foo:bar").
        method: Option<Name<'a>>,

        /// Named parameters (eg. "a" and "b" in "(a, b)").
        parlist: Vec<Name<'a>>,

        /// Does the function accept varags (eg. "...")?
        varargs: bool,
        block: Block<'a>,
    },
    IfStatement {
        consequents: Vec<Consequent<'a>>,
        alternate: Option<Block<'a>>,
    },
    LocalDeclaration {
        namelist: Vec<Name<'a>>,
        explist: Vec<Exp<'a>>,
    },
    LocalFunctionDeclaration {
        name: Name<'a>,
        parlist: Vec<Name<'a>>,
        varargs: bool,
        block: Block<'a>,
    },
    MethodCallStatement {
        pexp: Box<Exp<'a>>,
        name: &'a str,
        args: Vec<Exp<'a>>,
    },
    Repeat {
        block: Block<'a>,
        cexp: Box<Exp<'a>>,
    },
    Return(Option<Vec<Exp<'a>>>),
    VarlistDeclaration {
        varlist: Vec<Exp<'a>>,
        explist: Vec<Exp<'a>>,
    },
    While {
        cexp: Box<Exp<'a>>,
        block: Block<'a>,
    },
}
```

In addition to the already-mentioned `FunctionDeclaration` and `LocalFunctionDeclaration`:

- `LocalDeclaration` and `VarlistDeclaration` may be useful to inspect for the items they may contain (eg. `local something = something_interesting`, `other = something_else` etc).
