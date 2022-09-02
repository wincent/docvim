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
