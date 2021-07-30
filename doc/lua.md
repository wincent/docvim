# Lua 5.1 grammar

## Lexical tokens

- Names/identifiers: "any string of letters, digits, and underscores, not beginning with a digit ... "any character considered alphabetic by the current locale can be used"

## Reserved keywords

- `and`
- `break`
- `do`
- `else`
- `elseif`
- `end`
- `false`
- `for`
- `function`
- `if`
- `in`
- `local`
- `nil`
- `not`
- `or`
- `repeat`
- `return`
- `then`
- `true`
- `until`
- `while`

## Unary operators

- `#`
- `-`
- `not`

## Binary operators

Binary operators are all left-associative, with the exception of `..` and `^`.

- `%`
- `*`
- `+`
- `,`
- `-`
- `.`
- `..`
- `/`
- `:`
- `;`
- `<`
- `<=`
- `=`
- `==`
- `>`
- `>=`
- `[`
- `]`
- `^`
- `{`
- `}`
- `~=`

## Operator precedence

From lowest to highest:

- `or`
- `and`
- `<`, `>`, `<=`, `>=`, `~=`, and `==`
- `..`
- `+`
- `-` (binary)
- `*`, `/`, `%`
- `not`, `#`, and `-` (unary)
- `^`

## Other tokens

- `...`
- `(`
- `)`

## References

- [Lua 5.1 Reference Manual](https://www.lua.org/manual/5.1/manual.html).
- [Parsing Expressions by Recursive Descent](https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm) (general survey of techniques).
- [Simple but Powerful Pratt Parsing](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html) (Rust tutorial).
- [Top Down Operator Precendence](http://crockford.com/javascript/tdop/tdop.html) (another article on Pratt's algorithm).

## Appendix: Expression parsing

Given the expression grammar from the Lua manual:

```
exp ::= prefixexp
exp ::= nil | false | true
exp ::= Number
exp ::= String
exp ::= function
exp ::= tableconstructor
exp ::= `...´
exp ::= exp binop exp
exp ::= unop exp
prefixexp ::= var | functioncall | `(´ exp `)´
```

We can simplify that by grouping together various terminal and non-terminal productions — all those that don't involve operators — under a "primary" production:

```
exp ::= | primary
        | exp binop exp
        | unop exp

primary ::= | nil | false | true
            | Number
            | String
            | function
            | tableconstructor
            | `...´
            | var | functioncall | `(´ exp `)`
```

And we can express that in a form that avoids right-recursive rules, where `{...}` indicates "zero or more repetitions" and can be expressed in terms of a loop rather than a left-recursive call:

```
exp ::= primary { binop primary }

primary ::= | nil | false | true (etc, as above...)
            | unop primary
```

We can enforce associativity and precedence by applying the Pratt parsing algorithm in conjunction with these left and right binding "powers":

| Operator                         | Left binding power | Right binding power |
| -------------------------------- | ------------------ | ------------------- |
| `or`                             | 1                  | 2                   |
| `and`                            | 3                  | 4                   |
| `<`, `>`, `<=`, `>=`, `~=`, `==` | 5                  | 6                   |
| `..`                             | 8                  | 7                   |
| `+`                              | 9                  | 10                  |
| `-` (binary)                     | 11                 | 12                  |
| `*`, `/`, `%`                    | 13                 | 14                  |
| `not`, `#`, `-` (unary)          | n/a                | 15                  |
| `^`                              | 18                 | 17                  |

Note how:

- Left-associative operators bind more tightly to the item on the right, as indicated by the higher "power" on the right.
- Right-associative operators bind more tightly to the item on the left, as indicated by the higher "power" on the left.
- Unary operators are right-associative and don't have a binding power on the left, but we still skip the power number that would be in there if they had a left operand.

Given the above, the following example expression can be said to bind as shown:

```
Example input:  1 * 2 + 3 - 4 / 5 ^ -6 > -7 ^ 8
Desired output: (((1 * 2) + (3 - (4 / (5 ^ (-6))))) > (-(7 ^ 8)))

Input with binding powers shown as `[n]` and the non-extant left operand of the unary operator signified with `_`:

  [0]   [13]   [14]   [9]   [10]   [11]   [12]   [13]   [14]   [18]   [17]   []   [15]   [5]   [6]   []   [15]   [18]   [17]   [0]
      1      *      2     +      3      -      4      /      5      ^      _    -      6     >     _    -      7      ^      8
```

Applying the Pratt algorithm to this means proceeding from left to right making recursive calls, tracking the current minimum binding power (which starts at 0). As we examine each operator we consider the left binding power and the right binding power. If the left power is less than the minimum we stop looping and return our current left operand. Otherwise, we produce the right operand by making a recursive call, passing down the right binding power as the new minimum value.

In operation on the example input, the execution looks like this:

1. Call subroutine with minimum power of 0:
1. Next token is a primary (`1`); that becomes our LHS.
1. Peek at next token (`*`) → binding powers are `13` and `14`.
1. As left power (`13`) isn't smaller than current minimum (`0`), recurse to obtain RHS, passing right power (`14`) as minimum:
1. Next token is a primary (`2`) → LHS.
1. Peek at `+` → binding powers `9` and `10`.
1. As left power (`9`) is less than current minimum (`14`), return LHS (`2`).
1. Produce node from LHS (`1`) + operator (`*`) + RHS (`2`); this (`(1 * 2)`) becomes the new LHS.
1. Peek at next token (`+`) → binding powers `9` and `10`.
1. As left power (`9`) isn't smaller than current minimum (`0`), recurse to obtain RHS, passing right power (`10`) as minimum:
1. Next token is a primary (`3`) → LHS.
1. Peek at `-` → binding powers `11` and `12`.
1. As left power (`11`) isn't smaller than current minimum (`10`), recurse to obtain RHS, passing right power (`12`) as minimum:
1. Next token is a primary (`4`) → LHS.
1. Peek at next token (`/`) → binding powers `13` and `14`.
1. As left power (`13`) isn't smaller than current minimum (`12`), recurse to obtain RHS, passing right power (`14`) as minimum:
1. Next token is a primary (`5`) → LHS.
1. Peek at next token (`^`) → binding powers `18` and `17`.
1. As left power (`18`) isn't smaller than current minimum (`14`), recurse to obtain RHS, passing right power (`17`) as minimum:
1. Next token is a unary operator (`-`); recurse to obtain RHS, passing right power (`15`) as minimum:
1. Next token is a primary (`6`) → LHS.
1. Peek at next token (`>`) → binding powers `5` and `6`.
1. As left power (`5`) is less than current minimum (`15`), return LHS (`6`).
1. Produce node from operator (unary `-`) + RHS (`6`); this (`(-6)`) becomes the new LHS.
1. Peek at next token (`>`) → binding powers `5` and `6`.
1. As left power (`5`) is less than current minimum (`15`), return LHS (`(-6)`).
1. Produce node from LHS (`5`) + operator (`^`) + RHS (`(-6)`); this (`(5 ^ (-6))`) becomes the new LHS.
1. Peek at next token (`>`) → binding powers `5` and `6`.
1. As left power (`5`) is less than current minimum (`14`), return LHS (`(5 ^ (-6))`).
1. Produce node from LHS (`4`) + operator (`/`) + RHS (`(5 ^ (-6))`); this (`(4 / (5 ^ (-6))`) becomes the new LHS.
1. Peek at next token (`>`) → binding powers `5` and `6`.
1. As left power (`5`) is less than current minimum (`12`), return LHS (`(4 / (5 ^ (-6))`).
1. Produce node from LHS (`3`) + operator (`-`) + RHS (`(4 / (5 ^ (-6))`); this (`(3 - (4 / (5 ^ (-6))))`) becomes the new LHS.
1. Peek at next token (`>`) → binding powers `5` and `6`.
1. As left power (`5`) is less than current minimum (`10`), return LHS (`(3 - (4 / (5 ^ (-6)))`).
1. Produce node from LHS (`(1 * 2)`) + operator (`+`) + RHS (`(3 + (4 / (5 ^ (-6))))`); this (`((1 * 2) + (3 - (4 / (5 ^ (-6)))))`) becomes the new LHS.
1. Peek at next token (`>`) → binding powers `5` and `6`.
1. As left power (`5`) isn't smaller than current minimum (`0`), recurse to obtain RHS, passing right power (`6`) as minimum:
1. Next token is a unary operator (`-`); recurse to obtain RHS, passing right power (`15`) as minimum:
1. Next token is a primary (`7`) → LHS.
1. Peek at next token (`^`) → binding powers are `18` and `17`.
1. As left power (`18`) isn't smaller than current minmum (`15`), recurse to obtain RHS, passing right power (`17`) as minimum:
1. Next token is a primary (`8`) → LHS.
1. Peek at next token → it's the end of input, so return LHS (`8`).
1. Produce node from LHS (`7`) + operator (`^`) + RHS (`8`); this (`(7 ^ 8)`) becomes the new LHS.
1. Peek at next token → it's the end of input, so return LHS (`(7 ^ 8)`).
1. Produce node from operator (unary `-`) + RHS (`(7 ^ 8)`); this (`(-(7 ^ 8))`) becomes the new LHS.
1. Peek at next token → it's the end of input, so return LHS (`(-(7 ^ 8))`).
1. Produce node from LHS (`((1 * 2) + (3 - (4 / (5 ^ (-6)))))`) + operator (`>`) + RHS (`(-(7 ^ 8))`); this (`(((1 * 2) + (3 - (4 / (5 ^ (-6))))) > (-(7 ^ 8)))`) becomes the new LHS.
1. Peek at next token → it's the end of input, so return LHS (`(((1 * 2) + (3 - (4 / (5 ^ (-6))))) > (-(7 ^ 8)))`).

And, tedious though the demonstration was, the output:

    (((1 * 2) + (3 - (4 / (5 ^ (-6))))) > (-(7 ^ 8)))

matches the desired result:

    (((1 * 2) + (3 - (4 / (5 ^ (-6))))) > (-(7 ^ 8)))
