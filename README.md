# Transmogrify

Parsing experiments in rust

*This is not intended for production use*

## What makes a good parser?
- Supports arbitrary (left and right, direct and indirect) recursion
- Error reporting that clearly identifies the specific problem in the input
- Lexer built in - we don't want to maintain a separate grammar for the lexer
- Can attach actions to build an AST, or directly execute the input

## Status
- Arbitrary recursion a la [Left recursion in Parsing Expression Grammars](https://doi.org/10.1016/j.scico.2014.01.013)
- Rich error messages by tracking expected tokens at each failed terminal
- Arbitrary token skipping rules, and a Lexer rule to disable skipping
- Limited action support: actions can transform the input, but with limited output formats and no side-effects

## TODO
- Can we stop doing everything via Boxed dyn Traits?
  - Requires figuring out heterogenous lists (in the style of frunk?)
- Can we support actions that build a tree of user-defined types?
- Can we support actions that have side-effects?
