# Design

Stages

* Lexer
  * Break up text source into tokens, add semicolons where they make sense.
* Parser
  * Parse into stage 1 AST
  * Struct, Enum, Function, control flow nodes
  * Create symbol table for each namespace
* Resolver/Typechecker
  * Resolve all symbols
  * Create new typed AST with types for each expression/node
  * Toplevel: find all struct/enum definitions, recursively add nested definitions?
    * `struct X { struct Y }` would have `X` and `X.Y` visible at toplevel.
    * Process all type definitions at each "level" (nested struct but not block),
    then dfs?
* Backend

C Backend
  * Topological sort of types and functions