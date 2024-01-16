# Design

Stages

* Lexer
  * Break up text source into tokens, add semicolons where they make sense.
* Parser
  * Parse into stage 1 AST
  * Struct, Enum, Function, control flow nodes
  * Create symbol table for each namespace
  * Generic arguments are ambiguous with indexing operation - fix in name
  resolution
* Name Resolver/Typechecker
  * Current have name resolution and type checking in same pass - almost
  definitely need to separate later.
  * Resolve all symbols
  * Create new typed AST with types for each expression/node
  * Toplevel: find all struct/enum definitions, recursively add nested definitions?
    * `struct X { struct Y }` would have `X` and `X.Y` visible at toplevel.
    * Process all type definitions at each "level" (nested struct but not block),
    then dfs?
  * Don't generate structs, enums, functions until needed: read definition, which
  can be generic, instantiate concrete version when used.
* Backend

C Backend
  * Topological sort of types and functions

Types are:
* Unit, `()`
* Primitive, i.e. basic integer types, bool
* Functions
* Reference/Pointer
* Array
  * Unsized array (Rust slice `[T]`) included
* Span - fat pointer `&[T]` - isn't really necessary as part of AST?
* Generic - generic type parameter - not necessary in TAST?
* Path - to user defined type
  * Adt: algebraic data type, structs and enums

* Use isize by default?
