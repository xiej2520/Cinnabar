# Cinnabar

Attempt to make a smaller (and simpler?) C++ and Rust inspired language.

## Progress

* Lexer complete
* Parser partially complete
* Rudimentary Typechecker partially complete
* C codegen partially complete
* Currently working features
  * Enum and Struct types and variables
  * Functions, return values, recursion
  * Unary and Binary operations and assignment
  * Block and If expressions
  * References and dereferencing
* Partial implementation (in various stages of functionality)
  * Immutable references
  * Generic parsing and monomorphization

## Todo

* Basic generic functions
* Basic generic structs
* Fix `__print` and `__println` to use string lengths
* Use place expressions for codegen
* Methods and struct/enum functions
* Extern C

## Feature Goals

* LLVM codegen
* Fast to compile with good performance
* Move semantics
* Pattern matching and sum types
* Generics/Traits
* RAII
* Function/Generic/Operator overloading
