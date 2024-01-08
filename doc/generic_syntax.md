# Bikeshedding on generic syntax

* `<>` Angle brackets
  * Pros
    * **Familiarity to C++, Java, C#**
    * Easy to distinguish??
  * Cons
    * Parsing nightmare: `>>` maximal munch, unlimited lookahead, `v.a<s,t>` and
    other ambiguity
* `::<>`, `.<>`, `:<>` turbofish
  * Pros
    * Somewhat familiar, still angle brackets
    * No parsing ambiguity
  * Cons
    * Ugly
    * 1/2 more characters
* `[]` square brackets, deprecate use on indexing
  * Pros
    * Unambiguous
    * No parsing ambiguity
  * Cons
    * Unfamiliar
    * No array indexing (unless special grammar allows it?)
  * May cause issues if array type is still specified with brackets
    * `Vec[int][4]` array of 4 Vecs of int?
* `!` `!()`, `![]`, `\()`, `\[]` unused single leading char
  * Pros
    * Unambiguous
    * Allows parentheses/brackets use for other purposes.
  * Cons
    * Unfamiliar
    * More typing for deeply nested types, ugly as turbofish?
  * `Vec!int`, `Map!(Vec!int, Map!(int, int))`
  * `Vec\int`, `Map\[Vec\int, Map\[int, int]]`


* For now, use `[]` for generics and indexing - resolve in name resolution.

```Cinnabar
fun f[T, R](T container, R elem) {
  #[let x = fib(35)]
  let i = if container.contains(elem) { 25 } else { 15 }
}
```

## Reference and dereference

* Postfix dereference is easier to follow and needs less parentheses.
* Annoying to parse if using `*`, same as infix multiply - needs backtracking in
parser?

* Use `@` for now? `^` has same problem.
* Zig uses `.*`, makes sense but two characters and `*` isn't used like this elsewhere.

```Cinnabar
let a = ptr@@.field
```
