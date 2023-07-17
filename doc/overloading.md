# Overloading

Will not use Hindley-Milner type system (too hard). All expressions will have
their type known at the call site.

Would like to at least have overloading by arity. Single constructor function vs
multiple "factory" functions.

```Cinnabar
fun foo(x T, y T)     // 1
fun foo(x i32, y i32) // 2



```

```Cinnabar
/*
 * Types A B C
 * Traits AB BC CA ABC
 */

fun foo(x ABC, y ABC) // 1
fun foo(x AB, y AB)   // 2
fun foo(x AB, y B)    // 3
fun foo(x AB, y A)    // 4
fun foo(x A, y AB)    // 5

foo(C, C) // 1
foo(B, B) // 3
foo(A, A) // 4 or 5? ambiguous overload error, how to specify?
// exponential time backtracking...
```

Force user to resolve ambiguity?

```Cinnabar
fun foo(x T, y i32)
fun foo(x i32, y T)
foo(i32, i32) // ambiguous overload error
foo(i32 as any, i32)
```
