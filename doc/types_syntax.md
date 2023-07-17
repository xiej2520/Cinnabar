# Types

A base typename is a valid identifier. Preferably PascalCase.

```Cinnabar
struct HashMap { }
enum UnaryOperator { }
```

A generic type is parameterized by angle brackets surrounding a possibly comma
separated list of generic parameters. The generic parameters can be types or
constant values.

```Cinnabar
struct Array[T, size u32] { } // type T, size
struct Array[T type, size u32]
```

Use `:` or `as` for type casts?

```Cinnabar
let i: i32 = 4
let i as i32 = 4

fun foo(i i32, j f32) // definition
foo(i as i32, j as f32) // call
foo(i: i32, j: f32)     // call
```