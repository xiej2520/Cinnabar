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

In parser and AST:

```Cinnabar
struct HashMap[Key, Value] { }
let m = HashMap[i32, bool]
```

Base type is named `HashMap`, gets an entry in the type table.
Parameterized `m` is `HashMap[i32, bool]`, also gets an entry in the type table.
Use canonical name `HashMap[i32, bool]` for lookup, mangled name `HashMap_i32_bool_ABC`
for backend?

[https://doc.rust-lang.org/reference/expressions.html](Rust Expressions)

* Place expressions: represents memory location
  * Left of assignment
  * Operand of borrow, address of, dereference
  * Operand of field expr
  * Operand of array indexing
  * Initializer of let
* Assignee expression: left operand of assignment expression
  * Place expression
  * Underscore
  * Tuple of assignee expression
  * Slice of assignee expression
  * Tuple structs, structs of assignee expression
  * Unit struct

## Generics

* Only support type generics for now! Too much effort for const generics/monomorphizing
code with const generics might be horrible.
* Generics only at top-level?

```Rust
fn f<R>() {
    struct S<T> { // can't use generic parameters from outer function
        t: T,
        r: R,
    }
}
```

```C++

template<class R>
void f() {
    R r;
    template<class T>
    struct S { // a template declaration cannot appear at block scope
        T t;
    };
}
```

```C++
template<class T>
struct S {
    T t;
    template<class R>
    void f() {
        R r;
        T t;
    };
};
```