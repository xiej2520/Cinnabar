# Visibility and Scope

When a function, struct, or enum is defined in a scope, it can be used anywhere
in the scope, even before its definition (hoisted to the beginning). Variables
can only be used after their declaration, except for global variables.

```Cinnabar
fun main() {
  let x = g + 2
  let s = S()
  let t = T()

  struct T {
    j i32
  }
}

struct S {
  i i32
}

let g = 0;
```

Functions, structs, enums, and blocks (therefore if, while, for) create a scope.

```Cinnabar

if true {
  enum E {
    
  }
  let e: E
}
else {
  let e1: E // error
}
```

Variables, functions, and types all belong to the same namespace: they cannot
have the same name as an existing one, except for function overloading.
Allow shadowing variable in same block like rust?

```Cinnabar
let s: S; // not allowed
fun s() {
  
}
struct s { // not allowed
  
}
```

Names can be shadowed in a new block.

```Cinnabar
fun main() {
  let x;
  {
    let x;
  }
}
```

Structs, enums, and functions define their name both in the scope that they are
declared, and the scope that they contain.

```Cinnabar
// fun foo (1) defined here
foo()
fun foo() { // (1)
  fun foo() { } // error, foo has same name as foo (1)
  fun foo(i i32) { } // (2) allowed, overloads foo (1)
}

struct S {
  // struct S defined here
  struct S { } // error, S has same name
  struct T {
    struct S { } // allowed...
  }
}
```

Java and C++ have nested struct/class

```Java
class Main {
  public static void main(String args[]) {
    X.Y y;
  }
  class X {
    class Y { }
  }
}
```
