# Expressions

```Cinnabar
foo({ bar(); 4}, { bar(); if true { foo(); 4 + { foo(); bar(); } } else { bar(); }})
foo(
  {
    bar()
    4
  },
  if true {
    foo()
    4 + {
      foo()
      bar()
    }
  }
  else {
    bar()
  }
)
```

(Call
  (Block
    (Call)
    (Literal)
  )
  (If
    (Binary
      (Literal)
      (Block
        (Call)
        (Call)
      )
    )
    (Call)
  )
)

```C
int x;

bar();
x = 4;

int y;
if (true) {
  foo();
  int a;

  foo();
  a = bar();
  y = a;
}
else {
  y = bar();
}

foo(x, y);
```


```Cinnabar
var x = {
  var y = 3
  y + 3
  y + {
    var z = 4
    4 + 5
  }
}
```
