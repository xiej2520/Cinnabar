# Grammar

* https://craftinginterpreters.com/appendix-i.html
* https://doc.rust-lang.org/reference/statements-and-expressions.html

```BNF
program = declaration* EOF ;

declaration = structDecl
            | enumDecl
            | funDecl
            | varDecl
            | ";" ;

IdentType = IDENT IDENT ;

structDecl = "struct" IDENT
             "{"
             IdentType*
             funDecl*
             "}" ;

enumDecl = "enum" IDENT
           "{"
           IdentType*
           funDecl*
           "}" ;

funDecl = "fun" IDENT
           "("
           (IdentType)?
           ("," IdentType)*
           ")"
           block ;

varDecl = "let" | "var"
          IDENT IDENT?
          ("=" expression)?
          ";"
          ;

statement = exprStmt
          | declaration
          | forStmt
          | whileStmt
          | returnStmt
          | breakStmt
          | contStmt
          | ";"
          ;

exprStmt = expression ";"
         | assignStmt ";" ;

assignStmt = expression "=" expression ;

forStmt = "for" IDENT "in" expression block

whileStmt = "while" expression block

returnStmt = "return" expression? ";" ;

expression = literal
           | binaryExpr
           | unaryExpr
           | groupingExpr
           | callExpr
           | fieldExpr
           | ifExpr
           | matchExpr
           | blockExpr
           | indexExpr
           | rangeExpr
           ;


binaryExpr = expression binOp expression;
unaryExpr = (unaryOp expression) | (expression unaryOp) ;
groupingExpr = "(" expression ")" ;
callExpr = expression "(" IDENT ( "," IDENT )* ")" ;
fieldExpr = expression "." IDENT ;

ifExpr = "if" expression block
         ( "else if" expression block )*
         ( "else" block )? ;

matchExpr = "match" expression
            "{"
            (expression block)*
            "}"

blockExpr = "{"
            declaration*
            "}" ;

indexExpr = expression "[" expression  ( "," expression )* "]" ;
```

Generics will get parsed the same, will be converted during type checking.
E.g. vec[3+4] and Vector[i32] will both get parsed into indexExpr.
Allows for multidimensional slicing in the future?

Cinnabar is mostly semicolon free. Semicolons will get added at newlines where
they make sense, and before the end of a braced block.

```Cinnabar
let x = if true { 3 }
  else { 4 }
```

should become

```Cinnabar
let x = if true { 3; }
  else { 4; };
```

The lexer is responsible for adding semicolons. Semicolons belong at the end of
every expression statement, including braced blocks.

Else is special: `else` is expected to come after a braced block. The parser
expects `else` immediately after an `if`'s block, without an intervening semicolon,
so the lexer has to chew up all the semicolons that it added.

```Cinnabar
let x = if true {
  4
}


else {
  5
}
```

becomes

```Cinnabar
let x = if true {
  4;
}
else {
  5;
};
```


Use `\(param type...)` for lambdas.

```Cinnabar
fun main() {
  let gcd = \(a i32 b i32) {
    return if b == 0 { a } else { gcd(b, a % b) }
  }
}
```
