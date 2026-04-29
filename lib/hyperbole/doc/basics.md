Hyperbole runs at compiletime to expose the TASTy trees of expressions. It can be
invoked either from a macro, or within user code. In both cases, it produces a
single string showing a tabular representation of the TASTy tree, for displaying
in the console.

## Inside a Macro

Within a macro, where there is a `Quotes` instance available, any `Expr` value can
be introspected by calling its `introspect` extension method.

This will construct an instance of `Teletype`, a kind of string including ANSI
control codes. An ordinary `Text` value can be obtained using `Teletype#plain`,
which can be printed during compilation (as information or as an error), or
even used in the expansion of the macro.

## In Ordinary Code

The TASTy tree for any expression can also be obtained in ordinary code, using the
global `introspect` method.

For example, we can introspect the expression `1 + x`:
```scala
val x = 5
introspect(1 + x)
```
which will expand to the string,
```
TASTy             Param   Source   Code
╶─────────────────────────────────────────────────────╴
▪ Inlined                 1 + x    1.+(rs$line$2.x)
└─▪ Apply                 1 + x    1.+(rs$line$2.x)
  ├─▪ Select      +       1 +      1.+
  │ └─▪ Literal   1       1        1
  └─▪ Ident       x           x    rs$line$2.x
```

We could then compare the AST to a variant such as this:
```scala
val y: 5 = 5
introspect(y + 1)
```
which produces the simpler tree,
```
TASTy         Param   Source   Code
╶─────────────────────────────────────╴
▪ Inlined             y + 1    6
└─▪ Literal   6       y + 1    6
```
thanks to constant folding taking advantage of the singleton type of `y`.
