### Using Symbolic Operators

A project which defines symbolic operator implementations using Symbolism will
need Symbolism's extension methods in scope. This is simply a matter of
importing it, like so:
```scala
import symbolism.*
```

Subsequently, any object which does not define its own `+`, `-`, `*`, `/`, `<`,
`>`, `<=` or `>=` method will use Symbolism's definition, and search for an
appropriate contextual instance for the implementation, based on the types of
the left and right operands.

For example, calling `a + b` will search for an instance of
`Operator["+", a.type, b.type]`, say `plus`, and invoke its `apply` method on
the parameters, `a` and `b`. The result of `plus(a, b)` will be `plus.Result`,
whatever that happens to be for the particular choice of typeclass instance.

In many cases, the types `a.type`, `b.type` and `plus.Result` would all be the
same, as they are for adding two `Int`s or joining two stringlike objects, but
there is flexibility for the left and right operand types to be different, and
for the result type to be different again.

In fact, the typeclass definitions allow for implementations to be provided
with metaprogramming, which allows the result type to be computed as a function
of the left and right operand types.

### Implementing Symbolism's Typeclasses

Every binary operator is implemented with the `Operator` typeclass, or its
simplified subtype, `ClosedOperator` (for the cases where the left, right and
result types are the same). Both are parameterized on the singleton string
type of the operator name, and can be used to represent any binary operator,
though extension methods are (for now) only provided for `+`, `-`, `*` and `/`.

Any implementation of `Operator` should define the two operand types and a
`Result` type member, which will be the type of the result of the binary
operation, as well as the implementation of the operator, as its two-parameter
`apply` method.

The `apply` method, and its parameters, are inlined, so as to minimize the
performance cost of deferring simple operations to a typeclass, and any
implementation of `Operator` should do the same.



