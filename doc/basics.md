Hyperbole runs at compiletime to inspect an expression, but can present its
output in different ways:
- as a message reported at compiletime
- as a terminal compile error
- as a value which can be used in some way at runtime

It can be invoked on an expression in ordinary code, or in an inline context
where a `Quotes` instance is available, on a lifted `Expr` or a `Tree` value.

In all cases, the macro is invoked with an `introspect` method, either passing
the expression to be inspected, or a lifted `Expr` expression value in a quoted
context, disambiguated by overloading. In a quoted context, an additional
optional parameter, `terminate`, may be specified as `true` to indicate that
the output should be reported as a compiler error rather than an informational
message at compiletime; by default it is `false`.

A contextual `Introspection` value determines what result should be yielded
from a call to `introspect`. This given value can determine not only the result
type, but whether the expression is evaluated or its value retained. Three
implementations offered:

- `introspection.println`—records the introspection details to `stdout` with
  `println`, and returns the expression value
- `introspection.text`—constructs a `Text` value containing the introspection
  details, ignoring the expression without evaluating it

The default is to log using Eucalyptus. Note that this contextual value is not
necessary for the `introspect` method that take an `Expr` value.

For example, in a macro method body,
```scala
import hyperbole.*
def macroImpl(expr: Expr[T])(using Quotes): Expr[Unit] =
  import quotes.reflect.*
  introspect(expr, terminate = true)
```
or, for example, in a [Probably](https://github.com/propensive/probably/) test,
```scala
import hyperbole.*, introspection.log
import probably.*
import eucalyptus.*, logging.stdout

test(t"Two joined lists are not empty"):
  val xs = List(1)
  val ys = List(2)
  introspect(xs ++ ys)
.assert(!_.isEmpty)

```


