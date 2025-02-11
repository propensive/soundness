All terms and types are defined in the `typonym` package:
```scala
import typonym.*
```

### Application

Scala's rich type system may be used as a means of carrying static information
from one point in the code to another, so that it may be used for static
analysis and to influence typechecking. Much as a _runtime value_ may be passed
as a parameter to a method, and influence that method's _runtime behavior_, a
_compiletime type_ may be passed as a type parameter, and affect how that
method is typechecked.

The runtime behavior of a method is, assuredly, implemented with ordinary
calling and branching logic, operating on runtime values and runs, of course,
at runtime. And we write almost all code this way because it's easier.

In contrast, typechecking behavior (ignoring, for a moment, its relationship
with the runtime behavior it constrains) may be implemented using type-level
constructs that operate on types, and are not so much _run_ as _applied_ at
_compiletime_. This is a harder way of implementing logic: type-level
operations are less powerful and less expressive than those available at
runtime. So it is not ideal.

However, macros offer the ability to define type-level behavior using the
power, expressivity and simplicity of value-level code, which will be _run_ at
_compiletime_.

Such code often benefits from collections, such as lists, sets and maps, which
presents a need to represent such types in the type system, and to convert
between types (both static types and value-level reflections of those types in
macros) and standard Scala collections of values.

### Type-level Representations

Singleton types of `Int`s, `String`s, `Double`s and `Boolean`s have a
straightforward isomorphism from their value-level to their type-level
representations.

A Scala `List` will be encoded as a `Tuple` parameter to the `TypeList` type
constuctor. For example, the list, `List("one", "two", "three")` would be
represented as, `TypeList[("one", "two", "three")]` (by trivially composing it
with the type-level representation of `String`s).

### Reification

Typonym offers two overloaded variants of the `reify` method for transforming
types into values, to be invoked with a static type; or with an instance of
`Type`, from within a macro implementation.

For example, to convert the `List` example above to a value, we could call,
```scala
val list: List[String] = reify[TypeList[("one", "two", "three")]]
```
or in a more general context,
```scala
inline def printElements[ItemsType <: TypeList[?]]: Unit =
  reify[ItemsType].foreach(println)
```

Similarly, within a macro, 
```scala

def printLongest[ItemsType <: TypeList[?]: Type](using Quotes): Expr[Unit] =
  val items: List[String] = reify(Type.of[ItemsType])
  println(items.maxBy(_.length))
  '{()}
```
