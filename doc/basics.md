Annotations in Scala are rarely the best solution for any task, but can
nevertheless be convenient as a "feature of last resort" when no other solution
provides the right ergonomics. This small domain is where Adversaria helps.

Currently three use cases are supported:
- getting all the annotations applied to a particular type
- finding the particular parameter of a case class to which a certain
  annotation has been applied
- getting every annotation applied to a particular case class field

This list of supported use cases is likely to grow.

### Annotations

If we define the following annotations in the standard way (each starting with
a lower-case letter, as is the convention)
```scala
import scala.annotation.StaticAnnotation

final case class id() extends StaticAnnotation
final case class count(n: Int) extends StaticAnnotation
```
we could apply them to some case classes, such as:
```scala
@count(10)
case class Company(name: Text)
case class Person(name: Text, @id email: Text)
```

We would like to write code that can access annotations such as `@count` and
`@id` through a simple typeclass interface.

### Contextual evidence

Elsewhere, we may have a polymorphic method, say `inspect`, which inspects an
instance of a type:
```scala
def inspect[T](value: T): Unit
```

If we would like to get the annotations on `T` that are subtypes of `count`, we
can get these with the typeclass, `Annotations`:
```scala
def inspect[T](value: T)(using anns: Annotations[count, T]): Unit =
  anns.collect:
    case `count`(n: Int) => println(t"count = $n")
```

If `inspect` is called with a type, `T`, that does not have any `@count`
annotations, then no contextual `Annotations[count, T]` instance will be
constructed, and the code will not compile. So `inspect[Company](company)`
would compile, while `inspect[Person](person)` would not.

### Direct inspection

Three methods also provide access to annotations on fields:
- `Annotations.field[T](fn)` will return a list of annotations on the case
  class field indicated by the lambda, `fn`. This lambda must be a simple field
accessor, such as `_.email`, otherwise the method will not compile.
- `Annotations.fields[T, A]` will return a list of `CaseField` instances
  providing access to the name, annotation and value (if given an instance of
`T` to dereference) for each annotation on any field with an annotation of type
`A` in `T`'s definition.
- `Annotations.firstField[T, A]` will return the first such field, if it exists.




