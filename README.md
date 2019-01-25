[![Maven Central](https://goo.gl/VtnfEV)](https://goo.gl/NmvGmr)
[![Join the chat on Gitter](https://goo.gl/osTzxH)](https://goo.gl/r4r4yU)

# Adversaria

Adversaria is a tiny library which provides a few tools for working with static
annotations in Scala, by making them available through typeclass interfaces.

The nature of annotations in Scala is that they are very rarely the best
solution for any task. The can, however, be convenient in certain
circumstances, and this small domain is where Adversaria aims to help.

Currently there are three use cases:

- getting all the annotations applied to a particular type
- finding the parameter of a case class to which a particular annotation has
  been applied
- getting all the annotations applied to a particular case class field

We expect the list of supported use cases to grow as additional suggestions are
received.

## Availability

An early release of Adversaria is available on Maven Central, and can be
included in an sbt project by including,
```
"com.propensive" %% "adversaria" % "0.2.0"
```
in your `build.sbt` file.

## Examples

If we were to define the following annotations,
```scala
import scala.annotation.StaticAnnotation

final case class id() extends StaticAnnotation
final case class count(n: Int) extends StaticAnnotation
```

we could apply them to some case classes, such as,
```scala
@count(10)
case class Company(name: String)

case class Person(name: String, @id email: String)
```

Perhaps we would like to find out the annotations on `Company`. We can get this
information by requesting an implicit `TypeMetadata[Company]` value, and
accessing its `annotations` field, like so,

```scala
val info = implicitly[TypeMetadata[Company]]
println(info.annotations)

> List(count(10))
```

The `TypeMetadata` implicit should resolve for any type, regardless of
whether it has any annotations or not.

Another supported use case is to find the field of a case class which has been
annotated with a particular annotation, if and only if that annotation exists.
We use the `FindMetadata` typeclass for this. It takes two type parameters:
the type of the annotation, and the type to check, respectively,

```scala
val idField = implicitly[FindMetadata[id, Person]]
println(idField.get(Person("John Smith", "test@example.com)))

> test@example.com
```
However, attempting to resolve such an implicit on a case class which has no
field annotated with that annotation will fail at compile-time:

```scala
val idField = implicitly[FindMetadata[id, Company]]

! adversaria: could not find a parameter annotated with type @id
```

## Status

This library is very experimental, and provided for evaluation purposes only.
It should not be considered production-ready, and the API is liable to change
without warning.

## License

Adversaria is provided under the Apache 2.0 license.

