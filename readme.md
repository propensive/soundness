[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/wisteria/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/wisteria/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Wisteria

__Wisteria__ is a generic macro for automatic materialization of typeclasses for datatypes composed from product
types (e.g. case classes) and coproduct types (e.g. enums). It supports recursively-defined datatypes
out-of-the-box, and incurs no significant time-penalty during compilation.

## Features

 - derives typeclasses for case classes, case objects and sealed traits
 - offers a lightweight syntax for writing derivations without needing to understand complex parts of Scala
 - builds upon Scala 3's built-in generic derivation
 - works with recursive and mutually-recursive definitions
 - supports parameterized ADTs (GADTs), including those in recursive types
 - supports typeclasses whose generic type parameter is used in either covariant and contravariant positions


## Availability

Wisteria has not yet been published as a binary, though work is ongoing to fix this.

## Getting Started

Given an ADT such as,
```scala
enum Tree[+T]:
  case class Branch(left: Tree[T], right: Tree[T])
  case class Leaf(value: T)
```
and provided an given instance of `Show[Int]` is in scope, and a Wisteria derivation for the `Show` typeclass
has been provided, we can automatically derive given typeclass instances of `Show[Tree[Int]]` on-demand, like
so,
```scala
Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).show
```
Typeclass authors may provide Wisteria derivations in the typeclass's companion object, but it is easy to create
your own.

The definition of a `Show` typeclass with generic derivation defined with Wisteria might look like this:
```scala
import wisteria.*

trait Show[T]:
  def show(value: T): String

object Show extends Derivation[Show]:
  def join[T](ctx: CaseClass[Show, T]): Show[T] =
    ctx.params.map { p =>
      s"${p.label}=${p.typeclass.show(p.dereference(value))}"
    }.mkString("{", ",", "}")

  override def split[T](ctx: SealedTrait[Show, T]): Show[T] = value =>
    ctx.dispatch(value) { sub => sub.typeclass.show(sub.cast(value))
```

The `Derivation` trait provides a `derived` method which will attempt to construct a corresponding typeclass
instance for the type passed to it. Importing `Show.derived` as defined in the example above will make generic
derivation for `Show` typeclasses available in the scope of the import.

While any object may be used to define a derivation, if you control the typeclass you are deriving for, the
companion object of the typeclass is the obvious choice since it generic derivations for that typeclass will
be automatically available for consideration during contextual search.

### Limitations

Wisteria is not currently able to access default values for case class parameters.


## Related Projects

The following _Scala One_ libraries are dependencies of _Wisteria_:

[![Rudiments](https://github.com/propensive/rudiments/raw/main/doc/images/128x128.png)](https://github.com/propensive/rudiments/) &nbsp;

The following _Scala One_ libraries are dependents of _Wisteria_:

[![Gossamer](https://github.com/propensive/gossamer/raw/main/doc/images/128x128.png)](https://github.com/propensive/gossamer/) &nbsp; [![Probably](https://github.com/propensive/probably/raw/main/doc/images/128x128.png)](https://github.com/propensive/probably/) &nbsp;

## Status

Wisteria is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Wisteria is designed to be _small_. Its entire source code currently consists
of 858 lines of code.

## Building

Wisteria can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Wisteria are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/wisteria/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Wisteria easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Wisteria was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Wisteria is a flowering plant, much like magnolia is, and Wisteria is a derivative of Magnolia.

## License

Wisteria is copyright &copy; 2017-23 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
