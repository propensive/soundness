It is not possible to write code such as,
```
def increment[MonadicType[_]](xs: MonadicType[Int]) =
  for(x <- xs) yield x + 1
```
because the compiler is not able to express the constraint that the type
constructor `F[_]` provides the methods `map` and `flatMap` (with the correct
signatures), which are necessary for the for-comprehension to compile.

With _Mercator_, it is possible to demand a contextual instance of `Monad[MonadicType]` to
enforce this constraint. Mercator will automatically instantiate such an
instance at the use-site for any type which has the required methods, like so,
```
import mercator._
def increment[F[_]: Monad](xs: F[Int]) = for(x <- xs) yield x + 1
```

The methods `flatMap` and `map` will be provided to the instance of `F[_]` as
extension methods, which are then used by the for-comprehension.

## Point

An instance of `Monad[F]` will generate an implementation of `point` (sometimes
called "unit", though not to be confused with `Unit`) which
constructs a new instance of the type from a single value. For example, `point(x)` for
`Option` is `Some(x)`, or for `Either` it is `Right(x)`. This implementation
assumes the existence of a unique `apply` method on the type's companion object, and
that applying the value to it will produce a result of the correct type.

A `Point[MonadicType]` instance can always be provided to explicitly specify the
`point` instance for the given type constructor.

## Functors

Mercator also provides a `Functor` typeclass, which provides implementations of just
`point` and `map`. If `map` is required for a particular operation, but `flatMap` is not,
then only the `Functor` typeclass should be summoned.

