## Soundness

Soundness is an ecosystem of Scala libraries that constrains your programs to _correctness_,
liberating you from the pain of runtime errors, without compromising the legibility of your code.

Now you can write code that feels just as intuitive as JavaScript or Python, but with the safety of
Haskell or Rust.

Soundness provides over 100 comprehensive libraries for building web and CLI applications, testing,
working with various data formats, scientific calculations and developing tooling, all designed with
a common philosophy of clarity and correctness.

### Scala 3

Soundness was developed natively for Scala 3, and exploits its extensive new features to the
maximum. Its sound typesystem and powerful metaprogramming capabilities make it uniquely capable of
defining APIs which take full advantage of precise types without compromising the legibility of your
code.

Soundness is evolved from, but unencumbered by the legacy of Scala 2. No interface has been
compromised in its expressiveness for compatibility with Scala 2. Adopting Soundness means a
full-throated endorsement of the future of Scala.

#### Two Golden Principles

At the foundation of Soundness are two core principles: Impossible states should be unrepresentable,
and transitions between states should be total. Together, these ensure that your program starts in a
correct state and never leaves it. That means no unexpected runtime errors.

Every library in the Soundness ecosystem is built on these principles, and coerces your code along a
path of correctness.

#### Dependent Typing

Soundness follows the principle that any static analysis the programmer can do, the compiler can do
better. And any static analysis the compiler _can_ do, it _should_ do. So Soundness makes the the
Scala compiler do _more_ at compiletime so you don't have to, and so the runtime doesn't have to.
Scala's powerful type system makes it possible to track more invariants about values at compiletime,
and to use that information to your advantage.

Why write the code to handle a failure, if the source code has everything to show it's impossible?
With Soundness, you don't have to.

#### Safe Exceptions



#### Maintenance/Continuity


#### Null safety

The curse of `null`, the _billion-dollar mistake_ is well known by anyone who has ever encountered
a `NullPointerException`. In Soundness, `null` is unrepresentable and unnecessary, and Scala's type
system enforces it. You won't see a `null` in Soundness code, and`NullPointerException`s rarely ever
occur, so you can code without concern for them.

#### Compositionality

Monads are the foundation of functional programming, but they don't compose. Lifting every
value into a monadic wrapper type is a burden that's familiar, but can never compose as easily as
expressions.

#### Typeclass-driven Modularity

The object-oriented paradigm of using inheritance to provide common functionality between different
types is flawed. Soundness provides a wealth of typeclasses for defining common behavior, and
a smorgasbord of instances for working with its various types. With datatypes and behavior specified
independently, each Soundness library is modular and composable.

Generic derivation provides typeclasses over product and sum types.

Everything just works with everything else—or it's just a typeclass instance away.

#### Safe Literals

Soundness makes it impossible to represent impossible states, so values are not
only known to be safe by construction, but checked at compiletime. When there's
not enough information available to check a value at compiletime, it's checked
at runtime—but you must define what happens if it's invalid.

Values like URLs, timestamps and ports are all eagerly verified, and they're
represented as literals. If they're not valid, it's a compile error.
Stringly-typed values—parsed at runtime—are a thing of the past.

#### Elegant Prose

Good code reads like elegant prose.

Just as functional programming introduced new nomenclature for the most common
code structures, Soundness has its own. But it's borrowed from Enlish, so you
get a headstart understanding it. Familiar words make familiar appearances in
Soundness code, and each has been chosen with care for its clarity, uniqueness,
nuance and expressiveness.

#### Declarative Programming

Declarative code is easier to reason about because it's independent of control flow. Its essential
structure arises from scopes and contexts, and in Scala 3 it's possible with _contextual values_.
Declare a new `given` instance or import an existing one, and it's valid for the entire scope. And
you decide whether that's just a method body, or your entire project—on a continuuum between local
and global. There's less repetition, and your code is more maintainable.

#### Decoupled Integration

Soundness is a vast ecosystem of small libraries, for many diverse applications. For specific needs,
individual libraries can be selected à la carte, without introducing a complex graph of
dependencies. Care has been taken to decouple libraries which aren't directly related, without
compromising their integration. Soundness's typeclass-based approach has made it possible to avoid
unnecessary dependencies through the careful specification of small interfaces.

But Soundness also provides six bundles of libraries targetting different domains. These are `web`
for web applications; `data` for data processing; `sci` for scientific applications; `cli` for
command-line applications; `test` for testing; and, `tool` for tooling development. The `base`
bundle provides a common set of fundamental libraries, and `all` includes everything.

#### Polymorphic and Small

For all the functionality it provides, Soundness's API is _tiny_. Instead of a multitude of similar
methods and types, Soundness prefers fewer methods and fewer types, but each more versatile and
composable. Types can be composed in a variety of ways, with the resultant type exhibiting
the properties of its constituent parts in predictable ways, without introducing new types with new
names—and potentially new properties. This is the elegance of modularity.
