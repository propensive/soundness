### Decoupled Integration

Soundness is a vast ecosystem of small libraries, for many diverse applications. For specific needs, individual libraries can be selected à la carte, without introducing a complex graph of dependencies. Care has been taken to decouple libraries which aren't directly related, without compromising their integration. Soundness's typeclass-based approach has made it possible to avoid unnecessary dependencies through the careful specification of small interfaces.

But Soundness also provides six bundles of libraries targetting different domains. These are web for web applications; data for data processing; sci for scientific applications; cli for command-line applications; test for testing; and, tool for tooling development. The base bundle provides a common set of fundamental libraries, and all includes everything.
### Compositionality

Monads are the foundation of functional programming, but they don't compose. Lifting every value into a monadic wrapper type is a burden that's familiar, but can never compose as easily as expressions.

Soundness APIs are _direct-style_ APIs. They are designed to compose. And they're ready for a whole new level of safety with Scala's advanced _capture checking_ functionality.
### Declarative Programming

Declarative code is easier to reason about because it's independent of control flow. Its essential structure arises from scopes and contexts, and in Scala 3 it's possible with _contextual_ values. Declare a new given instance or import an existing one, and it's valid for the entire scope. And you decide whether that's just a method body, or your entire project—on a continuuum between local and global. There's less repetition, and your code is more maintainable.
### Next Generation Scala

Soundness was developed natively for Scala 3, and exploits its extensive new features to the limit. Its sound typesystem and powerful metaprogramming capabilities make it uniquely able to accommodate APIs that take full advantage of precise types without compromising your code's legibility.

Soundness is evolved from the experience of Scala 2, but not held back by its legacy. No interface has been compromised in its expressiveness for compatibility with Scala 2. Adopting Soundness means a full-throated endorsement of the future of Scala.
