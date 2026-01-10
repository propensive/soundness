### Compositionality

Monads are the foundation of functional programming, but they don't compose. Lifting every value into a monadic wrapper type is a burden that's familiar, but can never compose as easily as expressions.

Soundness APIs are _direct-style_ APIs. They are designed to compose. And they're ready for a whole new level of safety with Scala's advanced _capture checking_ functionality.
