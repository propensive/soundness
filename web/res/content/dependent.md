### Dependent Typing

Soundness takes the idea that any static analysis a programmer can do, the compiler can do better. And any static analysis the compiler _can_ do, it _should_ do. So Soundness makes the the Scala compiler do more at compiletime so you don't have to, and so the runtime doesn't have to. Scala's powerful type system makes it possible to track more invariants about values at compiletime, and to use that information to your advantage.

Why write the code to handle a failure, if the source code has everything to show it's impossible? With Soundness, you don't have to.
