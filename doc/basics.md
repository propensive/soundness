## Quotes and Splices

A Scala 3 macro, written using quotes and splices syntax, typically looks something like this:
```scala
import scala.quoted.*

def say(user: String)(using Quotes): Expr[Unit] =
  val name: Expr[String] = Expr(user)
  '{Out.println("Hello "+${name})}
```

The usage of `'{...}` (quotes) and `${...}` (splices) are indicative of _phase shifts_.
Code inside quotes will be executed one phase later than the surrounding code, and code
inside a splice will be executed one phase earlier than the code surrounding it. In the
example above, the definition of `name` and the usage of `name` occur in the same phase
(and *must* occur in the same phase, due to the _Phase Consistency Principle_),
while `Out.println` and `"Hello "` are in the next phase. An instance of `Expr[String]`
or `Expr[Unit]` is an abstract notion of an expression that will become a `String` or a
`Unit` in the next phase.

For a macro, that "next phase" will be a later compilation, when a new instance of the
compiler is run, and all code from prior phases exists as compiled bytecode, and can be
run.

A similar effect could be achieved just by writing the code in a separate file, and
compiling it later, but the clever part is that quotes and splices can be interleaved
at a fine-grained level; as expressions. And furthermore, those expressions are
typechecked.

But quotes and splices and the concept of phases can be applied more generally than in
plain macros. The "next phase" does not have to be "the next compilation"; a quoted
block can represent any code which runs "elsewhere". There are a world of
possibilities for where "elsewhere" could be, but it could be inside another JVM,
on a cloud-based system, or inside a browser using ScalaJS.

_Superlunary_ provides the wiring to make it easy to exploit the powerful syntax and
consistency-checking of quoted code, to make it possible to deploy the code inside
quotes to an environment of your choosing.

