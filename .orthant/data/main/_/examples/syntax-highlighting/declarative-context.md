# How deeply the compiler reads

The call which highlights source is the same whether the compiler only tokenizes it or
typechecks it as well. The depth is a given: with a compiler and a classpath in scope, one
import turns colored identifiers into identifiers which know their types.

```scala fuel
compile
alternative Tokenized
  code
      val code = Scala.highlight(t"val fools = List(t\"Feste\", t\"Touchstone\")")
alternative Typechecked
  highlight highlighting.typecheckedScala  the whole front end, for this scope
  code
      given scalac: Scalac[3.8, Toolchain.Universe.Classfile] = Scalac[3.8](Nil)
      given classpath: LocalClasspath = LocalClasspath()
      import highlighting.typecheckedScala

      val code = Scala.highlight(t"val fools = List(t\"Feste\", t\"Touchstone\")")
##
```

The example is compiled, and is not run, since the second form starts a compiler.
[On highlighting](../../soundness/modules/syntax-highlighting.md#on-highlighting) lists the
depths, and [declarative context](../../philosophy/declarative-context.md) explains why the
choice is an import.
