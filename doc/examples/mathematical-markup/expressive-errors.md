# A value which is not mathematics

Only a value which has a mathematical form may be substituted into an Ergo expression. For
any other, the compiler says what the value is and what it lacks, at the substitution.

```scala fuel
compile
frame
  label A play, in a formula
  errors show
  code
      case class Play(title: Text, acts: Int)

      val hamlet = Play(t"Hamlet", 5)
      val length = ergo"(n = $hamlet)"
frame
  label Its number of acts
  highlight ${hamlet.acts}  an `Int` is a number of the formula
  code
      case class Play(title: Text, acts: Int)

      val hamlet = Play(t"Hamlet", 5)
      val length = ergo"(n = ${hamlet.acts})"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(length.in[Xml].show)
```

[Ergo](../../soundness/modules/mathematical-markup.md#ergo) describes the notation, and
[expressive errors](../../philosophy/expressive-errors.md) what an error should say.
