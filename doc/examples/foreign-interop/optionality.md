# A Kotlin method which may return null

Kotlin marks a result which may be `null` in its type, and a Kotlin compiler records that in the
classfile. A facade reads it: `Regex.find` returns a `MatchResult?`, so here it returns an
`Optional`, which cannot be used as though a match were certain.

```scala fuel
compile
frame
  label A match is taken for granted
  errors show
  code
      val number = make[kotlin.text.Regex](t"[0-9]+")

      val act: Text = number.find(t"Act 3, Scene 1").value
frame
  label No match is given a meaning
  highlight let(_.value)  the facade over a MatchResult, where there is one
  highlight or(t"none")  what Kotlin's null becomes here
  code
      val number = make[kotlin.text.Regex](t"[0-9]+")

      val act: Text = number.find(t"Act 3, Scene 1").let(_.value).or(t"none")
      val prologue: Text = number.find(t"Prologue").let(_.value).or(t"none")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(act)
Out.println(prologue)
```

[Kotlin and Java facades](../../soundness/modules/foreign-interop.md#kotlin-and-java-facades)
describes what a facade reads, and [optionality](../../philosophy/optionality.md) the type
which stands for Kotlin's `?`.
