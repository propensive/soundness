# A Scala lambda, called by a Kotlin method

Kotlin's `Regex.replace` takes a function from a `MatchResult` to a replacement. A Scala lambda
serves, with no adapter: its parameter is a facade over the Kotlin type, so the match is
navigated inside the lambda as it would be outside, and the result is `Text`.

```scala fuel
compile
frame
  label A Kotlin regular expression
  code
      val number = make[kotlin.text.Regex](t"[0-9]+")
frame
  label A replacement computed by Scala
  highlight Facade..MatchResult  the lambda's parameter is Kotlin's own type
  highlight scene.value  a Kotlin property, read as Text
  code
      val number = make[kotlin.text.Regex](t"[0-9]+")

      val cue: Text = number.replace
        ( t"Act 3, Scene 1",
          (scene: Facade over kotlin.text.MatchResult) => t"<${scene.value}>" )
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(cue)
```

[Kotlin and Java facades](../../soundness/modules/foreign-interop.md#kotlin-and-java-facades)
describes function-typed parameters, and [composability](../../philosophy/composability.md)
what it takes for two libraries to meet without glue.
