# A Kotlin `val` stays a `val`

A facade reads Kotlin's properties as members, and knows which of them Kotlin declared with
`val`. `Pair` is immutable in Kotlin, so an assignment to its `first` does not compile in Scala
either, though nothing about the JVM class would stop a setter being looked for.

```scala fuel
compile
frame
  label An assignment to a property Kotlin declared with val
  errors show
  code
      val twins = make[kotlin.Pair[Text, Text]](t"Viola", t"Sebastian")

      twins.first = t"Cesario"
frame
  label A new pair
  highlight twins.second  read through the getter Kotlin generated
  code
      val twins = make[kotlin.Pair[Text, Text]](t"Viola", t"Sebastian")

      val disguised = make[kotlin.Pair[Text, Text]](t"Cesario", twins.second)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(twins.first)
Out.println(disguised.first)
```

[Kotlin and Java facades](../../soundness/modules/foreign-interop.md#kotlin-and-java-facades)
describes properties and their setters.
