# A misspelled Kotlin method, with Kotlin's own spelling suggested

A member which a Kotlin class does not declare is a compile error in Scala. Where the name is
close to one which the class does declare, the error suggests it, with the signature written as
Kotlin writes it.

```scala fuel
compile
frame
  label A method Kotlin's Regex does not have
  errors show
  code
      val number = make[kotlin.text.Regex](t"[0-9]+")

      val numeric: Boolean = number.matchs(t"1599")
frame
  label The one it has
  highlight matches  declared by Kotlin as matches(input: CharSequence): Boolean
  code
      val number = make[kotlin.text.Regex](t"[0-9]+")

      val numeric: Boolean = number.matches(t"1599")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(numeric.inspect)
```

[Kotlin and Java facades](../../soundness/modules/foreign-interop.md#kotlin-and-java-facades)
describes how a facade finds its members.
