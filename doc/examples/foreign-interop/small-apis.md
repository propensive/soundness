# Three ways into a Kotlin type

A Kotlin class is reached through its constructor, its companion object, or, if it is an
object, itself. Each is one method which takes the type as a parameter, and everything after
that is ordinary member selection, checked against what Kotlin declared.

```scala fuel
compile
frame
  label A new instance
  highlight make  the constructor, with its arguments checked
  code
      val number = make[kotlin.text.Regex](t"[0-9]+")
      val numeric: Boolean = number.matches(t"1599")
frame
  label The companion object
  highlight companion  Regex.escape, as Kotlin would write it
  code
      val number = make[kotlin.text.Regex](t"[0-9]+")
      val numeric: Boolean = number.matches(t"1599")

      val literal: Text = companion[kotlin.text.Regex].escape(t"Act 1.")
frame
  label An object
  highlight singleton  Typography is a Kotlin object
  code
      val number = make[kotlin.text.Regex](t"[0-9]+")
      val numeric: Boolean = number.matches(t"1599")

      val literal: Text = companion[kotlin.text.Regex].escape(t"Act 1.")
      val ellipsis: Char = singleton[kotlin.text.Typography].ellipsis
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(numeric.inspect)
Out.println(literal)
Out.println(ellipsis.show)
```

[Kotlin and Java facades](../../soundness/modules/foreign-interop.md#kotlin-and-java-facades)
has the table of the three, and [small APIs](../../philosophy/small-apis.md) the reason there
are so few.
