# An atomic number which may have no element

The periodic table has 118 elements, and a number looked up in it may be none of them. The
result is an `Optional` element, which cannot be used as an element until the absent case has
been given a meaning.

```scala fuel
compile
frame
  label An element which may not exist, used as one
  errors show
  code
      def name(number: Int): Text = PeriodicTable(number).name
frame
  label A name for the absent case
  highlight or(t"undiscovered")  what absence means here
  code
      def name(number: Int): Text =
        PeriodicTable(number).let(_.name).or(t"undiscovered")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(name(26))
Out.println(name(119))
```

[Elements](../../soundness/modules/chemistry.md#elements) has the other ways to reach an
element, and [optionality](../../philosophy/optionality.md) explains `let` and `or`.
