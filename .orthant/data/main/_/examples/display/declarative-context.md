# How a boolean is written

"Yes", "on" and "true" each suit a different context, so a `Boolean` has no form of its own.
An import says which words the code in its scope uses, and without one a boolean is not shown
at all.

```scala fuel
compile
frame
  label No choice has been made
  errors show
  code
      def report(married: Boolean): Text = t"Beatrice is married: $married"
frame
  label Yes and no
  highlight yesNoAffirmation  for every boolean in scope
  code
      import affirmations.yesNoAffirmation

      def report(married: Boolean): Text = t"Beatrice is married: $married"
frame
  label A pair of the program's own
  highlight Affirmation(t"aye",..t"nay")  any two words
  code
      given affirmation: Affirmation = Affirmation(t"aye", t"nay")

      def report(married: Boolean): Text = t"Beatrice is married: $married"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(report(false))
```

[Booleans](../../soundness/modules/display.md#booleans) lists the affirmations which are
provided, and [declarative context](../../philosophy/declarative-context.md) explains why the
choice is an import.
