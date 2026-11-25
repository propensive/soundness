# Text, by way of a locale

A value which is read from a `Polyglot` depends on a locale for the languages it has. The
type `Text via (en & fr)` says so: text, by way of a locale which is English or French.

```scala fuel
compile
frame
  label A locale, as a parameter
  highlight (using..fr])  the locale it depends on
  code
      val greeting: Polyglot[Text, en & fr] = en(t"Hello") | fr(t"Bonjour")

      def welcome(using Locale[en & fr]): Text = greeting()
frame
  label The same, in the result type
  highlight via..fr)  read as a phrase
  code
      val greeting: Polyglot[Text, en & fr] = en(t"Hello") | fr(t"Bonjour")

      def welcome: Text via (en & fr) = greeting()
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(welcome(using Locale(fr)))
Out.println(welcome(using Locale(en)))
```

[Selecting a language](../../soundness/modules/internationalization.md#selecting-a-language)
describes locales, and [infix types](../../philosophy/infix-types.md) the words such types are
made of.
