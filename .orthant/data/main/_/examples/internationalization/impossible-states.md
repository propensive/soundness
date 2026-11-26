# A locale the text was not written for

A `Locale` records in its type the languages it may stand for, and a `Polyglot` is read only
under a locale it covers. A greeting in English and French cannot be read where the language
may be German.

```scala fuel
compile
frame
  label Where the locale may be German
  errors show
  code
      val greeting: Polyglot[Text, en & fr] = en(t"Hello") | fr(t"Bonjour")

      def welcome(using Locale[en & de]): Text = greeting()
frame
  label Where it is English or French
  highlight Locale[en..fr]  the languages the greeting has
  code
      val greeting: Polyglot[Text, en & fr] = en(t"Hello") | fr(t"Bonjour")

      def welcome(using Locale[en & fr]): Text = greeting()
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(welcome(using Locale(fr)))
```

[Selecting a language](../../soundness/modules/internationalization.md#selecting-a-language)
has more on locales.
