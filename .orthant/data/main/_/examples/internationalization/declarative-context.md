# The language is chosen once

A `Polyglot` is read by applying it, and the language comes from the `Locale` in scope. The
code which reads the text names no language, and one given decides for every text read
beneath it.

```scala fuel
compile
alternative In French
  highlight Locale(fr)  the one place a language is named
  code
      val greeting: Polyglot[Text, en & fr] = en(t"Hello") | fr(t"Bonjour")
      val parting: Polyglot[Text, en & fr] = en(t"Farewell") | fr(t"Adieu")

      def scene(using Locale[en & fr]): Text = t"${greeting()}… ${parting()}"

      val performance: Text =
        given locale: Locale[en & fr] = Locale(fr)
        scene
alternative In English
  highlight Locale(en)  the one place a language is named
  code
      val greeting: Polyglot[Text, en & fr] = en(t"Hello") | fr(t"Bonjour")
      val parting: Polyglot[Text, en & fr] = en(t"Farewell") | fr(t"Adieu")

      def scene(using Locale[en & fr]): Text = t"${greeting()}… ${parting()}"

      val performance: Text =
        given locale: Locale[en & fr] = Locale(en)
        scene
##
```

[Selecting a language](../../soundness/modules/internationalization.md#selecting-a-language)
has the details, and [declarative context](../../philosophy/declarative-context.md) the
reasons for a choice which is not an argument.
