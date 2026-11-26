# A locale, read from a language code

The user's language arrives as text, in a header or a setting. It is read with `as`, like a
number or a date, and the type which is asked for names the languages the program supports.

```scala fuel
compile
frame
  label A greeting in five languages
  code
      val greeting: Polyglot[Text, en & pl & fr & de & es] =
        en(t"Hello") | pl(t"Cześć") | fr(t"Bonjour") | de(t"Hallo") | es(t"Hola")
frame
  label A locale, from a language code
  highlight as[Locale[en..es]]  the languages which the program supports
  code
      val greeting: Polyglot[Text, en & pl & fr & de & es] =
        en(t"Hello") | pl(t"Cześć") | fr(t"Bonjour") | de(t"Hallo") | es(t"Hola")

      def welcome(code: Text): Text =
        given locale: Locale[en & pl & fr & de & es] =
          code.as[Locale[en & pl & fr & de & es]]

        greeting()
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(welcome(t"es-MX"))
Out.println(welcome(t"pl"))
Out.println(welcome(t"tlh"))
```

The last code is not one of the five, and is read as English.
[A locale from runtime data](../../soundness/modules/internationalization.md#a-locale-from-runtime-data)
describes the fallback.
