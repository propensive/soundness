# Every language, or it does not compile

A `Polyglot` value records in its type the languages it has been written in. Where English and
French are both required, a value with only English is not enough, and the missing
translation is found by the compiler and not by a French reader.

```scala fuel
compile
frame
  label English alone
  errors show
  code
      val greeting: Polyglot[Text, en & fr] = en(t"Hello")
frame
  label English and French
  highlight fr(t"Bonjour")  the French, which the type requires
  code
      val greeting: Polyglot[Text, en & fr] = en(t"Hello") | fr(t"Bonjour")
##
```

[Providing translations](../../soundness/modules/internationalization.md#providing-translations)
goes on to select one by locale.
