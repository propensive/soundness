# A type which grows with each translation

Each language is a value which wraps a translation, and `|` joins two of them. The type of the
result is computed from its operands: it lists the languages which have been supplied so far.

```scala fuel
compile
frame
  label English
  code
      val farewell: Polyglot[Text, en] = en(t"Farewell")
frame
  label And French
  highlight fr(t"Adieu")  adds fr to the type
  code
      val farewell: Polyglot[Text, en & fr] = en(t"Farewell") | fr(t"Adieu")
frame
  label And German
  highlight de(t"Lebewohl")  adds de to the type
  code
      val farewell: Polyglot[Text, en & fr & de] =
        en(t"Farewell") | fr(t"Adieu") | de(t"Lebewohl")
##
```

[Providing translations](../../soundness/modules/internationalization.md#providing-translations)
has the languages.
