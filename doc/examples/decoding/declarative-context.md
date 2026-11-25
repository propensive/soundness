# The words which mean true

A `Boolean` is read from `true` or `false`. A tool which also accepts `yes` and `no` says so
with an import, and every decode in its scope accepts those spellings as well.

```scala fuel
compile
frame
  label Only true and false are booleans
  code
      def consent(answer: Text): Optional[Boolean] = safely(answer.as[Boolean])
frame
  label Yes and no are, too
  highlight yesNoTruth  adds two spellings
  code
      import truths.yesNoTruth

      def consent(answer: Text): Optional[Boolean] = safely(answer.as[Boolean])
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(consent(t"yes").inspect)
Out.println(consent(t"True").inspect)
Out.println(consent(t"perchance").inspect)
```

Without the import, the first line would be an absent value too.
[Booleans](../../soundness/modules/decoding.md#booleans) lists the other spellings, and
[declarative context](../../philosophy/declarative-context.md) explains why they are imported.
