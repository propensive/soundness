# An identifier, or none

`Uuid.extract` gives an `Optional[Uuid]`: the identifier, or `Unset`. The absent case has no
wrapper to unpack and cannot be used as an identifier by mistake, and `or` says what stands in
for it.

```scala fuel
compile
frame
  label Used as though it were present
  errors show
  code
      def owner(cookie: Text): Text = Uuid.extract(cookie).text
frame
  label Transformed, if it is there
  highlight let(_.text)  applied only to a value which is present
  code
      def owner(cookie: Text): Optional[Text] = Uuid.extract(cookie).let(_.text)
frame
  label And replaced, if it is not
  highlight or(t"anonymous")  what absence means here
  code
      def owner(cookie: Text): Text =
        Uuid.extract(cookie).let(_.text).or(t"anonymous")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(owner(t"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c"))
Out.println(owner(t"Caliban"))
```

[Parsing](../../soundness/modules/uuid.md#parsing) sets `extract` beside `parse`, and
[optionality](../../philosophy/optionality.md) explains `let` and `or`.
