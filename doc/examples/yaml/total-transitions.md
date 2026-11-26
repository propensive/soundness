# A quoted number is not a number

YAML types a plain scalar by its content, and a quoted one is always text. Decoding keeps to
that: a quoted `"42"` asked for as an `Int` is a failure, and is never quietly converted.

```scala fuel
compile
frame
  label A plain scalar, read as a number
  code
      import strategies.throwUnsafely

      val plain: Optional[Int] = safely(t"age: 42".read[Yaml](t"age").as[Int])
frame
  label A quoted scalar, which is text
  highlight \"42\"  a string, whatever it holds
  code
      import strategies.throwUnsafely

      val plain: Optional[Int] = safely(t"age: 42".read[Yaml](t"age").as[Int])
      val quoted: Optional[Int] = safely(t"age: \"42\"".read[Yaml](t"age").as[Int])
frame
  label A tag, which says otherwise
  highlight !!int  the document's own conversion
  code
      import strategies.throwUnsafely

      val plain: Optional[Int] = safely(t"age: 42".read[Yaml](t"age").as[Int])
      val quoted: Optional[Int] = safely(t"age: \"42\"".read[Yaml](t"age").as[Int])
      val tagged: Optional[Int] = safely(t"age: !!int \"42\"".read[Yaml](t"age").as[Int])
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(plain.inspect)
Out.println(quoted.inspect)
Out.println(tagged.inspect)
```

[Scalars, quoting and block styles](../../soundness/modules/yaml.md#scalars-quoting-and-block-styles)
and [tags](../../soundness/modules/yaml.md#tags) have the rules.
