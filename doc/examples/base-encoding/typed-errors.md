# Text which may not be in the alphabet

Any bytes can be written as Base64, and not every text can be read back from it. `deserialize`
therefore raises a `Serialization.Error`, and a method which calls it must handle the error or
declare it.

```scala fuel
compile
frame
  label A failure which is not accounted for
  errors show
  code
      import alphabets.base64Standard

      def secret(text: Text): Data = text.deserialize[Base64]
frame
  label The failure, declared
  highlight raises..Error  the caller decides
  code
      import alphabets.base64Standard

      def secret(text: Text): Data raises Serialization.Error = text.deserialize[Base64]
frame
  label The failure, as an absent value
  highlight safely  a failure becomes Unset
  code
      import alphabets.base64Standard

      def secret(text: Text): Optional[Data] = safely(text.deserialize[Base64])
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import codepages.utf8Codepage

Out.println(secret(t"SGVsbG8=").let(_.utf8).inspect)
Out.println(secret(t"SGVsbG8!").let(_.utf8).inspect)
```

[Deserializing](../../soundness/modules/base-encoding.md#deserializing) shows the error itself.
