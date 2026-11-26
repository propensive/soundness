# Bytes in a JSON document

JSON has no type for bytes, so they travel in a string. `serialize` gives the text, and a
`j"…"` literal encodes it where it is substituted. The two libraries meet at `Text`, and
neither has a method for the other.

```scala fuel
compile
frame
  label A key, as bytes
  code
      val key: Data = hex"cafebabe0badf00d"
frame
  label The same key, in a JSON string
  highlight ${key.serialize[Base64]}  text, encoded as a JSON string
  code
      import alphabets.base64Url

      val key: Data = hex"cafebabe0badf00d"
      val message: Json = j"""{"player": "Viola", "key": ${key.serialize[Base64]}}"""
frame
  label And read back
  highlight deserialize[Base64]  bytes again
  code
      import alphabets.{base64Url, hexLowerCase}, strategies.throwUnsafely

      val key: Data = hex"cafebabe0badf00d"
      val message: Json = j"""{"player": "Viola", "key": ${key.serialize[Base64]}}"""
      val back: Text = message(t"key").as[Text].deserialize[Base64].serialize[Hex]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import formatting.compactJsonFormatting

Out.println(message.show)
Out.println(back)
```

[Serializing bytes](../../soundness/modules/base-encoding.md#serializing-bytes) and
[building JSON](../../soundness/modules/json.md#building-json) describe the two halves.
