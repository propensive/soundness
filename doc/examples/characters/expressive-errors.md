# Where the bytes went wrong

A strict decoder raises a `Charset.Error` at the first sequence it cannot decode. The error
holds the position of the fault as a number and the encoding as a value, so a program can act
on either without reading the message.

```scala fuel
compile
frame
  label Bytes which are not UTF-8
  highlight -62  begins a character which never ends
  code
      import textSanitizers.strictSanitizer, strategies.throwUnsafely
      import errorDiagnostics.stackTracesDiagnostics

      val bytes = Data(80, 117, -62, 99, 107)
      val error = capture[Charset.Error](charsets.utf8Charset.decoded(bytes))
frame
  label The position of the fault
  highlight error.position  a number, not part of a message
  code
      import textSanitizers.strictSanitizer, strategies.throwUnsafely
      import errorDiagnostics.stackTracesDiagnostics

      val bytes = Data(80, 117, -62, 99, 107)
      val error = capture[Charset.Error](charsets.utf8Charset.decoded(bytes))
      val sound: Data = bytes.keep(error.position)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(error.message.show)
Out.println(sound.utf8)
```

[Bad input](../../soundness/modules/characters.md#bad-input) describes the sanitizers, and
[expressive errors](../../philosophy/expressive-errors.md) why an error keeps its values.
