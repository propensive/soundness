# Text which may not be a path

Text is not a path until it has been checked against a platform's rules, and it may fail them.
`as` raises a `Path.Error`, which a method must declare or handle.

```scala fuel
compile
frame
  label A method which does not say that it can fail
  errors show
  code
      def locate(text: Text): Path on Linux = text.as[Path on Linux]
frame
  label The failure, in the return type
  highlight raises..Error  the way it can fail
  code
      def locate(text: Text): Path on Linux raises Path.Error =
        text.as[Path on Linux]
frame
  label The failure, as a value
  highlight capture[Path.Error]  the error, not the path
  code
      import strategies.throwUnsafely, errorDiagnostics.emptyDiagnostics

      def locate(text: Text): Path on Linux raises Path.Error =
        text.as[Path on Linux]

      val error: Path.Error = capture[Path.Error](locate(t"home/viola"))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(error.reason.inspect)
```

[Text and paths](../../soundness/modules/paths.md#text-and-paths) has the conversions, and
[error handling](../../philosophy/error-handling.md) the reasons for an error in a type.
