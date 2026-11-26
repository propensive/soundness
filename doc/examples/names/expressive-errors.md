# An error which names the broken rule

Each rule carries a phrase which describes what it demands. Text which fails validation at
runtime raises a `Name.Error` which holds the rule, and its message uses the rule's own words.

```scala fuel
compile
frame
  label A plane with three rules
  code
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      sealed trait Tag

      inline given Tag is Nominative under
          MustNotStart["0"] & MustNotContain["."] & MustNotEqual[""] = !!
frame
  label The error for text which breaks one of them
  highlight capture[Name.Error]  the error, as a value
  code
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      sealed trait Tag

      inline given Tag is Nominative under
          MustNotStart["0"] & MustNotContain["."] & MustNotEqual[""] = !!

      val error: Name.Error = capture[Name.Error](Name[Tag](t"v1.0"))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(error.message.show)
```

[The rules](../../soundness/modules/names.md#the-rules) lists the rules and their phrases, and
[expressive errors](../../philosophy/expressive-errors.md) explains why an error is worded for
the person who must fix the input.
