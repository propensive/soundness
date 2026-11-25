# Every fault in a stylesheet at once

A stylesheet with two mistakes should be told of both. Parsing CSS validates every declaration
and gathers the faults into one error, `Css.Errors`, which the caller handles as it would any
other.

```scala fuel
compile
frame
  label A stylesheet with two faults, read
  code
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      val draft = t"a { colour: red; width: wide }"
      val sheet = draft.read[Css]
frame
  label The faults, captured as a value
  highlight capture[Css.Errors]  the error itself, in place of the stylesheet
  code
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      val draft = t"a { colour: red; width: wide }"
      val faults = capture[Css.Errors](draft.read[Css]).errors
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

faults.each: fault =>
  Out.println(fault.reason.inspect)
```

[Parsing](../../soundness/modules/css.md#parsing) says what each fault records.
