# Text, inspected as source

`show` writes text verbatim, and `inspect` writes it as it would appear in a program, with
its quotes and its escapes. A newline can then be told apart from a backslash and an `n`, and
the output can be pasted back into code.

```scala fuel
compile
frame
  label Text which holds a newline and quotes
  code
      val line: Text = t"\"Exit,\npursued by a bear\""
frame
  label For a reader
  highlight show  verbatim
    occurrence 2
  code
      val line: Text = t"\"Exit,\npursued by a bear\""
      val shown: Text = line.show
frame
  label For a programmer
  highlight inspect  as source code
    occurrence 2
  code
      val line: Text = t"\"Exit,\npursued by a bear\""
      val shown: Text = line.show
      val inspected: Text = line.inspect
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(shown)
Out.println(inspected)
```

[Inspecting](../../soundness/modules/display.md#inspecting) has the forms of the other types.
