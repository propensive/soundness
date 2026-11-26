# A total computed over the windows themselves

`gather` drains a stream and threads a value through each window of its buffer. The function is
given the window where it lies, and not a copy of it or its elements one by one, so a stream of
any length is reduced in the memory of a single block.

```scala fuel
compile
frame
  label Each window, as it is refilled
  highlight region
  code
      import codepages.utf8Codepage

      val speech = t"Tomorrow, and tomorrow, and tomorrow".in[Data]

      val size = speech.stream.gather(0): region =>
        (total, window) => total
frame
  label Its size, added to the total
  highlight (window:..size  the window is measured where it lies
  code
      import codepages.utf8Codepage

      val speech = t"Tomorrow, and tomorrow, and tomorrow".in[Data]

      val size = speech.stream.gather(0): region =>
        (total, window) => total + (window: Interval).size
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(size.show)
```

[Composing a pipeline](../../soundness/modules/streams.md#composing-a-pipeline) compares `gather`
with the other operations which drain a stream.
