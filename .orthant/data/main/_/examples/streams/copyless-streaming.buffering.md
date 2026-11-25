# One given sets the size of every block

No stage of a pipeline chooses the size of its own buffers. Each consults the `Buffering` in
scope, and a consumer asks for no more than that policy allows, so a single given decides how
much data is in flight anywhere in the pipeline, and how much a slow consumer holds back.

```scala fuel
compile
frame
  label The windows of a stream, counted under the standard policy
  highlight gather(0)
  code
      import codepages.utf8Codepage

      val speech = t"Tomorrow, and tomorrow, and tomorrow".in[Data]

      val windows = speech.stream.gather(0): region =>
        (count, window) => count + 1
frame
  label The same stream, under a policy of one-byte blocks
  focus given..16
  code
      import codepages.utf8Codepage

      given buffering: Buffering:
        def capacity(substrate: Substrate): Int = 1
        def depth: Int = 16

      val speech = t"Tomorrow, and tomorrow, and tomorrow".in[Data]

      val windows = speech.stream.gather(0): region =>
        (count, window) => count + 1
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(windows.show)
```

A block which crosses a boundary is sixteen staging blocks, so the thirty-six bytes are lent in
three windows of at most sixteen.
[Tuning backpressure](../../soundness/modules/streams.md#tuning-backpressure) describes each
member of the policy.
