# Output, collected for the length of a block

`Stdio.capture` runs a block and gives back what the block printed, to each of the two
streams. Within it, standard output and standard error are the capture's own. After it, they
are what they were.

```scala fuel
compile
frame
  label A block which prints
  highlight Stdio.capture  collects both streams
  code
      import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

      val captured = Stdio.capture:
        Out.println(t"Enter Viola")
        Err.println(t"Thunder")
frame
  label One stream, sent to the other
  highlight Err.divert(Out)  for this block only
  code
      import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

      val captured = Stdio.capture:
        Err.divert(Out):
          Out.println(t"Enter Viola")
          Err.println(t"Thunder")
##
```

```scala fuel
run
##
Out.println(captured.out.trim)
Out.println(captured.err.length.show)
```

Both lines were printed to standard output, and nothing reached standard error.
[Standard streams](../../soundness/modules/streams.md#standard-streams) has more, and
[delimited scopes](../../philosophy/delimited-scopes.md) the shape of the block.
