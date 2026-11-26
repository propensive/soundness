# A line held for one block

An output device is opened as a line for the length of a block, and released when the block
ends. Playing through the line needs the `Write` grant, so a line which was opened to read
cannot be played to.

```scala fuel
compile
frame
  label A line opened for reading
  errors show
  code
      def perform(outlet: Audio.Outlet, song: Audio): Unit raises Audio.Outlet.Error =
        outlet.open[Pcm](Read): out ?=>
          out.play(song)
frame
  label A line opened for writing
  highlight Write  the grant which `play` asks for
  code
      def perform(outlet: Audio.Outlet, song: Audio): Unit raises Audio.Outlet.Error =
        outlet.open[Pcm](Write): out ?=>
          out.play(song)
##
```

The example is compiled, and is not run, since it needs an audio device.
[Scoped lines](../../soundness/modules/audio.md#scoped-lines) describes the flags of a line,
and [delimited scopes](../../philosophy/delimited-scopes.md) the pattern.
