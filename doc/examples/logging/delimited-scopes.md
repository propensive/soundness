# Silence for the length of a block

`mute` runs a block with one kind of event suppressed. Inside the block the events have
somewhere to go, which is nowhere, so a method which mutes its events no longer declares them.

```scala fuel
compile
frame
  label A method which passes its events on
  code
      enum Cue:
        case Enter(name: Text)

      def enter(name: Text): Unit logs Cue = Log.info(Cue.Enter(name))

      def rehearse(): Unit logs Cue =
        enter(t"Viola")
        enter(t"Feste")
frame
  label A method which keeps them quiet
  highlight mute[Cue]  no cue leaves this block
  code
      enum Cue:
        case Enter(name: Text)

      def enter(name: Text): Unit logs Cue = Log.info(Cue.Enter(name))

      def rehearse(): Unit =
        mute[Cue]:
          enter(t"Viola")
          enter(t"Feste")
##
```

[Silencing](../../soundness/modules/logging.md#silencing) has the import which does the same
for a whole file, and [delimited scopes](../../philosophy/delimited-scopes.md) the idea of a
block which grants something.
