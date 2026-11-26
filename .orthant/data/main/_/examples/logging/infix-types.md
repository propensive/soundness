# One event, carried inside another

A method names the events it logs in its type, with `logs`. Where it calls a method which logs
events of another kind, one given says how those are carried, and its type reads as the
sentence it means: a `Performance` transcribes a `Cue`.

```scala fuel
compile
frame
  label A method which logs cues
  highlight logs  the events it emits
  code
      enum Cue:
        case Enter(name: Text)

      def enter(name: Text): Unit logs Cue = Log.info(Cue.Enter(name))
frame
  label A method which logs performances, and calls it
  highlight transcribes  each cue becomes a scene
  code
      enum Cue:
        case Enter(name: Text)

      def enter(name: Text): Unit logs Cue = Log.info(Cue.Enter(name))

      enum Performance:
        case Scene(cue: Cue)
        case Interval

      given (Performance transcribes Cue) = Performance.Scene(_)

      def perform(): Unit logs Performance =
        enter(t"Viola")
        Log.info(Performance.Interval)
##
```

[Declaring that a method logs](../../soundness/modules/logging.md#declaring-that-a-method-logs)
has the same arrangement between two libraries, and
[infix types](../../philosophy/infix-types.md) the vocabulary of `logs` and `transcribes`.
