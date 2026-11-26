# The format of a sound, in its type

`Audio in Wave` is read as it is written: audio, in the WAV format. An `Audio in Aiff` is
another type, so a sound in one format is never sent where the other is expected.

```scala fuel
compile
frame
  label A WAV where an AIFF is wanted
  errors show
  code
      import strategies.throwUnsafely

      def master(take: Audio in Aiff): Data = take.read[Data]

      def song(source: Data): Data = master(source.read[Audio in Wave])
frame
  label The WAV, converted
  highlight to[Aiff]  gives an `Audio in Aiff`
  code
      import strategies.throwUnsafely

      def master(take: Audio in Aiff): Data = take.read[Data]

      def song(source: Data): Data = master(source.read[Audio in Wave].to[Aiff])
##
```

[Converting and writing](../../soundness/modules/audio.md#converting-and-writing) describes
the conversion, and [infix types](../../philosophy/infix-types.md) the words which join the
types.
