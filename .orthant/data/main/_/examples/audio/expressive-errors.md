# Bytes which are not the format they are read as

Reading names the format which is expected, so bytes in another format are an error which says
which format failed, and not noise. These bytes are a WAV file.

```scala fuel
compile
frame
  label A WAV file
  code
      import strategies.throwUnsafely

      val wav = hex"""524946462c00000057415645666d74201000000001000100401f0000803e00000200100064
                      617461080000006400c8002c019001"""

      val tone = wav.read[Audio in Wave]
##
```

```scala fuel
run
errors show
##
wav.read[Audio in Aiff]
```

[Reading audio](../../soundness/modules/audio.md#reading-audio) describes the `Audio.Error`
which is raised, and [expressive errors](../../philosophy/expressive-errors.md) what an error
should say.
