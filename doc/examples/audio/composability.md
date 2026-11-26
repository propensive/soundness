# A sample rate is a frequency

The sample rate of a sound is not a bare number of hertz, and its duration is not a bare number
of seconds. Both are quantities, so arithmetic on them has units, and the units are checked.

```scala fuel
compile
frame
  label A sound, read from the bytes of a WAV file
  code
      import strategies.throwUnsafely

      val wav = hex"""524946462c00000057415645666d74201000000001000100401f0000803e00000200100064
                      617461080000006400c8002c019001"""

      val tone = wav.read[Audio in Wave]
frame
  label Its rate, halved
  highlight sampleRate/2  still a frequency
  code
      import strategies.throwUnsafely

      val wav = hex"""524946462c00000057415645666d74201000000001000100401f0000803e00000200100064
                      617461080000006400c8002c019001"""

      val tone = wav.read[Audio in Wave]
      val nyquist = tone.sampleRate/2
frame
  label A frequency added to a time
  errors show
  code
      import strategies.throwUnsafely

      val wav = hex"""524946462c00000057415645666d74201000000001000100401f0000803e00000200100064
                      617461080000006400c8002c019001"""

      val tone = wav.read[Audio in Wave]
      val nyquist = tone.sampleRate/2
      val nonsense = tone.sampleRate + tone.duration
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given decimalizer: Decimalizer = Decimalizer(4)

Out.println(nyquist.show)
Out.println(tone.duration.show)
```

[Reading audio](../../soundness/modules/audio.md#reading-audio) describes what a sound knows
of itself, and [quantities](../../soundness/modules/quantities.md) the arithmetic.
