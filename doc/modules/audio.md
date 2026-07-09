## Audio

### About

Soundness reads and writes audio, records it from the machine's inputs, and plays it
through the machine's outputs. Reading a [WAV](https://en.wikipedia.org/wiki/WAV),
[AIFF](https://en.wikipedia.org/wiki/Audio_Interchange_File_Format) or AU file yields an
`Audio` value — an immutable block of [PCM](https://en.wikipedia.org/wiki/Pulse-code_modulation)
samples that knows its sample rate, channel count, bit depth, and every sample it holds.

The audio's format lives in its type, so an `Audio in Wave` is distinct from an `Audio in
Aiff`, and converting between them, or between an audio value and its encoded bytes, is
one method. Recording and playback stream audio in chunks, with the channel layout —
mono, stereo, or surround — carried in the type as well.

### On audio

Uncompressed audio is simple underneath: a grid of numbers, one per sample per channel,
played back at a fixed rate. The file formats differ mostly in their headers, not their
substance. Java's sound library exposes all of this through mutable objects and untyped
formats, where a wrong channel count or bit depth surfaces as noise rather than an error.

Soundness presents the same capability as immutable, typed values. The format is part of
the type, the sample rate is a genuine [quantity](quantities.md) in hertz rather than a
bare number, and reading, converting and writing are the same polymorphic operations used
everywhere else. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Reading audio

Any source of bytes reads as audio when the expected format is named. Opening a file gives a
handle that reads as an `Audio`, whose metadata is then available directly:

```scala
val audio = p"/home/work/sound.wav".on[Linux].open(_.read[Audio in Wave])

audio.channels        // 2
audio.frames          // 44100
audio.bitsPerSample   // 16
audio.sampleRate      // 44100.0*Hertz
audio.duration        // 1.0*Second
```

Reading can fail — the bytes may not hold audio in the named format — so it draws on the
error strategy in scope. Applying an audio value to a channel and a frame returns that one
sample:

```scala
audio(0, 0)   // the first sample of the left channel
```

Audio that is not already PCM is converted to 16-bit signed PCM as it is read, so every
`Audio` value has the same simple internal form regardless of how it was stored.

### Converting and writing

`to` re-expresses an audio value in another format, and reading it as `Data` produces the
encoded bytes:

```scala
val aiff: Data = audio.to[Aiff].read[Data]
aiff.read[Audio in Aiff].frames   // 44100 — the audio survives the round trip
```

Reading an `Audio` *as* bytes and reading bytes *as* an `Audio` are the same `read`
operation in each direction, so writing a file is reading its audio as `Data` and sending
that to the destination.

### Channel layouts

A channel layout can be recorded in the type alongside the format, written with `across`:
`Monaural` for one channel, `Stereo` for two, and `Surround[n]` for a surround
configuration of `n` channels. The layout is a typeclass, so its channel count is known
statically:

```scala
summon[Stereo is ChannelLayout].channels      // 2
summon[Surround[6] is ChannelLayout].channels  // 6
```

### Recording

An input device is a `Feed`, and the machine's feeds are listed with `Feed.list`. A feed
records at a chosen sample rate, bit depth and layout, producing a `Recording` whose
`stream` yields audio in chunks until it is stopped:

```scala
val feed = Feed.list.head

val recording = feed.record[Stereo](44100.0*Hertz, bits = 16)
val firstChunk = recording.stream.head
recording.stop()
```

`feed.supports[Stereo](44100.0*Hertz, 16)` asks whether a feed can honour a configuration
before recording begins. Recording from a feed that is unavailable or misconfigured raises
a `FeedError`. On macOS the JVM must have been granted microphone permission, or no feeds
are available.

### Playback

An output device is an `Outlet`, listed with `Outlet.list`. Playing an audio value returns
a `Playback` that runs in the background; `await` blocks until it finishes, and `stop`
ends it early:

```scala
val outlet = Outlet.list.head

val playback = outlet.play(audio)
playback.await()
```

Playing to an outlet that is unavailable or cannot accept the audio's configuration raises
an `OutletError`.
