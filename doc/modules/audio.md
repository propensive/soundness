## Audio

### About

Audio reads and writes as typed values, records from the machine's inputs, and plays through the
machine's outputs. Reading a [WAV](https://en.wikipedia.org/wiki/WAV),
[AIFF](https://en.wikipedia.org/wiki/Audio_Interchange_File_Format), AIFC or AU file yields an
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

A sample's format living in its type is what makes an unsupported conversion a compile error: [impossible states](../philosophy/impossible-states.md) are simply not representable.

Soundness presents the same capability as immutable, typed values. The format is part of
the type, the sample rate is a genuine [quantity](quantities.md) in hertz rather than a
bare number, and reading, converting and writing are the same polymorphic operations used
everywhere else. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Reading audio

Any source of bytes reads as audio when the expected format is named. These bytes are a
complete WAV file — one channel, 16-bit signed samples at 8 kHz, four frames holding the values
100, 200, 300 and 400 — small enough to write down:

```scala
val wav = hex"""524946462c00000057415645666d74201000000001000100401f0000803e00000200100064
                617461080000006400c8002c019001"""

val audio = wav.read[Audio in Wave]

audio.channels        // 1
audio.frames          // 4
audio.bitsPerSample   // 16
audio.sampleRate      // 8000.0*Hertz
audio.duration        // 0.0005*Second
```

Reading can fail — the bytes may not hold audio in the named format — so it draws on the
error strategy in scope, raising an `Audio.Error` that names the format expected. Applying an
audio value to a channel and a frame returns that one sample:

```scala
audio(0, 0)   // 100
audio(0, 2)   // 300
```

Audio that is not already PCM is converted to 16-bit signed PCM as it is read, so every
`Audio` value has the same simple internal form regardless of how it was stored. A file on disk
reads the same way through its handle, for the duration of a scope, as any
[file](filesystem.md) does:

```scala
import filesystemOptions.createNonexistentParents
import pathInterfaces.pathOnLinux
import temporaryDirectories.javaBaseTemporaryDirectory

val wavPath = temporaryDirectory[Path on Linux] / "audio" / "tone.wav"
wavPath.create[File](): handle ?=>
  handle.write(wav)

wavPath.open[File]()(file.read[Audio in Wave]).frames   // 4
```

### Converting and writing

`to` re-expresses an audio value in another format, and reading it as `Data` produces the
encoded bytes:

```scala
val aiff: Data = audio.to[Aiff].read[Data]
aiff.read[Audio in Aiff].frames   // 4 — the audio survives the round trip
aiff.read[Audio in Aiff](0, 2)    // 300
```

Reading an `Audio` *as* bytes and reading bytes *as* an `Audio` are the same `read`
operation in each direction, so writing a file is reading its audio as `Data` and sending
that to the destination — or handing the audio value itself to the handle, since an `Audio`
streams as its encoded bytes:

```scala
val aiffPath = temporaryDirectory[Path on Linux] / "audio" / "tone.aiff"
aiffPath.create[File](): handle ?=>
  handle.write(audio.to[Aiff])
```

### Channel layouts

A channel layout can be recorded in the type alongside the format, written with `across`:
`Monaural` for one channel, `Stereo` for two, and `Surround[n]` for a surround
configuration of `n` channels. The layout is a typeclass, so its channel count is known
statically:

```scala
summon[Stereo is ChannelLayout].channels       // 2
summon[Surround[6] is ChannelLayout].channels  // 6
```

### Recording

An input device is a `Feed`, and the machine's feeds are listed with `Feed.list`, each with
a `name`, a `vendor` and a `description`, and the `configurations` it offers. A feed records
at a chosen sample rate, bit depth and layout, producing a `Recording` whose `stream` yields
audio in chunks until it is stopped. A machine may have no feeds at all, so the first one is
an `Optional`:

```scala
Feed.list.prim.let: feed =>
  if feed.supports[Stereo](44100.0*Hertz, 16) then
    val recording = feed.record[Stereo](44100.0*Hertz, bits = 16)
    val firstChunk: Optional[Audio across Stereo] = recording.stream.prim
    recording.stop()
```

`supports` asks whether a feed can honor a configuration before recording begins. Recording
from a feed that is unavailable or misconfigured raises a `Feed.Error`. On macOS the JVM must
have been granted microphone permission, or no feeds are available.

### Playback

An output device is an `Outlet`, listed with `Outlet.list`. Playing an audio value returns
a `Playback` that runs in the background; `await` blocks until it finishes, and `stop`
ends it early:

```scala
Outlet.list.prim.let: outlet =>
  val playback = outlet.play(audio)
  playback.await()
```

Playing to an outlet that is unavailable or cannot accept the audio's configuration raises
an `Outlet.Error`.

### Scoped lines

`record` and `play` suit open-ended use, where the device is held for as long as the program
wants it. Where the device should be held for exactly one block, a line is *opened* instead, as
[a file is](filesystem.md): the audio line lasts precisely as long as the scope, and the layout
and configuration are given as the form and its flags — `PcmFlag.Rate`, `PcmFlag.Bits` and
`PcmFlag.Chunk`:

```scala
Feed.list.prim.let: feed =>
  feed.open[Pcm across Stereo](Read, PcmFlag.Rate(48000), PcmFlag.Chunk(1024)): input ?=>
    input.stream.prim

Outlet.list.prim.let: outlet =>
  outlet.open[Pcm](Write): out ?=>
    out.play(audio)
```

Playing requires the `Write` grant, so a line opened for capture cannot be played to, and the
compiler — not the audio driver — says so.
