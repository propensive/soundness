## Audio

### About

Audio is read and written as typed values, recorded from the inputs of the machine, and played
through its outputs. Reading a [WAV](https://en.wikipedia.org/wiki/WAV),
[AIFF](https://en.wikipedia.org/wiki/Audio_Interchange_File_Format), AIFC or AU file gives an
`Audio` value: an [immutable] block of
[PCM](https://en.wikipedia.org/wiki/Pulse-code_modulation) samples which knows its sample rate,
its number of channels, its bit depth, and every sample it holds.

The format of the audio lives in its type. An `Audio in Wave` is distinct from an
`Audio in Aiff`, and converting between them, or between an audio value and its encoded bytes,
is one method. Recording and playback stream audio in chunks, with the channel layout, which is
mono, stereo or surround, carried in the type as well.

### On audio

Uncompressed audio is simple underneath: a grid of numbers, one for each sample of each channel,
played back at a fixed rate. The file formats differ mostly in their headers, and not in their
substance. Java's sound library exposes all of this through mutable objects and untyped formats,
where a wrong channel count or bit depth appears as noise rather than as an error.

Here the same capability is presented as immutable, typed values. The format is part of the
type. The sample rate is a real [quantity](quantities.md) in hertz, not a bare number. Reading,
converting and writing are the same polymorphic operations used everywhere else. Since the
format is in the type, an unsupported conversion is a compile error, and such
[impossible states](../philosophy/impossible-states.md) cannot be represented.

Everything comes from the `soundness` package. The second import chooses a
[strategy](errors.md#choosing-a-strategy) for errors, and the third lets the samples print their
results. The `Decimalizer` fixes how many significant figures a [quantity](quantities.md) is
printed with:

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given decimalizer: Decimalizer = Decimalizer(4)
```

### Reading audio

Any source of bytes reads as audio when the expected format is named, as one of these types:

```tabular
column  Format
column  Type
column  Media type
row  WAV  `Wave`  `audio/x-wav`
row  AIFF  `Aiff`  `audio/x-aiff`
row  AIFC  `Aifc`  `audio/x-aifc`
row  AU  `Au`  `audio/basic`
row  SND  `Snd`  `audio/basic`
```

These bytes are a complete WAV file, with one channel, 16-bit signed samples at 8 kHz, and four
frames holding the values 100, 200, 300 and 400. It is small enough to write down:

```scala
val wav = hex"""524946462c00000057415645666d74201000000001000100401f0000803e00000200100064
                617461080000006400c8002c019001"""

val audio = wav.read[Audio in Wave]
```

The value knows its shape:

```scala fuel
run
##
Out.println(audio.channels.show)
Out.println(audio.frames.show)
Out.println(audio.bitsPerSample.show)
Out.println(audio.sampleRate.show)
Out.println(audio.duration.show)
```

The sample rate is printed in inverse seconds, which is hertz, and the duration in seconds.

Reading can fail, since the bytes may not hold audio in the named format. It therefore draws on
the error strategy in scope, and raises an `Audio.Error` which names the format expected:

```scala fuel
run
errors show
##
wav.read[Audio in Aiff]
```

Applying an audio value to a channel and a frame gives that one sample:

```scala fuel
run
##
Out.println(audio(0, 0).show)
Out.println(audio(0, 2).show)
```

Audio which is not already PCM is converted to 16-bit signed PCM as it is read, so every `Audio`
value has the same simple internal form, however it was stored. A file on disk reads the same
way through its handle, for the duration of a scope, as any [file](filesystem.md) does:

```scala
import filesystemBackends.javaBaseFilesystem
import pathInterfaces.pathOnLinux
import temporaryDirectories.javaBaseTemporaryDirectory

val audioDirectory = temporaryDirectory[Path on Linux] / "audio" / Uuid().show
val wavPath = audioDirectory / "tone.wav"
```

```scala fuel
run
##
wavPath.create[File](CreateFlag.Parents): handle ?=>
  handle.write(wav)

Out.println(wavPath.open[File]()(file.read[Audio in Wave]).frames.show)
```

### Converting and writing

`to` re-expresses an audio value in another format, and reading it as `Data` produces the
encoded bytes. The audio survives the round trip:

```scala fuel
run
##
val aiff: Data = audio.to[Aiff].read[Data]

Out.println(aiff.read[Audio in Aiff].frames.show)
Out.println(aiff.read[Audio in Aiff](0, 2).show)
```

Reading an `Audio` *as* bytes and reading bytes *as* an `Audio` are the same `read` operation,
in each direction. Writing a file is therefore reading its audio as `Data` and sending that to
the destination. Or the audio value itself is handed to the handle, since an `Audio` streams as
its encoded bytes:

```scala fuel
run
##
val aiffPath = audioDirectory / "tone.aiff"

aiffPath.create[File](CreateFlag.Parents): handle ?=>
  handle.write(audio.to[Aiff])

Out.println(aiffPath.open[File]()(file.read[Audio in Aiff]).frames.show)
```

### Channel layouts

A channel layout can be recorded in the type beside the format, written with `across`:

```tabular
column  Layout
column  Channels
row  `Monaural`  1
row  `Stereo`  2
row  `Surround[n]`  `n`, in a surround configuration
```

The layout is a [typeclass], so its channel count is known statically:

```scala fuel
run
##
Out.println(summon[Stereo is ChannelLayout].channels.show)
Out.println(summon[Surround[6] is ChannelLayout].channels.show)
```

### Recording

The rest of this page needs audio devices, so its samples are compiled but not run.

An input device is a `Feed`. The feeds of the machine are listed with `Feed.list`, each with a
`name`, a `vendor`, a `description`, and the `configurations` it offers. A feed records at a
chosen sample rate, bit depth and layout, and produces a `Recording` whose `stream` yields audio
in chunks until it is stopped. A machine may have no feeds at all, so the first one is an
[optional] value:

```scala
Feed.list.head.let: feed =>
  if feed.supports[Stereo](44100.0*Hertz, 16) then
    val recording = feed.record[Stereo](44100.0*Hertz, bits = 16)
    val firstChunk: Optional[Audio across Stereo] = recording.stream.head
    recording.stop()
```

`supports` asks whether a feed can honor a configuration before recording begins. Recording
from a feed which is unavailable or misconfigured raises a `Feed.Error`. On macOS the JVM must
have been granted permission to use the microphone, or no feeds are available.

### Playback

An output device is an `Outlet`, listed with `Outlet.list`. Playing an audio value returns a
`Playback`, which runs in the background. `await` blocks until it finishes, and `stop` ends it
early:

```scala
Outlet.list.head.let: outlet =>
  val playback = outlet.play(audio)
  playback.await()
```

Playing to an outlet which is unavailable, or which cannot accept the configuration of the
audio, raises an `Outlet.Error`.

### Scoped lines

`record` and `play` suit open-ended use, where the device is held for as long as the program
wants it. Where the device should be held for exactly one block, a line is *opened* instead, as
[a file is](filesystem.md). The line lasts precisely as long as the scope. The layout and the
configuration are given as the form and its flags, which are `PcmFlag.Rate`, `PcmFlag.Bits` and
`PcmFlag.Chunk`:

```scala
Feed.list.head.let: feed =>
  feed.open[Pcm across Stereo](Read, PcmFlag.Rate(48000), PcmFlag.Chunk(1024)): input ?=>
    input.stream.head

Outlet.list.head.let: outlet =>
  outlet.open[Pcm](Write): out ?=>
    out.play(audio)
```

Playing needs the `Write` grant, so a line opened for capture cannot be played to, and the
compiler says so, not the audio driver.

The two kinds of device side by side:

```tabular
column  Aspect
column  `Feed`
column  `Outlet`
row  Direction  input  output
row  Listed with  `Feed.list`  `Outlet.list`
row  Open-ended use  `record`, giving a `Recording`  `play`, giving a `Playback`
row  Scoped line  `open[Pcm](Read, …)`  `open[Pcm](Write, …)`
row  Error  `Feed.Error`  `Outlet.Error`
```

### Related

- [Quantities](quantities.md) describes the hertz and seconds in which a sample rate and a
  duration are measured.
- [Filesystem](filesystem.md) describes the handles which audio files are read and written
  through.
- [Binary data](binary-data.md) describes `hex"…"` and the `Data` which encoded audio is.
- [Streams](streams.md) describes `read`, which works in both directions here.
- [Media types](media-types.md) names the formats.
- [Images](images.md) carry their format in their type in the same way.
