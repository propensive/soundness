                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package cacophony

import java.io as ji
import javax.sound.sampled as jss

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import prepositional.*
import quantitative.*
import symbolism.*
import turbulence.*
import vacuous.*
import zephyrine.*

object Audio:
  def apply[streamable: Streamable by Data over Credit](input: streamable)
  :   Audio raises Audio.Error =

    // `ByteArrayInputStream` only reads the array it wraps.
    val rawBytes: scala.Array[Byte] = Array.unsafeJvm(input.read[Data])

    val raw: jss.AudioInputStream =
      try jss.AudioSystem.getAudioInputStream(ji.ByteArrayInputStream(rawBytes)).nn
      catch
        case _: jss.UnsupportedAudioFileException => abort(Audio.Error(Unset))
        case _: ji.IOException                    => abort(Audio.Error(Unset))

    val encoding = raw.getFormat.nn.getEncoding.nn

    val pcm: jss.AudioInputStream =
      if
        encoding == jss.AudioFormat.Encoding.PCM_SIGNED ||
          encoding == jss.AudioFormat.Encoding.PCM_UNSIGNED
      then raw
      else
        val src = raw.getFormat.nn

        val target =
          jss.AudioFormat
            ( jss.AudioFormat.Encoding.PCM_SIGNED,
              src.getSampleRate,
              16,
              src.getChannels,
              src.getChannels*2,
              src.getSampleRate,
              false )

        try jss.AudioSystem.getAudioInputStream(target, raw).nn
        catch case _: IllegalArgumentException => abort(Audio.Error(Unset))

    // `readAllBytes` returns a fresh array that nothing else holds.
    val pcmBytes: Array[Byte]^{} = Array.unsafeFrozen(pcm.readAllBytes.nn)
    val pcmFormat: jss.AudioFormat = pcm.getFormat.nn
    pcm.close()
    new Audio(pcmFormat, pcmBytes)

  def apply[form: Audible as audible](format: jss.AudioFormat, data: Array[Byte]^{})
  :   Audio in form =

    new Audio(format, data):
      type Form = form

  private[cacophony] def of[layout](format: jss.AudioFormat, data: Array[Byte]^{})
  :   Audio across layout =

    new Audio(format, data):
      type Domain = layout

  private def writeAudio(audio: Audio, formatName: Text): Chain[Data] =
    // `ByteArrayInputStream` only reads the samples.
    val samples = ji.ByteArrayInputStream(Array.unsafeJvm(audio.data))
    val ais = jss.AudioInputStream(samples, audio.format, audio.frames)

    val fileType = jss.AudioSystem.getAudioFileTypes.nn.find(_.toString == formatName.s).getOrElse:
      throw RuntimeException(s"unregistered audio file format: ${formatName.s}")

    val out = StreamOutputStream()
    jss.AudioSystem.write(ais, fileType, out)
    out.close()
    out.stream

  given streamable: [form: Audible]
  =>  (Audio in form) is Streamable by Data over Credit =
    audio => zephyrine.Stream(writeAudio(audio, form.name))

  given streamableAcross: [form: Audible, layout]
  =>  (Audio in form across layout) is Streamable by Data over Credit =

    audio => zephyrine.Stream(writeAudio(audio, form.name))

  given abstractable: [format: Audible] => (Audio in format) is Abstractable:
    type Domain = HttpStreams
    type Result = HttpStreams.Content

    def genericize(audio: Audio in format): HttpStreams.Content =
      (format.mediaType.basic, HttpStreams.Body(audio.source[Data].chain))

  given aggregable: [format: Audible as audible] => (tactic: Tactic[Audio.Error])
  =>  ( ((Audio in format) is Aggregable by Data)^{tactic} ) =

    audible.read(_)

  given aggregable2: (tactic: Tactic[Audio.Error])
  =>  ( (Audio is Aggregable by Data)^{tactic} ) = Audio(_)

  // AudioError → Audio.Error
  case class Error(audible: Optional[Audible])(using Diagnostics)
  extends
    fulminate.Error(m"unable to read the audio in ${audible.lay(t"unspecified")(_.name)} format")

  case class Configuration
    ( channels:      Int,
      sampleRate:    Optional[Quantity[Seconds[-1]]],
      bitsPerSample: Int,
      encoding:      Sonation,
      bigEndian:     Boolean )

  // What a `Feed` and an `Outlet` share: a mixer the system describes, and the lines of one kind it
  // offers — target lines to record from, or source lines to play to.
  trait Device:
    import javax.sound.sampled as jss
    import anticipation.*
    import quantitative.*
    import symbolism.*
    import vacuous.*

    private[cacophony] val mixerInfo: jss.Mixer.Info
    protected def lineClass: Class[? <: jss.DataLine]

    def name:        Text = mixerInfo.getName.nn.tt
    def vendor:      Text = mixerInfo.getVendor.nn.tt
    def description: Text = mixerInfo.getDescription.nn.tt

    private def lineInfos(mixer: jss.Mixer): scala.Array[jss.Line.Info | Null] =
      if lineClass == classOf[jss.TargetDataLine] then mixer.getTargetLineInfo.nn
      else mixer.getSourceLineInfo.nn

    def configurations: List[Configuration] =
      val mixer = jss.AudioSystem.getMixer(mixerInfo).nn

      lineInfos(mixer).iterator.toList.flatMap:
        case dli: jss.DataLine.Info if dli.getLineClass == lineClass =>
          dli.getFormats.nn.iterator.toList.map: f0 =>
            val f = f0.nn

            val encoding =
              if f.getEncoding == jss.AudioFormat.Encoding.PCM_UNSIGNED then Sonation.PcmUnsigned
              else Sonation.PcmSigned

            val rate: Optional[Quantity[Seconds[-1]]] =
              if f.getSampleRate < 0 then Unset else f.getSampleRate.toDouble*Hertz

            Configuration(f.getChannels, rate, f.getSampleSizeInBits, encoding, f.isBigEndian)

        case _ => scala.collection.immutable.Nil

      . to(List)

    def supports[layout: ChannelLayout](rate: Quantity[Seconds[-1]], bits: Int): Boolean =
      val mixer = jss.AudioSystem.getMixer(mixerInfo).nn
      mixer.isLineSupported(jss.DataLine.Info(lineClass, pcm[layout](rate, bits)))

    // Signed little-endian PCM at `rate` with `bits` per sample, in `layout`'s channels.
    protected def pcm[layout: ChannelLayout as cl](rate: Quantity[Seconds[-1]], bits: Int)
    :   jss.AudioFormat =

      val sampleRate = rate.value.toFloat
      val format = jss.AudioFormat.Encoding.PCM_SIGNED
      jss.AudioFormat
        ( format, sampleRate, bits, cl.channels, cl.channels*(bits/8), sampleRate, false )

  object Feed:
    import javax.sound.sampled as jss
    import anticipation.*
    import contingency.*
    import fulminate.*
    import prepositional.*
    import quantitative.*

    def list: List[Feed] =

      jss.AudioSystem.getMixerInfo.nn.iterator.toList.flatMap: info0 =>
        val info = info0.nn
        val mixer = jss.AudioSystem.getMixer(info).nn

        val canRecord = mixer.getTargetLineInfo.nn.exists:
          case dli: jss.DataLine.Info => dli.getLineClass == classOf[jss.TargetDataLine]
          case _                      => false

        if canRecord then scala.collection.immutable.List(Feed(info))
        else scala.collection.immutable.Nil

      . to(List)

    // FeedError → Feed.Error
    object Error:
      enum Reason(val number: Int) extends Clarification:
        case Unavailable              extends Reason(1)
        case UnsupportedConfiguration extends Reason(2)
        case Closed                   extends Reason(3)

      given Reason is Communicable =
        case Reason.Unavailable              => m"the audio line could not be opened"
        case Reason.UnsupportedConfiguration => m"the requested configuration is not supported"
        case Reason.Closed                   => m"the recording has already been stopped"

    case class Error(feed: Text, reason: Feed.Error.Reason)(using Diagnostics)
    extends fulminate.Error(440, reason.number)(m"could not record from feed $feed because $reason")

  case class Feed(private[cacophony] val mixerInfo: jss.Mixer.Info) extends Device:
    import javax.sound.sampled as jss
    import anticipation.*
    import contingency.*
    import fulminate.*
    import prepositional.*
    import quantitative.*

    protected def lineClass: Class[? <: jss.DataLine] = classOf[jss.TargetDataLine]

    def record[layout: ChannelLayout]
      ( rate: Quantity[Seconds[-1]], bits: Int, chunkBytes: Int = 65536 )
    :   Recording across layout raises Feed.Error =

      val format = pcm[layout](rate, bits)

      val mixer = jss.AudioSystem.getMixer(mixerInfo).nn
      val info = jss.DataLine.Info(classOf[jss.TargetDataLine], format)

      if !mixer.isLineSupported(info)
      then abort(Feed.Error(name, Feed.Error.Reason.UnsupportedConfiguration))

      val line: jss.TargetDataLine =
        try mixer.getLine(info).nn.asInstanceOf[jss.TargetDataLine]
        catch case _: jss.LineUnavailableException =>
          abort(Feed.Error(name, Feed.Error.Reason.Unavailable))

      try line.open(format)
      catch case _: jss.LineUnavailableException =>
        abort(Feed.Error(name, Feed.Error.Reason.Unavailable))

      line.start()

      new Recording:
        type Domain = layout
        // [field-purity] stopped flag in anonymous Recording
        @scala.caps.unsafe.untrackedCaptures
        private var stopped = false

        def active: Boolean = !stopped

        def stop(): Unit =
          if !stopped then
            stopped = true
            line.stop()
            line.close()

        def stream: Chain[Audio across layout] =
          def recur: Chain[Audio across layout] =
            if stopped then Chain() else
              val buf = Array.allocate[Byte](chunkBytes)
              val n = line.read(buf.raw, 0, chunkBytes)

              if n <= 0 then Chain() else
                val audio =
                  Audio.of[layout]
                    ( line.getFormat.nn,
                      if n == chunkBytes then Array.freeze(buf)
                      else Array.freeze(Array.grow(buf, n)) )

                audio #:: recur

          Chain.defer(recur)

  object Outlet:
    import javax.sound.sampled as jss
    import scala.math
    import anticipation.*
    import contingency.*
    import fulminate.*
    import rudiments.*

    def list: List[Outlet] =

      jss.AudioSystem.getMixerInfo.nn.iterator.toList.flatMap: info0 =>
        val info = info0.nn
        val mixer = jss.AudioSystem.getMixer(info).nn

        val canPlay = mixer.getSourceLineInfo.nn.exists:
          case dli: jss.DataLine.Info => dli.getLineClass == classOf[jss.SourceDataLine]
          case _                      => false

        if canPlay then scala.collection.immutable.List(Outlet(info))
        else scala.collection.immutable.Nil

      . to(List)

    // OutletError → Outlet.Error
    object Error:
      enum Reason(val number: Int) extends Clarification:
        case Unavailable              extends Reason(1)
        case UnsupportedConfiguration extends Reason(2)
        case Closed                   extends Reason(3)

      given Reason is Communicable =
        case Reason.Unavailable              => m"the audio line could not be opened"
        case Reason.UnsupportedConfiguration => m"the requested configuration is not supported"
        case Reason.Closed                   => m"the playback has already been stopped"

    case class Error(outlet: Text, reason: Outlet.Error.Reason)(using Diagnostics)
    extends fulminate.Error(374, reason.number)(m"could not play to outlet $outlet because $reason")

  case class Outlet(private[cacophony] val mixerInfo: jss.Mixer.Info) extends Device:
    import javax.sound.sampled as jss
    import scala.math
    import anticipation.*
    import contingency.*
    import fulminate.*
    import rudiments.*

    protected def lineClass: Class[? <: jss.DataLine] = classOf[jss.SourceDataLine]

    def play(audio: Audio, chunkBytes: Int = 65536): Playback raises Outlet.Error =
      val mixer = jss.AudioSystem.getMixer(mixerInfo).nn
      val info = jss.DataLine.Info(classOf[jss.SourceDataLine], audio.format)

      if !mixer.isLineSupported(info)
      then abort(Outlet.Error(name, Outlet.Error.Reason.UnsupportedConfiguration))

      val line: jss.SourceDataLine =
        try mixer.getLine(info).nn.asInstanceOf[jss.SourceDataLine]
        catch case _: jss.LineUnavailableException =>
          abort(Outlet.Error(name, Outlet.Error.Reason.Unavailable))

      try line.open(audio.format)
      catch case _: jss.LineUnavailableException =>
        abort(Outlet.Error(name, Outlet.Error.Reason.Unavailable))

      line.start()

      new Playback:
        // Set once, by whichever of `stop` and the worker's own completion comes first. The worker
        // polls it between chunks on its own thread, so it is atomic for a `stop` to reach it, and
        // `ere(true)` returning `false` names the one call that gets to close the line.
        private val stopped: Atomic[Boolean] = Atomic(false)

        private val data: Array[Byte]^{} = audio.data

        private val worker: Thread =
          val task: Runnable^{this} = () =>
            try
              // `SourceDataLine.write` only reads the samples.
              val samples = Array.unsafeJvm(data)
              var offset = 0

              while !stopped() && offset < data.length do
                val len     = math.min(chunkBytes, data.length - offset)
                val written = line.write(samples, offset, len)
                if written <= 0 then offset = data.length else offset += written

              if !stopped() then line.drain()
            finally
              if !stopped.ere(true) then
                line.stop()
                line.close()

          Thread.ofVirtual.nn.start(task).nn

        def active: Boolean = !stopped()

        def stop(): Unit =
          if !stopped.ere(true) then
            line.stop()
            line.flush()
            line.close()

        def await(): Unit = worker.join()

// Pure: the samples are frozen, so an audio value is immutable and may be shared freely.
case class Audio
  ( private[cacophony] val format: jss.AudioFormat, private[cacophony] val data: Array[Byte]^{} )
extends Formal, Domainal, scala.caps.Pure:
  audio =>

  def channels: Int      = format.getChannels
  def bitsPerSample: Int = format.getSampleSizeInBits
  def frames: Long       = data.length.toLong/format.getFrameSize

  def sampleRate: Quantity[Seconds[-1]] = format.getSampleRate.toDouble*Hertz
  def duration: Quantity[Seconds[1]]    = (frames.toDouble/format.getSampleRate)*Second

  def apply(channel: Int, frame: Int): Int =
    val bytesPerSample = format.getSampleSizeInBits/8
    val frameSize      = format.getFrameSize
    val offset         = frame*frameSize + channel*bytesPerSample
    val bigEndian      = format.isBigEndian
    val signed         = format.getEncoding == jss.AudioFormat.Encoding.PCM_SIGNED

    var value = 0

    if bigEndian then
      var i = 0

      while i < bytesPerSample do
        value = (value << 8) | (data.readUnchecked(offset + i) & 0xff)
        i += 1
    else
      var i = bytesPerSample - 1

      while i >= 0 do
        value = (value << 8) | (data.readUnchecked(offset + i) & 0xff)
        i -= 1

    if signed && bytesPerSample < 4 then
      val shift = 32 - 8*bytesPerSample
      (value << shift) >> shift
    else
      value

  def to[form: Audible as audible]: Audio in form across audio.Domain =
    new Audio(format, data):
      type Form   = form
      type Domain = audio.Domain
