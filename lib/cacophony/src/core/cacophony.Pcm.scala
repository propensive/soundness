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

import javax.sound.sampled as jss

import anticipation.*
import aperture.*
import contingency.*
import prepositional.*
import quantitative.*
import turbulence.*

// The form for scoped PCM audio-line access. A `Feed` (an input device) opens as
// `Pcm across layout` for capture -- `feed.open[Pcm across Stereo](Read, PcmFlag.Rate(48000))`
// -- providing a handle streaming `Audio` chunks for as long as the scope lasts; an `Outlet`
// (an output device) opens as plain `Pcm` for playback, whose `play` operation requires the
// `Write` grant and plays each `Audio` synchronously. In both cases the OS line is confined to
// the scope, released however it ends. The long-lived `Feed.record`/`Outlet.play` APIs remain
// for open-ended use.
trait Pcm extends Domainal

enum PcmFlag:
  case Rate(hertz: Int)
  case Bits(bits: Int)
  case Chunk(bytes: Int)

object Pcm:
  class PcmInput[layout] private[cacophony] (line: jss.TargetDataLine, chunkBytes: Int)
  extends caps.ExclusiveCapability:

    def stream: LazyList[Audio across layout] =
      def recur: LazyList[Audio across layout] =
        val buffer: Array[Byte] = new Array[Byte](chunkBytes)
        val count = line.read(buffer, 0, buffer.length)

        if count <= 0 then LazyList() else
          val chunk =
            if count == buffer.length then buffer
            else java.util.Arrays.copyOf(buffer, count).nn

          Audio.of[layout](line.getFormat.nn, chunk) #:: recur

      LazyList.defer(recur)

  class PcmOutput private[cacophony] (mixerInfo: jss.Mixer.Info, name: Text, chunkBytes: Int)
  extends caps.ExclusiveCapability:

    // Public: called from the grant-gated `play` extension, where a `private` member's
    // inline-accessor bridge would fail capture checking. Each call opens a line for the
    // audio's own format, plays it to completion, and closes it.
    def playAudio(audio: Audio)(using Tactic[OutletError]): Unit =
      val mixer = jss.AudioSystem.getMixer(mixerInfo).nn
      val info = jss.DataLine.Info(classOf[jss.SourceDataLine], audio.format)

      if !mixer.isLineSupported(info)
      then abort(OutletError(name, OutletError.Reason.UnsupportedConfiguration))

      val line: jss.SourceDataLine =
        try mixer.getLine(info).nn.asInstanceOf[jss.SourceDataLine]
        catch case _: jss.LineUnavailableException =>
          abort(OutletError(name, OutletError.Reason.Unavailable))

      try line.open(audio.format)
      catch case _: jss.LineUnavailableException =>
        abort(OutletError(name, OutletError.Reason.Unavailable))

      try
        line.start()
        val data = audio.data
        var offset = 0

        while offset < data.length do
          val length = math.min(chunkBytes, data.length - offset)
          val written = line.write(data, offset, length)
          if written <= 0 then offset = data.length else offset += written

        line.drain()
      finally
        line.stop()
        line.close()

  extension (output: (PcmOutput & Granting[Grant.Write])^)
    transparent inline def play(audio: Audio)(using Tactic[OutletError]): Unit =
      output.playAudio(audio)

  // Named classes rather than anonymous given instances, for the reasons documented on
  // galilei's `FileOpenable`.
  class FeedOpenable[layout: ChannelLayout as channelLayout](using Tactic[FeedError])
  extends Openable:

    type Self = Feed
    type Form = Pcm across layout
    type Operand = PcmFlag
    type Result = PcmInput[layout]

    def open[grants <: Grant, result]
      ( value: Feed, mode: Mode granting grants, flags: List[PcmFlag] )
      ( block: ((PcmInput[layout] & Granting[grants])^) ?=> result )
    :   result =

      val rate = flags.collectFirst { case PcmFlag.Rate(hertz) => hertz }.getOrElse(44100)
      val bits = flags.collectFirst { case PcmFlag.Bits(bits) => bits }.getOrElse(16)
      val chunk = flags.collectFirst { case PcmFlag.Chunk(bytes) => bytes }.getOrElse(65536)

      val bytesPerFrame = channelLayout.channels*(bits/8)

      val format =
        jss.AudioFormat
          ( jss.AudioFormat.Encoding.PCM_SIGNED,
            rate.toFloat,
            bits,
            channelLayout.channels,
            bytesPerFrame,
            rate.toFloat,
            false )

      val mixer = jss.AudioSystem.getMixer(value.mixerInfo).nn
      val info = jss.DataLine.Info(classOf[jss.TargetDataLine], format)

      if !mixer.isLineSupported(info)
      then abort(FeedError(value.name, FeedError.Reason.UnsupportedConfiguration))

      val line: jss.TargetDataLine =
        try mixer.getLine(info).nn.asInstanceOf[jss.TargetDataLine]
        catch case _: jss.LineUnavailableException =>
          abort(FeedError(value.name, FeedError.Reason.Unavailable))

      try line.open(format)
      catch case _: jss.LineUnavailableException =>
        abort(FeedError(value.name, FeedError.Reason.Unavailable))

      line.start()

      try block(using new PcmInput[layout](line, chunk) with Granting[grants] {})
      finally
        line.stop()
        line.close()

  class OutletOpenable(using Tactic[OutletError]) extends Openable:
    type Self = Outlet
    type Form = Pcm
    type Operand = PcmFlag
    type Result = PcmOutput

    def open[grants <: Grant, result]
      ( value: Outlet, mode: Mode granting grants, flags: List[PcmFlag] )
      ( block: ((PcmOutput & Granting[grants])^) ?=> result )
    :   result =

      val chunk = flags.collectFirst { case PcmFlag.Chunk(bytes) => bytes }.getOrElse(65536)
      block(using new PcmOutput(value.mixerInfo, value.name, chunk) with Granting[grants] {})

  given feedOpenable: [layout: ChannelLayout]
  =>  Tactic[FeedError]
  =>  ( FeedOpenable[layout]^ ) =
    FeedOpenable[layout]

  given outletOpenable: Tactic[OutletError] => ( OutletOpenable^ ) = OutletOpenable()
