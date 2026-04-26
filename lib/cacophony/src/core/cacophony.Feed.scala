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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import contingency.*
import prepositional.*
import proscenium.*
import quantitative.*
import symbolism.*
import turbulence.*
import vacuous.*

object Feed:
  def list: List[Feed] =
    jss.AudioSystem.getMixerInfo.nn.toList.flatMap: info0 =>
      val info = info0.nn
      val mixer = jss.AudioSystem.getMixer(info).nn

      val canRecord = mixer.getTargetLineInfo.nn.exists:
        case dli: jss.DataLine.Info => dli.getLineClass == classOf[jss.TargetDataLine]
        case _                      => false

      if canRecord then List(Feed(info)) else Nil

case class Feed(private[cacophony] val mixerInfo: jss.Mixer.Info):
  def name:        Text = mixerInfo.getName.nn.tt
  def vendor:      Text = mixerInfo.getVendor.nn.tt
  def description: Text = mixerInfo.getDescription.nn.tt

  def configurations: List[Configuration] =
    val mixer = jss.AudioSystem.getMixer(mixerInfo).nn

    mixer.getTargetLineInfo.nn.toList.flatMap:
      case dli: jss.DataLine.Info if dli.getLineClass == classOf[jss.TargetDataLine] =>
        dli.getFormats.nn.toList.map: f0 =>
          val f = f0.nn

          val encoding =
            if f.getEncoding == jss.AudioFormat.Encoding.PCM_UNSIGNED then Encoding.PcmUnsigned
            else Encoding.PcmSigned

          val rate: Optional[Quantity[Seconds[-1]]] =
            if f.getSampleRate < 0 then Unset else f.getSampleRate.toDouble*Hertz

          Configuration(f.getChannels, rate, f.getSampleSizeInBits, encoding, f.isBigEndian)

      case _ => Nil

  def supports[layout: ChannelLayout as cl]
                (rate: Quantity[Seconds[-1]], bits: Int)
              : Boolean =
    val sampleRate = rate.value.toFloat
    val bytesPerFrame = cl.channels*(bits/8)

    val format = jss.AudioFormat
                  (jss.AudioFormat.Encoding.PCM_SIGNED,
                   sampleRate,
                   bits,
                   cl.channels,
                   bytesPerFrame,
                   sampleRate,
                   false)

    val mixer = jss.AudioSystem.getMixer(mixerInfo).nn
    mixer.isLineSupported(jss.DataLine.Info(classOf[jss.TargetDataLine], format))

  def record[layout: ChannelLayout as cl]
              (rate: Quantity[Seconds[-1]], bits: Int, chunkBytes: Int = 65536)
            : Recording across layout raises FeedError =

    val sampleRate    = rate.value.toFloat
    val bytesPerFrame = cl.channels*(bits/8)

    val format = jss.AudioFormat
                  (jss.AudioFormat.Encoding.PCM_SIGNED,
                   sampleRate,
                   bits,
                   cl.channels,
                   bytesPerFrame,
                   sampleRate,
                   false)

    val mixer = jss.AudioSystem.getMixer(mixerInfo).nn
    val info = jss.DataLine.Info(classOf[jss.TargetDataLine], format)

    if !mixer.isLineSupported(info)
    then abort(FeedError(name, FeedError.Reason.UnsupportedConfiguration))

    val line: jss.TargetDataLine =
      try mixer.getLine(info).nn.asInstanceOf[jss.TargetDataLine]
      catch case _: jss.LineUnavailableException =>
        abort(FeedError(name, FeedError.Reason.Unavailable))

    try line.open(format)
    catch case _: jss.LineUnavailableException =>
      abort(FeedError(name, FeedError.Reason.Unavailable))

    line.start()

    new Recording:
      type Domain = layout
      private var stopped = false

      def active: Boolean = !stopped

      def stop(): Unit =
        if !stopped then
          stopped = true
          line.stop()
          line.close()

      def stream: Stream[Audio across layout] =
        def recur: Stream[Audio across layout] =
          if stopped then Stream() else
            val buf: Array[Byte] = new Array[Byte](chunkBytes)
            val n = line.read(buf, 0, buf.length)

            if n <= 0 then Stream() else
              val chunk =
                if n == buf.length then buf
                else java.util.Arrays.copyOf(buf, n).nn

              Audio.of[layout](line.getFormat.nn, chunk) #:: recur

        Stream.defer(recur)
