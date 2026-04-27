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
import quantitative.*
import symbolism.*
import vacuous.*

object Outlet:
  def list: List[Outlet] =
    jss.AudioSystem.getMixerInfo.nn.toList.flatMap: info0 =>
      val info = info0.nn
      val mixer = jss.AudioSystem.getMixer(info).nn

      val canPlay = mixer.getSourceLineInfo.nn.exists:
        case dli: jss.DataLine.Info => dli.getLineClass == classOf[jss.SourceDataLine]
        case _                      => false

      if canPlay then List(Outlet(info)) else Nil

case class Outlet(private[cacophony] val mixerInfo: jss.Mixer.Info):
  def name:        Text = mixerInfo.getName.nn.tt
  def vendor:      Text = mixerInfo.getVendor.nn.tt
  def description: Text = mixerInfo.getDescription.nn.tt

  def configurations: List[Configuration] =
    val mixer = jss.AudioSystem.getMixer(mixerInfo).nn

    mixer.getSourceLineInfo.nn.toList.flatMap:
      case dli: jss.DataLine.Info if dli.getLineClass == classOf[jss.SourceDataLine] =>
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
    mixer.isLineSupported(jss.DataLine.Info(classOf[jss.SourceDataLine], format))

  def play(audio: Audio, chunkBytes: Int = 65536): Playback raises OutletError =
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

    line.start()

    new Playback:
      private var stopped = false
      private val data    = audio.data

      private val worker: Thread =
        val task: Runnable = () =>
          try
            var offset = 0
            while !stopped && offset < data.length do
              val len     = math.min(chunkBytes, data.length - offset)
              val written = line.write(data, offset, len)
              if written <= 0 then offset = data.length else offset += written

            if !stopped then line.drain()
          finally
            if !stopped then
              stopped = true
              line.stop()
              line.close()

        Thread.ofVirtual.nn.start(task).nn

      def active: Boolean = !stopped

      def stop(): Unit =
        if !stopped then
          stopped = true
          line.stop()
          line.flush()
          line.close()

      def await(): Unit = worker.join()
