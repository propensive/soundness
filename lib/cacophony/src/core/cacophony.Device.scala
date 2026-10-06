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
import quantitative.*
import symbolism.*
import vacuous.*

// What a `Feed` and an `Outlet` share: a mixer the system describes, and the lines of one kind it
// offers — target lines to record from, or source lines to play to.
trait Device:
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
    jss.AudioFormat(format, sampleRate, bits, cl.channels, cl.channels*(bits/8), sampleRate, false)
