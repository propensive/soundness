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

import java.io as ji
import javax.sound.sampled as jss

import anticipation.*
import contingency.*
import prepositional.*
import proscenium.*
import quantitative.*
import rudiments.*
import symbolism.*
import turbulence.*
import vacuous.*

object Audio:
  def apply[streamable: Streamable by Data](input: streamable): Audio raises AudioError =
    val rawBytes: Array[Byte] = input.read[Data].javaInputStream.readAllBytes.nn

    val raw: jss.AudioInputStream =
      try jss.AudioSystem.getAudioInputStream(ji.ByteArrayInputStream(rawBytes)).nn
      catch
        case _: jss.UnsupportedAudioFileException => abort(AudioError(Unset))
        case _: ji.IOException                    => abort(AudioError(Unset))

    val encoding = raw.getFormat.nn.getEncoding.nn

    val pcm: jss.AudioInputStream =
      if encoding == jss.AudioFormat.Encoding.PCM_SIGNED
        || encoding == jss.AudioFormat.Encoding.PCM_UNSIGNED
      then raw
      else
        val src = raw.getFormat.nn
        val target = jss.AudioFormat
                      (jss.AudioFormat.Encoding.PCM_SIGNED,
                       src.getSampleRate,
                       16,
                       src.getChannels,
                       src.getChannels*2,
                       src.getSampleRate,
                       false)

        try jss.AudioSystem.getAudioInputStream(target, raw).nn
        catch case _: IllegalArgumentException => abort(AudioError(Unset))

    val pcmBytes: Array[Byte] = pcm.readAllBytes.nn
    val pcmFormat: jss.AudioFormat = pcm.getFormat.nn
    pcm.close()
    new Audio(pcmFormat, pcmBytes)

  def apply[form: Audible as audible](format: jss.AudioFormat, data: Array[Byte]): Audio in form =
    new Audio(format, data):
      type Form = form

  given streamable: [form: Audible] => (Audio in form) is Streamable by Data = audio =>
    val ais = jss.AudioInputStream
               (ji.ByteArrayInputStream(audio.data),
                audio.format,
                audio.frames)

    val fileType = jss.AudioSystem.getAudioFileTypes.nn.find(_.toString == form.name.s).getOrElse:
      throw RuntimeException(s"unregistered audio file format: ${form.name.s}")

    val out = StreamOutputStream()
    jss.AudioSystem.write(ais, fileType, out)
    out.close()
    out.stream

  given abstractable: [format: Audible] => (Audio in format) is Abstractable:
    type Domain = HttpStreams
    type Result = HttpStreams.Content

    def genericize(audio: Audio in format): HttpStreams.Content =
      (format.mediaType.basic, audio.read[Stream[Data]])

  given aggregable: [format: Audible as audible] => Tactic[AudioError]
  =>  (Audio in format) is Aggregable by Data =

    audible.read(_)


  given aggregable2: Tactic[AudioError] => Audio is Aggregable by Data = Audio(_)

case class Audio
            (private[cacophony] val format: jss.AudioFormat,
             private[cacophony] val data:   Array[Byte])
extends Formal:

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
        value = (value << 8) | (data(offset + i) & 0xff)
        i += 1
    else
      var i = bytesPerSample - 1
      while i >= 0 do
        value = (value << 8) | (data(offset + i) & 0xff)
        i -= 1

    if signed && bytesPerSample < 4 then
      val shift = 32 - 8*bytesPerSample
      (value << shift) >> shift
    else value

  def to[form: Audible as audible]: Audio in form = Audio[form](format, data)
