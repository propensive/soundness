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
import gesticulate.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

trait Audible extends Typeclass:
  audible: Audible =>
    def name: Text
    def mediaType: MediaType

    def read[input: Streamable by Data over Credit](source: input)
    :   Audio in Self raises Audio.Error =
      // `ByteArrayInputStream` only reads the array it wraps.
      val rawBytes: scala.Array[Byte] = Array.unsafeJvm(source.read[Data])

      val fileFormat: jss.AudioFileFormat =
        try jss.AudioSystem.getAudioFileFormat(ji.ByteArrayInputStream(rawBytes)).nn
        catch
          case _: jss.UnsupportedAudioFileException => abort(Audio.Error(this))
          case _: ji.IOException                    => abort(Audio.Error(this))

      if fileFormat.getType.nn.toString != name.s then abort(Audio.Error(this))

      val raw: jss.AudioInputStream =
        try jss.AudioSystem.getAudioInputStream(ji.ByteArrayInputStream(rawBytes)).nn
        catch
          case _: jss.UnsupportedAudioFileException => abort(Audio.Error(this))
          case _: ji.IOException                    => abort(Audio.Error(this))

      val encoding = raw.getFormat.nn.getEncoding.nn

      val pcm: jss.AudioInputStream =
        if encoding == jss.AudioFormat.Encoding.PCM_SIGNED ||
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
          catch case _: IllegalArgumentException => abort(Audio.Error(this))

      val readBytes = pcm.readAllBytes.nn
      val pcmFormat: jss.AudioFormat = pcm.getFormat.nn
      pcm.close()

      // The audio privately owns its sample array; the inline Java-side copy adapts to the
      // pure base class (a named array value would charge its read capability against it).
      scala.caps.unsafe.unsafeAssumePure:
        new Audio(pcmFormat, java.util.Arrays.copyOf(readBytes, readBytes.length).nn):
          type Form = audible.Self
