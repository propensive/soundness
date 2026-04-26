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

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTraces

object Tests extends Suite(m"Cacophony Tests"):

  // 1-channel, 16-bit signed little-endian PCM at 8 kHz, 4 frames
  // sample values: 100, 200, 300, 400
  val wav = hex"""524946462c00000057415645666d74201000000001000100401f0000803e00000200100064
                  617461080000006400c8002c019001"""

  // header is malformed beyond a plausible RIFF prefix
  val broken = hex"5249464600000000ffffffff"

  def run(): Unit =
    test(m"Read a WAV's channel count"):
      wav.read[Audio in Wave].channels
    . assert(_ == 1)

    test(m"Read a WAV's frame count"):
      wav.read[Audio in Wave].frames
    . assert(_ == 4L)

    test(m"Read a WAV's bits-per-sample"):
      wav.read[Audio in Wave].bitsPerSample
    . assert(_ == 16)

    test(m"Read a WAV's sample rate"):
      wav.read[Audio in Wave].sampleRate
    . assert(_ == 8000.0*Hertz)

    test(m"Read a WAV's duration"):
      wav.read[Audio in Wave].duration
    . assert(_ == (4.0/8000.0)*Second)

    test(m"Read a WAV's first sample"):
      wav.read[Audio in Wave].apply(0, 0)
    . assert(_ == 100)

    test(m"Read a WAV's third sample"):
      wav.read[Audio in Wave].apply(0, 2)
    . assert(_ == 300)

    test(m"Read a WAV as AIFF fails"):
      capture[AudioError](wav.read[Audio in Aiff])
    . assert(_ == AudioError(Aiff()))

    test(m"Read malformed audio fails"):
      capture[AudioError](broken.read[Audio in Wave])
    . assert(_ == AudioError(Wave()))

    test(m"Convert a WAV to AIFF preserves frame count"):
      val aiff = wav.read[Audio in Wave].to[Aiff].read[Data]
      aiff.read[Audio in Aiff].frames
    . assert(_ == 4L)

    test(m"Convert a WAV to AIFF preserves the first sample"):
      val aiff = wav.read[Audio in Wave].to[Aiff].read[Data]
      aiff.read[Audio in Aiff].apply(0, 0)
    . assert(_ == 100)

    test(m"Convert a WAV to AIFF preserves the third sample"):
      val aiff = wav.read[Audio in Wave].to[Aiff].read[Data]
      aiff.read[Audio in Aiff].apply(0, 2)
    . assert(_ == 300)

    test(m"Round-trip WAV through AU preserves frame count"):
      val au = wav.read[Audio in Wave].to[Au].read[Data]
      au.read[Audio in Au].frames
    . assert(_ == 4L)

    test(m"Monaural channel layout reports 1 channel"):
      summon[Monaural is ChannelLayout].channels
    . assert(_ == 1)

    test(m"Stereo channel layout reports 2 channels"):
      summon[Stereo is ChannelLayout].channels
    . assert(_ == 2)

    test(m"Surround[6] channel layout reports 6 channels"):
      summon[Surround[6] is ChannelLayout].channels
    . assert(_ == 6)

    test(m"Audio.to preserves frame count"):
      wav.read[Audio in Wave].to[Aiff].frames
    . assert(_ == 4L)

    test(m"Feed.list does not throw"):
      Feed.list.length
    . assert(_ >= 0)

    Feed.list.headOption.foreach: feed =>
      if feed.supports[Monaural](8000.0*Hertz, 16) then
        test(m"Recording from a feed produces audio chunks"):
          val recording = feed.record[Monaural](8000.0*Hertz, 16, chunkBytes = 1024)
          val chunk: Audio across Monaural = recording.stream.head
          recording.stop()
          chunk.channels
        . assert(_ == 1)
