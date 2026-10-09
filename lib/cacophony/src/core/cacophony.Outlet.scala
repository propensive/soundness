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

import scala.math

import javax.sound.sampled as jss

import anticipation.*
import contingency.*
import fulminate.*

object Outlet:
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
      // [field-purity] stopped flag in anonymous Playback
      @scala.caps.unsafe.untrackedCaptures
      private var stopped = false
      private val data: Array[Byte]^{} = audio.data

      private val worker: Thread =
        val task: Runnable^{this} = () =>
          try
            // `SourceDataLine.write` only reads the samples.
            val samples = Array.unsafeJvm(data)
            var offset = 0

            while !stopped && offset < data.length do
              val len     = math.min(chunkBytes, data.length - offset)
              val written = line.write(samples, offset, len)
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
