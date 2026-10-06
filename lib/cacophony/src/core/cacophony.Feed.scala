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
import contingency.*
import prepositional.*
import quantitative.*
import fulminate.*

object Feed:
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
            val buf: scala.Array[Byte] = new scala.Array[Byte](chunkBytes)
            val n = line.read(buf, 0, buf.length)

            if n <= 0 then Chain() else
              val chunk =
                if n == buf.length then buf else java.util.Arrays.copyOf(buf, n).nn

              Audio.of[layout](line.getFormat.nn, chunk) #:: recur

        Chain.defer(recur)
