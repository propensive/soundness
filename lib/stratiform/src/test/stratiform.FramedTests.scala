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
package stratiform

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import Tel.given

// `Framed` over an in-process `Duplex.pair()`, so no socket is opened: a request and its reply
// crossing between a `Framed from Reply to Request` and its mirror image, messages queued before
// any is read, and the two ways a record can be unreadable.
object FramedTests extends Suite(m"Stratiform framed duplex tests"):
  case class Request(id: Int, text: Text) derives CanEqual
  case class Reply(id: Int, length: Int) derives CanEqual

  def run(): Unit =
    test(m"a request and its reply cross a framed duplex"):
      val (near, far) = Duplex.pair()
      val client = near.framed[Reply, Request]
      val server = far.framed[Request, Reply]

      client.send(Request(1, t"hello"))
      val request = server.messages.next()
      server.send(Reply(request.id, request.text.length))
      (request, client.messages.next())
    . assert(_ == (Request(1, t"hello"), Reply(1, 5)))

    test(m"messages sent before any is read arrive in order"):
      val (near, far) = Duplex.pair()
      val client = near.framed[Reply, Request]
      val server = far.framed[Request, Reply]

      client.send(Request(1, t"one"))
      client.send(Request(2, t"two"))
      client.send(Request(3, t"three"))
      client.close()

      server.messages.to(List)
    . assert(_ == List(Request(1, t"one"), Request(2, t"two"), Request(3, t"three")))

    test(m"a symmetric protocol frames the same type both ways"):
      val (near, far) = Duplex.pair()
      val left = near.framed[Request, Request]
      val right = far.framed[Request, Request]

      left.send(Request(1, t"ping"))
      right.send(right.messages.next().copy(text = t"pong"))
      left.messages.next()
    . assert(_ == Request(1, t"pong"))

    test(m"a truncated record raises a framing error"):
      val (near, far) = Duplex.pair()
      val server = far.framed[Request, Reply]

      // A length prefix promising nine bytes, followed by two and the end of the stream.
      near.send(zephyrine.Stream(Data(0, 0, 0, 9, 1, 2)))
      near.close()

      capture[Framing.Error](server.messages.next()).reason
    . assert(_ == Framing.Error.Reason.ShortRead)

    test(m"a record that is not the expected message raises a decoding error"):
      val (near, far) = Duplex.pair()
      val server = far.framed[Request, Reply]

      // A well-framed record whose body is not a BinTEL `Request`.
      near.send(zephyrine.Stream(LengthPrefix.encode(Data(-1, -1, -1, -1))))
      near.close()

      try
        server.messages.next()
        t"decoded"
      catch
        case error: Bintel.Error => t"bintel"
        case error: Tel.Error    => t"tel"
    . assert(_ != t"decoded")
