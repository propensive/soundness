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
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package scintillate

import anticipation.*
import contingency.*, strategies.throwUnsafely
import eucalyptus.*, logging.silentLogging
import gossamer.*
import hieroglyph.charEncoders.utf8Encoder
import jacinta.*, formatting.compactJsonFormatting, servables.jsonServable
import parasite.*, probates.awaitProbate
import proscenium.*
import rudiments.*
import telekinesis.*
import vacuous.*
import zephyrine.memoize
import webserverErrorPages.minimalErrorPage

// Scintillate's three servers — the `Reactor` event loop and `SocketServer` on virtual and
// on platform threads — all running the same handler.
object RivalScintillate:
  case class Greeting(message: Text)

  val large: Data = HttpWorkload.largeBody.snapshot
  val requestId: Text = HttpWorkload.requestIdHeader.tt.lower

  val handler: Http.Connection ?=> Http.Response =
    request.location match
      case t"/bench" => Http.Response(Http.Ok)(t"Hello, World!")
      case t"/json"  => Http.Response(Http.Ok)(Greeting(t"Hello, World!").in[Json])
      case t"/large" => Http.Response(Http.Ok)(large)
      case t"/echo"  => Http.Response(Http.Ok)(request.body().memoize)

      case location if location.starts(t"/user/") =>
        val value = request.textHeaders.filter(_.key.lower == requestId).prim.lay(t"")(_.value)
        Http.Response(Http.Ok)(t"${location.skip(6)}:$value")

      case _ =>
        Http.Response(Http.NotFound)(t"")

  // The `Threading` in force here selects the kind of thread `SocketServer`'s
  // per-connection daemons run on — independent of the harness workers' threading.
  // The launcher thread is virtual, hence a daemon: it never obstructs JVM exit.
  private def socketServer(server: Int)(using Threading): Unit =
    Thread.ofVirtual.nn.start: () =>
      supervise:
        val service = SocketServer(HttpRivals.port(server)).handle(handler)
        HttpRivals.forever.await()
        service.cancel()

    HttpRivals.ready(server)

  lazy val virtual: Unit =
    socketServer(HttpRivals.SocketServerVirtual)(using threading.virtualThreading)

  lazy val platform: Unit =
    socketServer(HttpRivals.SocketServerPlatform)(using threading.platformThreading)

  // The event-loop front-end: handlers inline on the selector lanes.
  lazy val reactor: Unit =
    Reactor(HttpRivals.port(HttpRivals.Reactor))(handler)
    HttpRivals.ready(HttpRivals.Reactor)
