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

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.apache.pekko.http.scaladsl.model.{ContentTypes, HttpEntity}
import org.apache.pekko.http.scaladsl.server.Directives.*
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

// Pekko HTTP (the Apache fork of Akka HTTP) with its routing DSL and spray-json, on a
// default actor system with logging switched off. spray-json's marshallers are imported only
// where the JSON route needs them: in scope for the whole route, its array format would claim
// the echo route's `as[Array[Byte]]` and reject the octet-stream body.
object RivalPekko:
  case class Greeting(message: String)
  given RootJsonFormat[Greeting] =
    import DefaultJsonProtocol.*
    jsonFormat1(Greeting.apply)

  private val config = com.typesafe.config.ConfigFactory.parseString:
    "pekko.loglevel = OFF\npekko.stdout-loglevel = OFF\npekko.log-dead-letters = off"

  lazy val server: Unit =
    given system: ActorSystem = ActorSystem("bench", config.withFallback(com.typesafe.config.ConfigFactory.load()))

    val route = concat
      ( path("bench")(get(complete(HttpWorkload.hello))),
        path("json"):
          import SprayJsonSupport.*
          get(complete(Greeting(HttpWorkload.hello))),

        path("large"):
          get(complete(HttpEntity(ContentTypes.`application/octet-stream`, HttpWorkload.largeBody))),

        path("echo"):
          post:
            entity(as[Array[Byte]]): bytes =>
              complete(HttpEntity(ContentTypes.`application/octet-stream`, bytes)),

        path("user" / Segment): id =>
          get:
            optionalHeaderValueByName(HttpWorkload.requestIdHeader): requestId =>
              complete(s"$id:${requestId.getOrElse("")}") )

    Http().newServerAt("127.0.0.1", HttpRivals.port(HttpRivals.PekkoHttp)).bind(route)
    HttpRivals.ready(HttpRivals.PekkoHttp)
