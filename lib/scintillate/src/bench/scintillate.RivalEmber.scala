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

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.comcast.ip4s.*
import io.circe.syntax.*
import org.http4s.*, org.http4s.dsl.io.*, org.http4s.implicits.*, org.http4s.circe.*
import org.typelevel.ci.CIString

// http4s ember, the FS2 ecosystem's server (built directly on `fs2.io.net`), with circe.
// It runs on the global `IORuntime`, exactly as its own users run it; the `Resource`
// finalizer is deliberately discarded, since JVM exit is teardown.
object RivalEmber:
  case class Greeting(message: String) derives io.circe.Encoder.AsObject

  private val requestId = CIString(HttpWorkload.requestIdHeader)

  lazy val server: Unit =
    val app = HttpRoutes.of[IO]:
      case GET -> Root / "bench"          => Ok(HttpWorkload.hello)
      case GET -> Root / "json"           => Ok(Greeting(HttpWorkload.hello).asJson)
      case GET -> Root / "large"          => Ok(HttpWorkload.largeBody)
      case request @ POST -> Root / "echo" => request.as[Array[Byte]].flatMap(Ok(_))

      case request @ GET -> Root / "user" / id =>
        Ok(s"$id:${request.headers.get(requestId).fold("")(_.head.value)}")

    org.http4s.ember.server.EmberServerBuilder.default[IO]
    . withHost(host"127.0.0.1")
    . withPort(Port.fromInt(HttpRivals.port(HttpRivals.Ember)).get)
    . withHttpApp(app.orNotFound)
    . build.allocated.unsafeRunSync()

    HttpRivals.ready(HttpRivals.Ember)
