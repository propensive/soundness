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

// Cask, Li Haoyi's Flask-style framework, with upickle; like its own `cask.Main`, it runs its
// routes on Undertow (an instance of its own, separate from the plain Undertow rival).
object RivalCask:
  case class Greeting(message: String) derives upickle.default.Writer

  object Routes extends cask.Routes:
    private val octetStream = Seq("Content-Type" -> "application/octet-stream")

    @cask.get("/bench")
    def bench(): String = HttpWorkload.hello

    @cask.get("/json")
    def json(): cask.Response[String] =
      cask.Response
        ( upickle.default.write(Greeting(HttpWorkload.hello)),
          headers = Seq("Content-Type" -> "application/json") )

    @cask.get("/large")
    def large(): cask.Response[Array[Byte]] =
      cask.Response(HttpWorkload.largeBody, headers = octetStream)

    @cask.post("/echo")
    def echo(request: cask.Request): cask.Response[Array[Byte]] =
      cask.Response(request.bytes, headers = octetStream)

    @cask.get("/user/:id")
    def user(id: String, request: cask.Request): String =
      val requestId = request.headers.get(HttpWorkload.requestIdHeader.toLowerCase.nn)
      s"$id:${requestId.flatMap(_.headOption).getOrElse("")}"

    initialize()

  object App extends cask.Main:
    val allRoutes = Seq(Routes)

  lazy val server: Unit =
    io.undertow.Undertow.builder().nn
    . addHttpListener(HttpRivals.port(HttpRivals.Cask), "127.0.0.1").nn
    . setHandler(App.defaultHandler).nn
    . build().nn
    . start()

    HttpRivals.ready(HttpRivals.Cask)
