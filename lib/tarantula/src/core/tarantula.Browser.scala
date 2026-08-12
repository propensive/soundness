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
package tarantula

import anticipation.*
import gossamer.*
import guillotine.*
import jacinta.*
import vacuous.*

import adversaria.name

// The `alwaysMatch` capability objects, one per driver family. Vendor extension keys contain a
// colon, so they are spelled with `@name[Json]` rather than as Scala identifiers.
private case class Arguments(args: List[Text])
private case class Named(browserName: Text)
private case class Edgium(browserName: Text, @name[Json](t"ms:edgeOptions") opts: Arguments)
private case class Gecko(browserName: Text, @name[Json](t"moz:firefoxOptions") opts: Arguments)

private case class Chromium
  ( browserName: Text, @name[Json](t"goog:chromeOptions") opts: Arguments )

object Browser:
  // The W3C driver programs, as data: adding a browser is a new case with a command and a name,
  // not a subclass with its own launch/stop pair. The process lifecycle is identical for all of
  // them, and belongs to the session rather than to the browser.
  enum Driver:
    case Chromedriver, Geckodriver, Safaridriver, Edgedriver

    // chromedriver and its Edge fork take `--port=N`; geckodriver and safaridriver take the port
    // as a separate argument.
    def command(port: Int): Command = this match
      case Chromedriver => sh"chromedriver --port=$port"
      case Edgedriver   => sh"msedgedriver --port=$port"
      case Geckodriver  => sh"geckodriver --port $port"
      case Safaridriver => sh"safaridriver --port $port"

    def browserName: Text = this match
      case Chromedriver => t"chrome"
      case Geckodriver  => t"firefox"
      case Safaridriver => t"safari"
      case Edgedriver   => t"MicrosoftEdge"

    // Chromium took `--headless=new` as the supported spelling when the old `--headless` became
    // an alias for it; Firefox has always taken a single-dashed `-headless`. Safari has no
    // headless mode at all, so asking for one is a no-op rather than an error.
    def headless: List[Text] = this match
      case Chromedriver | Edgedriver => List(t"--headless=new")
      case Geckodriver               => List(t"-headless")
      case Safaridriver              => Nil

  val Chrome: Browser = Browser(Driver.Chromedriver)
  val Firefox: Browser = Browser(Driver.Geckodriver)
  val Safari: Browser = Browser(Driver.Safaridriver)
  val Edge: Browser = Browser(Driver.Edgedriver)

// A browser to drive: which driver program to run, on which port, and what to ask of it. A pure
// value — no process, no connection, no session id — so it may be a `val`, shared between tests,
// and used for several sessions in turn. `Browser.Chrome.headless.session: session ?=> …` opens
// one.
case class Browser
  ( driver:    Browser.Driver,
    port:      Int = 4444,
    arguments: List[Text] = Nil,
    requested: Optional[Json] = Unset ):

  def on(port: Int): Browser = copy(port = port)
  def headless: Browser = copy(arguments = List.of(arguments.stdlib ++ driver.headless.stdlib))
  def arguing(more: Text*): Browser = copy(arguments = List.of(arguments.stdlib ++ more))

  // Replaces the generated capabilities wholesale, for the cases this type does not model:
  // proxies, timeouts, mobile emulation, a Selenium grid's own extensions.
  def requesting(capabilities: Json): Browser = copy(requested = capabilities)

  // The `POST /session` payload. W3C capabilities are an `alwaysMatch`/`firstMatch` pair, not the
  // JSON Wire Protocol's `desiredCapabilities`; only `alwaysMatch` is generated here, since
  // `firstMatch` exists to let one request suit several drivers and a `Browser` names exactly one.
  def capabilities: Json = requested.or:
    val always =
      val options = Arguments(arguments)

      driver match
        case Browser.Driver.Chromedriver => Chromium(driver.browserName, options).in[Json]
        case Browser.Driver.Edgedriver   => Edgium(driver.browserName, options).in[Json]
        case Browser.Driver.Geckodriver  => Gecko(driver.browserName, options).in[Json]
        case Browser.Driver.Safaridriver => Named(driver.browserName).in[Json]

    Json.make(capabilities = Json.make(alwaysMatch = always))
