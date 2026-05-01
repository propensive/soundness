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
package probably

import java.lang as jl

import ambience.*, environments.java
import anticipation.*
import contingency.*
import digression.*
import escapade.*
import fulminate.*
import iridescence.*
import prepositional.*
import turbulence.*
import vacuous.*

import luminosity.dark
import termcaps.environment
import themes.solarized

abstract class Suite(suiteName: Message) extends Testable(suiteName):
  val suiteIo = safely(stdios.virtualMachine).vouch

  var runner0: Runner[Report] =
    given stdio: Stdio = suiteIo

    given palette: (theme: Theme) => TestPalette = new Palette:
      type Form = Srgb
      val yellow:      Color in Srgb = theme.spectrum.yellow.in[Srgb]
      val red:         Color in Srgb = theme.spectrum.red.in[Srgb]
      val blue:        Color in Srgb = theme.spectrum.blue.in[Srgb]

      def warning:     Color in Srgb = yellow
      def critical:    Color in Srgb = theme.spectrum.magenta.in[Srgb]
      def benchmark:   Color in Srgb = theme.spectrum.cyan.in[Srgb]
      def mixed:       Color in Srgb = blue
      def informative: Color in Srgb = blue
      def cold:        Color in Srgb = mix(yellow, red, 0.2)
      def warm:        Color in Srgb = mix(yellow, red, 0.5)
      def hot:         Color in Srgb = mix(yellow, red, 0.8)
      def accented:    Color in Srgb = theme.spectrum.cyan.in[Srgb]
      def highlight:   Color in Srgb = accent(yellow)
      def pass:        Color in Srgb = theme.spectrum.green.in[Srgb]
      def fail:        Color in Srgb = red
      def aspirePass:  Color in Srgb =
        mix(theme.spectrum.green.in[Srgb], theme.spectrum.cyan.in[Srgb], 0.5)

      def aspireFail:  Color in Srgb = subdue(yellow, 0.5)
      def detail:      Color in Srgb = blue
      def background:  Color in Srgb = theme.background.in[Srgb]
      def foreground:  Color in Srgb = theme.foreground.in[Srgb]
      def subdued:     Color in Srgb = subdue(theme.foreground.in[Srgb], 0.5)

    try Runner() catch case error: EnvironmentError =>
      jl.System.out.nn.println(StackTrace(error).teletype.render)
      ???

  given runner: Runner[Report] = runner0
  given testable: Testable = this

  def run(): Unit

  def apply()(using runner: Runner[Report]): Unit =
    runner0 = runner
    runner.suite(this, run())

  final def main(arguments: IArray[Text]): Unit =
    try runner.suite(this, run())
    catch case error: Throwable =>
      runner.terminate(error)
      jl.System.exit(2)
    finally try
      runner.complete()
      if runner.report.passed then jl.System.exit(0) else jl.System.exit(1)
    catch case error: EnvironmentError =>
      jl.System.out.nn.println(StackTrace(error).teletype)
      jl.System.exit(3)
