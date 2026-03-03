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
import turbulence.*
import vacuous.*

import termcaps.environment
import themes.solarized
import luminance.dark

abstract class Suite(suiteName: Message) extends Testable(suiteName):
  val suiteIo = safely(stdios.virtualMachine).vouch

  var runner0: Runner[Report] =
    given stdio: Stdio = suiteIo

    given palette: (theme: Theme) => TestPalette = new Palette:
      def warning:     Color = theme.spectrum.yellow
      def critical:    Color = theme.spectrum.magenta
      def benchmark:   Color = theme.spectrum.cyan
      def mixed:       Color = theme.spectrum.blue
      def informative: Color = theme.spectrum.blue
      def cold:        Color = mix(theme.spectrum.yellow, theme.spectrum.red, 0.2)
      def warm:        Color = mix(theme.spectrum.yellow, theme.spectrum.red, 0.5)
      def hot:         Color = mix(theme.spectrum.yellow, theme.spectrum.red, 0.8)
      def accented:    Color = theme.spectrum.cyan
      def highlight:   Color = accent(theme.spectrum.yellow)
      def pass:        Color = theme.spectrum.green
      def fail:        Color = theme.spectrum.red
      def detail:      Color = theme.spectrum.blue
      def background:  Color = theme.background
      def foreground:  Color = theme.foreground
      def subdued:     Color = subdue(theme.foreground, 0.5)

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
