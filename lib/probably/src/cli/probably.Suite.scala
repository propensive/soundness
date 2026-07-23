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
package probably

import java.lang as jl

import ambience.*, environments.javaEnvironment
import anticipation.*
import contingency.*
import digression.*
import escapade.*
import fulminate.*
import gossamer.*
import iridescence.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*

import luminosity.darkBrightness
import termcaps.environmentTermcap
import themes.solarizedTheme

abstract class Suite(suiteName: Message) extends Testable(suiteName):
  val suiteIo = safely(stdios.virtualMachineStdio).vouch

  private def makeRunner(selection: Selection): Runner[Report] =
    given stdio: Stdio = suiteIo

    given palette: (theme: Theme) => TestPalette = new TestPalette:
      val yellow:      Color in Srgb = theme.spectrum.yellow.to[Srgb]
      val red:         Color in Srgb = theme.spectrum.red.to[Srgb]
      val blue:        Color in Srgb = theme.spectrum.blue.to[Srgb]

      def warning:     Color in Srgb = yellow
      def critical:    Color in Srgb = theme.spectrum.magenta.to[Srgb]
      def benchmark:   Color in Srgb = theme.spectrum.cyan.to[Srgb]
      def mixed:       Color in Srgb = blue
      def informative: Color in Srgb = blue
      def cold:        Color in Srgb = mix(yellow, red, 0.2)
      def warm:        Color in Srgb = mix(yellow, red, 0.5)
      def hot:         Color in Srgb = mix(yellow, red, 0.8)
      def accented:    Color in Srgb = theme.spectrum.cyan.to[Srgb]
      def highlight:   Color in Srgb = accent(yellow)
      def pass:        Color in Srgb = theme.spectrum.green.to[Srgb]
      def fail:        Color in Srgb = red

      def aspirePass:  Color in Srgb =
        mix(theme.spectrum.green.to[Srgb], theme.spectrum.cyan.to[Srgb], 0.5)

      def aspireFail:  Color in Srgb = subdue(yellow, 0.5)
      def detail:      Color in Srgb = blue
      def background:  Color in Srgb = theme.background.to[Srgb]
      def foreground:  Color in Srgb = theme.foreground.to[Srgb]
      def subdued:     Color in Srgb = subdue(theme.foreground.to[Srgb], 0.5)
      def unaccented:  Color in Srgb = subdued
      def positive:    Color in Srgb = pass
      def negative:    Color in Srgb = fail

    try Runner(selection) catch case error: EnvironmentError =>
      jl.System.out.nn.println(StackTrace(error).teletype.render)
      ???

  var runner0: Runner[Report] = makeRunner(Selection.all)

  // An alias given is memoized on first use, which is safe here only because `main`
  // replaces `runner0` with a selection-aware runner before anything summons it.
  given runner: Runner[Report] = runner0

  // A pure `Testable` view of this suite rather than `this`: the suite itself captures its
  // runner and deferred test blocks, so it is a capability, which `Testable`'s pure type
  // (rightly) forbids. `Testable` equality is structural (name and parent), so the view is
  // interchangeable with the suite wherever tests are grouped or reported.
  private val testableView: Testable = Testable(suiteName)
  given testable: Testable = testableView

  def run(): Unit

  def apply()(using runner: Runner[Report]): Unit =
    runner0 = runner
    runner.suite(testableView, run())

  final def main(arguments: IArray[Text]): Unit =
    val selection = Selection.parse(arguments.to(List))
    if !arguments.isEmpty then runner0 = makeRunner(selection)

    if selection.listOnly then
      given stdio: Stdio = suiteIo

      try
        runner.suite(testableView, run())

        runner.listed.each: (id, kind) =>
          val path =
            def names(id: TestId): List[Text] =
              id.suite.let { suite => names(suite.id) }.or(Nil) :+ id.moniker.or(id.name.text)
            names(id).join(t"/")

          Out.println(t"${id.id}  ${kindName(kind)}  $path")

        jl.System.exit(0)
      catch case error: Throwable =>
        jl.System.out.nn.println(StackTrace(error).teletype.render)
        jl.System.exit(2)
    else
      try runner.suite(testableView, run())
      catch case error: Throwable =>
        runner.terminate(error)
        jl.System.exit(2)
      finally
        try
          if runner.admitted == 0 && !selection.trivial then
            given stdio: Stdio = suiteIo
            Out.println(t"No tests matched the selection.")

          runner.complete()
          if runner.report.passed then jl.System.exit(0) else jl.System.exit(1)
        catch case error: EnvironmentError =>
          jl.System.out.nn.println(StackTrace(error).teletype)
          jl.System.exit(3)

  private def kindName(kind: Entry.Kind): Text = kind match
    case Entry.Kind.Check   => t"test"
    case Entry.Kind.Bench   => t"bench"
    case Entry.Kind.Stress  => t"stress"
    case Entry.Kind.Profile => t"profile"
