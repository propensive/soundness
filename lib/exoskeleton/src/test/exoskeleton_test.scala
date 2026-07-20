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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package exoskeleton

import soundness.*

import classloaders.systemClassloader
import environments.javaEnvironment
import systems.javaSystem
import temporaryDirectories.systemTemporaryDirectory
import workingDirectories.defaultWorkingDirectory
import logging.silentLogging
import threading.platformThreading

import strategies.throwUnsafely
import backstops.silentBackstop

import Shell.*

import filesystemBackends.virtualMachine

object Tests extends Suite(m"Exoskeleton Tests"):
  def run(): Unit =
    CaptureTests()

    supervise:
      val foo: Text = "hello"
      Enclave(t"abcd").dispatch:
        ' {
            import executives.completions
            import interpreters.posixInterpreter

            val Alpha = Subcommand("alpha", e"a command to run")
            val Beta = Subcommand("beta", e"another command to run")
            val Gamma = Subcommand("gamma", e"a third command to run", hidden = true)
            val Distribution = Subcommand("distribution", e"a different command to run")
            val RedHat = Subcommand("red hat", e"Red Hat Linux")
            val Ubuntu = Subcommand("ubuntu", e"Ubuntu")
            val Gentoo = Subcommand("gentoo", e"Gentoo Linux")
            val Tree = Subcommand("tree", e"path-segment completion", hidden = true)

            class Hue(name: Text)
            class Segment(name: Text)

            cli:
              arguments match
                case Alpha() :: _ => execute(Exit.Ok)
                case Beta() :: _  => execute(Exit.Ok)

                case Gamma() :: _ =>
                  given Hue is Discoverable = _ => List(t"red", t"green", t"blue").map(Suggestion(_))
                  given Hue is Interpretable =
                    case argument :: Nil => Hue(argument())
                    case _               => Hue(t"unknown")

                  Flag[Hue]("colors", repeatable = true, aliases = List('c'), description = "multiple reds, greens or blues")()
                  execute(Exit.Ok)

                case Distribution() :: distribution =>
                  distribution match
                    case RedHat() :: _ =>
                      Flag("one", description = t"there is only one")()
                      execute(Exit.Ok)
                    case Ubuntu() :: _ =>
                      Flag("one", description = t"the first one")()
                      Flag("two", description = t"the second one")()
                      execute(Exit.Ok)
                    case Gentoo() :: _ =>
                      given Hue is Discoverable = _ => List(t"red", t"green", t"blue").map(Suggestion(_))
                      given Hue is Interpretable =
                        case argument :: Nil => Hue(argument())
                        case _               => Hue(t"unknown")

                      Flag[Hue]("color", aliases = List('f'), description = "red, green or blue")()
                      execute(Exit.Ok)

                    case _             => execute(Exit.Ok)

                case Tree() :: _ =>
                  given Segment is Discoverable = _ => List(Suggestion(t"src/", incomplete = true))
                  given Segment is Interpretable =
                    case argument :: Nil => Segment(argument())
                    case _               => Segment(t"")

                  Flag[Segment]("at", description = t"path segment")()
                  execute(Exit.Ok)

                case _ =>
                  execute(Exit.Fail(1))

            t"finished"
          }

      . sandbox:
          // Warmup runs to avoid timing issues in CI. A missing shell binary on the host
          // should not abort the suite — individual tests will surface a `TmuxError`.
          safely(Bash.tmux()(Tmux.completions(t"")))
          safely(Zsh.tmux()(Tmux.completions(t"")))
          safely(Fish.tmux(width = 120)(Tmux.completions(t"")))
          safely(Powershell.tmux()(Tmux.completions(t"")))

          test(m"Test subcommands on bash"):
            Bash.tmux()(Tmux.completions(t""))
          . assert(_ == t"alpha         beta          distribution")

          test(m"Test subcommands on zsh"):
            Zsh.tmux()(Tmux.completions(t""))
          . assert(_ == t"alpha          -- a command to run\nbeta           -- another command to run\ndistribution   -- a different command to run")

          test(m"Test subcommands on fish"):
            Fish.tmux(width = 120)(Tmux.completions(t""))
          . assert(_ == t"alpha  (a command to run)  beta  (another command to run)  distribution  (a different command to run)")

          test(m"Test subcommands with spaces on bash"):
            Bash.tmux()(Tmux.completions(t"distribution "))
          . assert(_ == t"gentoo   red hat  ubuntu")

          test(m"Test subcommands with spaces on zsh"):
            Zsh.tmux()(Tmux.completions(t"distribution "))
          . assert(_ == t"gentoo    -- Gentoo Linux\nred hat   -- Red Hat Linux\nubuntu    -- Ubuntu")

          test(m"Test subcommands with spaces on fish"):
            Fish.tmux(width = 120)(Tmux.completions(t"distribution "))
          . assert(_ == t"gentoo  (Gentoo Linux)  red hat  (Red Hat Linux)  ubuntu  (Ubuntu)")

          test(m"Test subcommands on powershell"):
            Powershell.tmux()(Tmux.completions(t""))
          . assert(_ == t"alpha  (a command to run)  beta  (another command to run)  distribution  (a different command to run)")

          test(m"Test subcommands with spaces on powershell"):
            Powershell.tmux()(Tmux.completions(t"distribution "))
          . assert(_ == t"gentoo  (Gentoo Linux)  red hat  (Red Hat Linux)  ubuntu  (Ubuntu)")

          test(m"Test flags on bash"):
            Bash.tmux()(Tmux.completions(t"distribution ubuntu "))
          . assert(_ == t"--one  --two")

          test(m"Test flags on fish"):
            Fish.tmux(width = 120)(Tmux.completions(t"distribution ubuntu "))
          . assert(_ == t"--one  (the first one)  --two  (the second one)")

          test(m"Test flags on powershell"):
            Powershell.tmux()(Tmux.completions(t"distribution ubuntu "))
          . assert(_ == t"--one  (the first one)  --two  (the second one)")

          test(m"Autocomplete progress for flag in Fish"):
            Fish.tmux(width = 120):
              Tmux.progress(t"distribution ubuntu ")
          . assert(_ == t"distribution ubuntu --^")

          test(m"Autocomplete progress for flag in Bash"):
            Bash.tmux():
              Tmux.progress(t"distribution ubuntu ")
          . assert(_ == t"distribution ubuntu --^")

          test(m"Autocomplete progress for flag in Zsh"):
            Zsh.tmux():
              Tmux.progress(t"distribution ubuntu ")
          . assert(_ == t"distribution ubuntu --^")

          test(m"Autocomplete progress for flag in Powershell"):
            Powershell.tmux():
              Tmux.progress(t"distribution ubuntu ")
          . assert(_ == t"distribution ubuntu --^")

          test(m"Test flags on zsh"):
            Zsh.tmux()(Tmux.completions(t"distribution ubuntu --"))
          . assert(_ == t"--one   -- the first one\n--two   -- the second one")

          test(m"Test capture 1"):
            summon[Enclave.Tool].completions:
              Zsh.tmux()(Tmux.completions(t"distribution ubuntu "))

          . assert()

          test(m"Test capture 2"):
            Zsh.tmux()(Tmux.completions(t"distribution "))

          . assert()

          test(m"flag parameter on zsh"):
            Zsh.tmux()(Tmux.completions(t"distribution gentoo --color "))
          . assert(_ == t"blue   green  red")

          test(m"flag parameter on bash"):
            Bash.tmux()(Tmux.completions(t"distribution gentoo --color "))
          . assert(_ == t"blue   green  red")

          test(m"flag parameter on fish"):
            Fish.tmux()(Tmux.completions(t"distribution gentoo --color "))
          . assert(_ == t"blue  green  red")

          test(m"flag parameter on zsh is not repeatable"):
            Zsh.tmux()(Tmux.progress(t"distribution gentoo --color red "))
          . assert(_ == t"distribution gentoo --color red ^")

          test(m"flag parameter on bash is not repeatable"):
            Bash.tmux()(Tmux.progress(t"distribution gentoo --color red "))
          . assert(_ == t"distribution gentoo --color red ^")

          test(m"flag parameter on fish is not repeatable"):
            Fish.tmux()(Tmux.progress(t"distribution gentoo --color red "))
          . assert(_ == t"distribution gentoo --color red ^")

          test(m"repeatable flag parameter on zsh is repeatable"):
            Zsh.tmux()(Tmux.progress(t"gamma --colors red "))
          . assert(_ == t"gamma --colors red --colors ^")

          test(m"repeatable flag parameter on bash is repeatable"):
            Bash.tmux()(Tmux.progress(t"gamma --colors red "))
          . assert(_ == t"gamma --colors red -^")

          test(m"repeatable flag parameter on fish is repeatable"):
            Fish.tmux()(Tmux.progress(t"gamma --colors red "))
          . assert(_ == t"gamma --colors red -^")

          test(m"flag parameter with `=` on zsh"):
            Zsh.tmux()(Tmux.completions(t"distribution gentoo --color="))
          . assert(_ == t"blue   green  red")

          test(m"flag parameter with `=` on bash"):
            Bash.tmux()(Tmux.completions(t"distribution gentoo --color="))
          . assert(_ == t"blue   green  red")

          test(m"flag parameter with `=` on fish"):
            Fish.tmux()(Tmux.completions(t"distribution gentoo --color="))
          . assert(_ == t"--color=blue  --color=green  --color=red")

          test(m"completion of flag parameter with `=` on zsh"):
            Zsh.tmux()(Tmux.progress(t"distribution gentoo --color=b"))
          . assert(_ == t"distribution gentoo --color=blue ^")

          test(m"completion of flag parameter with `=` on bash"):
            Bash.tmux()(Tmux.progress(t"distribution gentoo --color=b"))
          . assert(_ == t"distribution gentoo --color=blue ^")

          test(m"completion of flag parameter with `=` on fish"):
            Fish.tmux()(Tmux.progress(t"distribution gentoo --color=b"))
          . assert(_ == t"distribution gentoo --color=blue ^")

          test(m"short flag options on zsh"):
            Zsh.tmux()(Tmux.progress(t"distribution gentoo -"))
          . assert(_ == t"distribution gentoo -f ^")

          test(m"short flag options on fish"):
            Fish.tmux()(Tmux.completions(t"distribution gentoo -"))
          . assert(_ == t"-f  --color  (red, green or blue)")

          test(m"short flag options on bash"):
            Bash.tmux()(Tmux.completions(t"distribution gentoo -"))
          . assert(_ == t"--color  -f")

          test(m"flag options on zsh"):
            Zsh.tmux()(Tmux.progress(t"distribution gentoo --"))
          . assert(_ == t"distribution gentoo --color ^")

          test(m"flag options on fish"):
            Fish.tmux()(Tmux.progress(t"distribution gentoo --"))
          . assert(_ == t"distribution gentoo --color ^")

          test(m"flag options on bash"):
            Bash.tmux()(Tmux.progress(t"distribution gentoo --"))
          . assert(_ == t"distribution gentoo --color ^")

          test(m"completion of short flag parameter on zsh"):
            Zsh.tmux()(Tmux.progress(t"distribution gentoo -fb"))
          . assert(_ == t"distribution gentoo -fblue ^")

          test(m"completion of short flag parameter on bash"):
            Bash.tmux()(Tmux.progress(t"distribution gentoo -fb"))
          . assert(_ == t"distribution gentoo -fblue ^")

          test(m"completion of short flag parameter on fish"):
            Fish.tmux()(Tmux.progress(t"distribution gentoo -fb"))
          . assert(_ == t"distribution gentoo -fblue ^")

          test(m"flag parameter on powershell"):
            Powershell.tmux()(Tmux.completions(t"distribution gentoo --color "))
          . assert(_ == t"red  green  blue")

          test(m"flag parameter on powershell is not repeatable"):
            Powershell.tmux()(Tmux.progress(t"distribution gentoo --color red "))
          . assert(_ == t"distribution gentoo --color red ^")

          test(m"repeatable flag parameter on powershell is repeatable"):
            Powershell.tmux()(Tmux.progress(t"gamma --colors red "))
          . assert(_ == t"gamma --colors red -^")

          test(m"flag parameter with `=` on powershell"):
            Powershell.tmux()(Tmux.completions(t"distribution gentoo --color="))
          . assert(_ == t"--color=red  --color=green  --color=blue")

          test(m"completion of flag parameter with `=` on powershell"):
            Powershell.tmux()(Tmux.progress(t"distribution gentoo --color=b"))
          . assert(_ == t"distribution gentoo --color=blue ^")

          test(m"short flag options on powershell"):
            Powershell.tmux()(Tmux.completions(t"distribution gentoo -"))
          . assert(_ == t"--color  (red, green or blue)  -f  (red, green or blue)")

          test(m"flag options on powershell"):
            Powershell.tmux()(Tmux.progress(t"distribution gentoo --"))
          . assert(_ == t"distribution gentoo --color ^")

          test(m"completion of short flag parameter on powershell"):
            Powershell.tmux()(Tmux.progress(t"distribution gentoo -fb"))
          . assert(_ == t"distribution gentoo -fblue ^")

          suite(m"Admin commands"):
            val tool = summon[Enclave.Tool].path

            test(m"'{admin}' pid returns a positive integer"):
              sh"$tool '{admin}' pid".exec[Text]().trim.as[Int]
            .check(_ > 0)

            test(m"'{admin}' pid is stable across invocations"):
              val pid1 = sh"$tool '{admin}' pid".exec[Text]().trim
              val pid2 = sh"$tool '{admin}' pid".exec[Text]().trim
              pid1 == pid2
            .assert(_ == true)

            test(m"'{admin}' pid exits with status 0"):
              sh"$tool '{admin}' pid".exec[Exit]()
            .assert(_ == Exit.Ok)

            test(m"'{admin}' install exits with status 0"):
              sh"$tool '{admin}' install".exec[Exit]()
            .assert(_ == Exit.Ok)

            test(m"'{admin}' install output lines are existing files"):
              val output = sh"$tool '{admin}' install".exec[Text]()
              val paths = output.trim.lines.filter(_.length > 0)
              paths.all: path =>
                safely(path.as[Path on Local]).let(_.exists()).or(false)
            .assert(_ == true)

            test(m"'{admin}' kill terminates the daemon"):
              val pid = sh"$tool '{admin}' pid".exec[Text]().trim
              sh"$tool '{admin}' kill".exec[Unit]()
              snooze(0.2*Second)
              sh"kill -0 $pid".exec[Exit]()
            .assert(_ == Exit.Fail(1))

          suite(m"Raw completion invocation"):
            val tool = summon[Enclave.Tool].path

            test(m"completion with bash args returns alpha"):
              sh"$tool '{completions}' bash 1 0 /dev/null -- abcd ''".exec[Text]()
            .check(_.contains(t"alpha"))

            test(m"completion with zsh args returns alpha"):
              sh"$tool '{completions}' zsh 2 0 /dev/null -- abcd ''".exec[Text]()
            .check(_.contains(t"alpha"))

            test(m"completion with fish args returns alpha"):
              sh"$tool '{completions}' fish 1 0 /dev/null -- abcd ''".exec[Text]()
            .check(_.contains(t"alpha"))

            // Regression check for #1086 / #1116: mid-word completion in fish. Position
            // arguments match what fish 4.6 actually sends: `count (commandline --tokenize
            // --cut-at-cursor)` returns 2 (fish cuts *before* the partial — the partial is
            // not counted) and `commandline -C -t` returns 1 (non-zero indicates mid-word).
            test(m"fish mid-word completion suggests focused subcommand"):
              sh"$tool '{completions}' fish 2 1 /dev/null -- abcd distribution g".exec[Text]()
            .check(_.contains(t"gentoo"))

            // Regression check for #1086 part (2): `Suggestion.incomplete` on fish branch.
            // With the bug, output contained `src/` once; with the fix, it appears twice
            // (the second with a trailing space) to force fish's LCP no-trailing-space
            // behaviour for progressive completion.
            test(m"fish incomplete suggestion emits LCP duplicate"):
              sh"$tool '{completions}' fish 3 0 /dev/null -- abcd tree --at ".exec[Text]()
            .check(_.cut(t"\n").stdlib.count(_.starts(t"src/")) >= 2)

            // Regression check for #1109: with focus0 = 0 and position0 = 0 the
            // completions executive previously hit a `.get` on an empty Option in
            // `Completion.focusText`. With the `stackTrace` backstop in user
            // deployments that dumped the Java stack trace to stdout (which fish
            // then displayed as completion candidates); under the test suite's
            // `silent` backstop the daemon exits non-zero instead. Either way,
            // the focusText call should not throw, so the daemon should exit
            // cleanly.
            test(m"fish focus=0 exits cleanly"):
              sh"$tool '{completions}' fish 0 0 /dev/null -- abcd".exec[Exit]()
            .assert(_ == Exit.Ok)

      object HelpApp:
        import interpreters.posixInterpreter
        import stdios.muteStdio

        val Alpha = Subcommand(t"alpha", e"a command to run")
        val Beta = Subcommand(t"beta", e"another command to run")
        val Gamma = Subcommand(t"gamma", e"a hidden command", hidden = true)
        val Distribution = Subcommand(t"distribution", e"a different command to run")
        val Ubuntu = Subcommand(t"ubuntu", e"Ubuntu")
        val RedHat = Subcommand(t"red hat", e"Red Hat Linux")

        def app(using Cli): Execution = arguments match
          case Alpha() :: _ => execute(Exit.Ok)
          case Beta() :: _  => execute(Exit.Ok)
          case Gamma() :: _ => execute(Exit.Ok)

          case Distribution() :: rest => rest match
            case Ubuntu() :: _ =>
              Flag(t"one", description = t"the first one")()
              Flag(t"two", description = t"the second one")()
              execute(Exit.Ok)

            case RedHat() :: _ =>
              Flag(t"only", description = t"there is only one")()
              execute(Exit.Ok)

            case _ => execute(Exit.Ok)

          case _ => execute(Exit.Fail(1))

        lazy val tree: Help =
          helpTree
           (t"mytool",
            summon[Environment],
            summon[WorkingDirectory],
            summon[Stdio],
            Login(t"tester", Unset))
           (app)

      test(m"Help root lists visible subcommands"):
        HelpApp.tree.subcommands.map(_.command)
      .assert(_ == List(t"alpha", t"beta", t"distribution"))

      test(m"Help excludes hidden subcommands"):
        HelpApp.tree.subcommands.map(_.command).has(t"gamma")
      .assert(_ == false)

      test(m"Help descends into nested subcommands"):
        HelpApp.tree.subcommands.filter(_.command == t"distribution").bind: distribution =>
          distribution.subcommands.map(_.command)
      .assert(_ == List(t"red hat", t"ubuntu"))

      test(m"Help captures a leaf subcommand's flags"):
        HelpApp.tree.subcommands
         .filter(_.command == t"distribution").bind(_.subcommands)
         .filter(_.command == t"ubuntu").bind(_.parameters.map(_.name))
      .assert(_ == List(t"--one", t"--two"))

      test(m"Help renders as Printable text mentioning a subcommand"):
        summon[Help is Printable].print(HelpApp.tree, stdios.muteStdio.termcap)
      .assert(_.contains(t"alpha"))
