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

import filesystemBackends.virtualMachineFilesystem

// Statuses must be declared at the top level: capture checking rejects
// `value.type <: value.type | other.type` for singletons of method-local definitions
// (soundness#1811), which would stop the union of statuses from forming.
object CannotConnect extends Status(1, t"the server could not be reached")
object BadConfig extends Status(2, t"the configuration file was invalid")

object Tests extends Suite(m"Exoskeleton Tests"):
  def run(): Unit =
    CaptureTests()
    SettingTests()

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
            val Files = Subcommand("files", e"path completion", hidden = true)
            val Verify = Subcommand("verify", e"verify a file")

            class Hue(name: Text)
            class Segment(name: Text)
            class Node(name: Text)

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

                  given Node is Discoverable = _ => List(Suggestion(t"key.", incomplete = true))
                  given Node is Interpretable =
                    case argument :: Nil => Node(argument())
                    case _               => Node(t"")

                  Flag[Segment]("at", description = t"path segment")()
                  Flag[Node]("node", description = t"node prefix")()
                  execute(Exit.Ok)

                case Files() :: rest =>
                  import systems.javaSystem
                  given WorkingDirectory = summon[Cli].workingDirectory

                  rest match
                    case Verify() :: Pathname(file) :: _ => execute(Exit.Ok)
                    case Pathname(file) :: Nil           => execute(Exit.Ok)
                    case _                               => execute(Exit.Fail(1))

                case _ =>
                  execute(Exit.Fail(1))

            t"finished"
          }

      . sandbox:
          // Warmup runs to avoid timing issues in CI. A missing shell binary on the host
          // should not abort the suite — individual tests will surface a `Tmux.Error`.
          safely(scala.caps.unsafe.unsafeAssumeSeparate(Bash.tmux()(Tmux.completions(t""))))
          safely(scala.caps.unsafe.unsafeAssumeSeparate(Zsh.tmux()(Tmux.completions(t""))))
          safely(scala.caps.unsafe.unsafeAssumeSeparate(Fish.tmux(width = 120)(Tmux.completions(t""))))
          safely(scala.caps.unsafe.unsafeAssumeSeparate(Powershell.tmux()(Tmux.completions(t""))))

          test(m"Test subcommands on bash"):
            scala.caps.unsafe.unsafeAssumeSeparate(Bash.tmux()(Tmux.completions(t"")))
          . assert(_ == t"alpha         beta          distribution")

          test(m"Test subcommands on zsh"):
            scala.caps.unsafe.unsafeAssumeSeparate(Zsh.tmux()(Tmux.completions(t"")))
          . assert(_ == t"alpha          -- a command to run\nbeta           -- another command to run\ndistribution   -- a different command to run")

          test(m"Test subcommands on fish"):
            scala.caps.unsafe.unsafeAssumeSeparate(Fish.tmux(width = 120)(Tmux.completions(t"")))
          . assert(_ == t"alpha  (a command to run)  beta  (another command to run)  distribution  (a different command to run)")

          test(m"Test subcommands with spaces on bash"):
            scala.caps.unsafe.unsafeAssumeSeparate(Bash.tmux()(Tmux.completions(t"distribution ")))
          . assert(_ == t"gentoo   red hat  ubuntu")

          test(m"Test subcommands with spaces on zsh"):
            scala.caps.unsafe.unsafeAssumeSeparate(Zsh.tmux()(Tmux.completions(t"distribution ")))
          . assert(_ == t"gentoo    -- Gentoo Linux\nred hat   -- Red Hat Linux\nubuntu    -- Ubuntu")

          test(m"Test subcommands with spaces on fish"):
            scala.caps.unsafe.unsafeAssumeSeparate(Fish.tmux(width = 120)(Tmux.completions(t"distribution ")))
          . assert(_ == t"gentoo  (Gentoo Linux)  red hat  (Red Hat Linux)  ubuntu  (Ubuntu)")

          test(m"Test subcommands on powershell"):
            scala.caps.unsafe.unsafeAssumeSeparate(Powershell.tmux()(Tmux.completions(t"")))
          . assert(_ == t"alpha  (a command to run)  beta  (another command to run)  distribution  (a different command to run)")

          test(m"Test subcommands with spaces on powershell"):
            scala.caps.unsafe.unsafeAssumeSeparate(Powershell.tmux()(Tmux.completions(t"distribution ")))
          . assert(_ == t"gentoo  (Gentoo Linux)  red hat  (Red Hat Linux)  ubuntu  (Ubuntu)")

          test(m"Test flags on bash"):
            scala.caps.unsafe.unsafeAssumeSeparate(Bash.tmux()(Tmux.completions(t"distribution ubuntu ")))
          . assert(_ == t"--one  --two")

          test(m"Test flags on fish"):
            scala.caps.unsafe.unsafeAssumeSeparate(Fish.tmux(width = 120)(Tmux.completions(t"distribution ubuntu ")))
          . assert(_ == t"--one  (the first one)  --two  (the second one)")

          test(m"Test flags on powershell"):
            scala.caps.unsafe.unsafeAssumeSeparate(Powershell.tmux()(Tmux.completions(t"distribution ubuntu ")))
          . assert(_ == t"--one  (the first one)  --two  (the second one)")

          test(m"Autocomplete progress for flag in Fish"):
            scala.caps.unsafe.unsafeAssumeSeparate(Fish.tmux(width = 120)(Tmux.progress(t"distribution ubuntu ")))
          . assert(_ == t"distribution ubuntu --^")

          test(m"Autocomplete progress for flag in Bash"):
            scala.caps.unsafe.unsafeAssumeSeparate(Bash.tmux()(Tmux.progress(t"distribution ubuntu ")))
          . assert(_ == t"distribution ubuntu --^")

          test(m"Autocomplete progress for flag in Zsh"):
            scala.caps.unsafe.unsafeAssumeSeparate(Zsh.tmux()(Tmux.progress(t"distribution ubuntu ")))
          . assert(_ == t"distribution ubuntu --^")

          test(m"Autocomplete progress for flag in Powershell"):
            scala.caps.unsafe.unsafeAssumeSeparate(Powershell.tmux()(Tmux.progress(t"distribution ubuntu ")))
          . assert(_ == t"distribution ubuntu --^")

          test(m"Test flags on zsh"):
            scala.caps.unsafe.unsafeAssumeSeparate(Zsh.tmux()(Tmux.completions(t"distribution ubuntu --")))
          . assert(_ == t"--one   -- the first one\n--two   -- the second one")

          test(m"Test capture 1"):
            scala.caps.unsafe.unsafeAssumeSeparate:
              summon[Enclave.Tool].completions:
                scala.caps.unsafe.unsafeAssumeSeparate(Zsh.tmux()(Tmux.completions(t"distribution ubuntu ")))

          . assert()

          test(m"Test capture 2"):
            scala.caps.unsafe.unsafeAssumeSeparate(Zsh.tmux()(Tmux.completions(t"distribution ")))

          . assert()

          test(m"flag parameter on zsh"):
            scala.caps.unsafe.unsafeAssumeSeparate(Zsh.tmux()(Tmux.completions(t"distribution gentoo --color ")))
          . assert(_ == t"blue   green  red")

          test(m"flag parameter on bash"):
            scala.caps.unsafe.unsafeAssumeSeparate(Bash.tmux()(Tmux.completions(t"distribution gentoo --color ")))
          . assert(_ == t"blue   green  red")

          test(m"flag parameter on fish"):
            scala.caps.unsafe.unsafeAssumeSeparate(Fish.tmux()(Tmux.completions(t"distribution gentoo --color ")))
          . assert(_ == t"blue  green  red")

          test(m"flag parameter on zsh is not repeatable"):
            scala.caps.unsafe.unsafeAssumeSeparate(Zsh.tmux()(Tmux.progress(t"distribution gentoo --color red ")))
          . assert(_ == t"distribution gentoo --color red ^")

          test(m"flag parameter on bash is not repeatable"):
            scala.caps.unsafe.unsafeAssumeSeparate(Bash.tmux()(Tmux.progress(t"distribution gentoo --color red ")))
          . assert(_ == t"distribution gentoo --color red ^")

          test(m"flag parameter on fish is not repeatable"):
            scala.caps.unsafe.unsafeAssumeSeparate(Fish.tmux()(Tmux.progress(t"distribution gentoo --color red ")))
          . assert(_ == t"distribution gentoo --color red ^")

          test(m"repeatable flag parameter on zsh is repeatable"):
            scala.caps.unsafe.unsafeAssumeSeparate(Zsh.tmux()(Tmux.progress(t"gamma --colors red ")))
          . assert(_ == t"gamma --colors red --colors ^")

          test(m"repeatable flag parameter on bash is repeatable"):
            scala.caps.unsafe.unsafeAssumeSeparate(Bash.tmux()(Tmux.progress(t"gamma --colors red ")))
          . assert(_ == t"gamma --colors red -^")

          test(m"repeatable flag parameter on fish is repeatable"):
            scala.caps.unsafe.unsafeAssumeSeparate(Fish.tmux()(Tmux.progress(t"gamma --colors red ")))
          . assert(_ == t"gamma --colors red -^")

          test(m"flag parameter with `=` on zsh"):
            scala.caps.unsafe.unsafeAssumeSeparate(Zsh.tmux()(Tmux.completions(t"distribution gentoo --color=")))
          . assert(_ == t"blue   green  red")

          test(m"flag parameter with `=` on bash"):
            scala.caps.unsafe.unsafeAssumeSeparate(Bash.tmux()(Tmux.completions(t"distribution gentoo --color=")))
          . assert(_ == t"blue   green  red")

          test(m"flag parameter with `=` on fish"):
            scala.caps.unsafe.unsafeAssumeSeparate(Fish.tmux()(Tmux.completions(t"distribution gentoo --color=")))
          . assert(_ == t"--color=blue  --color=green  --color=red")

          test(m"completion of flag parameter with `=` on zsh"):
            scala.caps.unsafe.unsafeAssumeSeparate(Zsh.tmux()(Tmux.progress(t"distribution gentoo --color=b")))
          . assert(_ == t"distribution gentoo --color=blue ^")

          test(m"completion of flag parameter with `=` on bash"):
            scala.caps.unsafe.unsafeAssumeSeparate(Bash.tmux()(Tmux.progress(t"distribution gentoo --color=b")))
          . assert(_ == t"distribution gentoo --color=blue ^")

          test(m"completion of flag parameter with `=` on fish"):
            scala.caps.unsafe.unsafeAssumeSeparate(Fish.tmux()(Tmux.progress(t"distribution gentoo --color=b")))
          . assert(_ == t"distribution gentoo --color=blue ^")

          test(m"short flag options on zsh"):
            scala.caps.unsafe.unsafeAssumeSeparate(Zsh.tmux()(Tmux.progress(t"distribution gentoo -")))
          . assert(_ == t"distribution gentoo -f ^")

          test(m"short flag options on fish"):
            scala.caps.unsafe.unsafeAssumeSeparate(Fish.tmux()(Tmux.completions(t"distribution gentoo -")))
          . assert(_ == t"-f  --color  (red, green or blue)")

          test(m"short flag options on bash"):
            scala.caps.unsafe.unsafeAssumeSeparate(Bash.tmux()(Tmux.completions(t"distribution gentoo -")))
          . assert(_ == t"--color  -f")

          test(m"flag options on zsh"):
            scala.caps.unsafe.unsafeAssumeSeparate(Zsh.tmux()(Tmux.progress(t"distribution gentoo --")))
          . assert(_ == t"distribution gentoo --color ^")

          test(m"flag options on fish"):
            scala.caps.unsafe.unsafeAssumeSeparate(Fish.tmux()(Tmux.progress(t"distribution gentoo --")))
          . assert(_ == t"distribution gentoo --color ^")

          test(m"flag options on bash"):
            scala.caps.unsafe.unsafeAssumeSeparate(Bash.tmux()(Tmux.progress(t"distribution gentoo --")))
          . assert(_ == t"distribution gentoo --color ^")

          test(m"completion of short flag parameter on zsh"):
            scala.caps.unsafe.unsafeAssumeSeparate(Zsh.tmux()(Tmux.progress(t"distribution gentoo -fb")))
          . assert(_ == t"distribution gentoo -fblue ^")

          test(m"completion of short flag parameter on bash"):
            scala.caps.unsafe.unsafeAssumeSeparate(Bash.tmux()(Tmux.progress(t"distribution gentoo -fb")))
          . assert(_ == t"distribution gentoo -fblue ^")

          test(m"completion of short flag parameter on fish"):
            scala.caps.unsafe.unsafeAssumeSeparate(Fish.tmux()(Tmux.progress(t"distribution gentoo -fb")))
          . assert(_ == t"distribution gentoo -fblue ^")

          test(m"flag parameter on powershell"):
            scala.caps.unsafe.unsafeAssumeSeparate(Powershell.tmux()(Tmux.completions(t"distribution gentoo --color ")))
          . assert(_ == t"red  green  blue")

          test(m"flag parameter on powershell is not repeatable"):
            scala.caps.unsafe.unsafeAssumeSeparate(Powershell.tmux()(Tmux.progress(t"distribution gentoo --color red ")))
          . assert(_ == t"distribution gentoo --color red ^")

          test(m"repeatable flag parameter on powershell is repeatable"):
            scala.caps.unsafe.unsafeAssumeSeparate(Powershell.tmux()(Tmux.progress(t"gamma --colors red ")))
          . assert(_ == t"gamma --colors red -^")

          test(m"flag parameter with `=` on powershell"):
            scala.caps.unsafe.unsafeAssumeSeparate(Powershell.tmux()(Tmux.completions(t"distribution gentoo --color=")))
          . assert(_ == t"--color=red  --color=green  --color=blue")

          test(m"completion of flag parameter with `=` on powershell"):
            scala.caps.unsafe.unsafeAssumeSeparate(Powershell.tmux()(Tmux.progress(t"distribution gentoo --color=b")))
          . assert(_ == t"distribution gentoo --color=blue ^")

          test(m"short flag options on powershell"):
            scala.caps.unsafe.unsafeAssumeSeparate(Powershell.tmux()(Tmux.completions(t"distribution gentoo -")))
          . assert(_ == t"--color  (red, green or blue)  -f  (red, green or blue)")

          test(m"flag options on powershell"):
            scala.caps.unsafe.unsafeAssumeSeparate(Powershell.tmux()(Tmux.progress(t"distribution gentoo --")))
          . assert(_ == t"distribution gentoo --color ^")

          test(m"completion of short flag parameter on powershell"):
            scala.caps.unsafe.unsafeAssumeSeparate(Powershell.tmux()(Tmux.progress(t"distribution gentoo -fb")))
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
                safely(path.as[Path on Local]).let(_.existent()).or(false)
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

            // #1086 rendered `Suggestion.incomplete` on fish as a trailing-space twin of
            // every candidate, forcing fish's LCP no-trailing-space behaviour; #1783 showed
            // the twin doubles the visible menu. Fish already inserts a `/`-terminated
            // candidate without a trailing space, so a slash-terminated candidate must
            // appear exactly once...
            test(m"fish slash-terminated incomplete suggestion is not duplicated"):
              sh"$tool '{completions}' fish 3 0 /dev/null -- abcd tree --at ".exec[Text]()
            .assert(_.cut(t"\n").stdlib.count(_.starts(t"src/")) == 1)

            // ...and the twin survives only where fish needs it: a sole incomplete
            // candidate with no slash of its own.
            test(m"fish sole non-slash incomplete suggestion keeps its LCP twin"):
              sh"$tool '{completions}' fish 3 0 /dev/null -- abcd tree --node ".exec[Text]()
            .check(_.cut(t"\n").stdlib.count(_.starts(t"key.")) == 2)

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

          suite(m"Pathname completions"):
            import interfaces.paths.pathOnLinux

            val tool = summon[Enclave.Tool].path

            // A directory of known content, used as the working directory of each completion
            // invocation below so `Pathname`'s listings are deterministic.
            val fixture: Path on Linux = unsafely:
              val dir: Path on Linux = temporaryDirectory[Path on Linux]/t"exoskeleton-${Uuid()}"
              dir.create[Directory]()
              (dir/t"one.txt").create[File]()
              (dir/t"two.txt").create[File]()
              (dir/t"src").create[Directory]()
              dir

            given WorkingDirectory = () => fixture.encode

            // Issue #1783: the fish/powershell branch renders `Suggestion.incomplete` as a
            // second candidate line with a trailing space, so every path entry shows twice
            // in the menu, and selecting the twin inserts a stray trailing space.
            test(m"fish lists a file candidate exactly once"):
              sh"$tool '{completions}' fish 3 0 /dev/null -- abcd files verify ''".exec[Text]()
            .assert(_.cut(t"\n").stdlib.count(_.starts(t"one.txt")) == 1)

            test(m"fish lists a directory candidate exactly once"):
              sh"$tool '{completions}' fish 3 0 /dev/null -- abcd files verify ''".exec[Text]()
            .assert(_.cut(t"\n").stdlib.count(_.starts(t"src/")) == 1)

            test(m"powershell lists a file candidate exactly once"):
              val line = t"abcd files verify "
              sh"$tool '{completions}' powershell ${line.length} 0 '' -- $line".exec[Text]()
            .assert(_.cut(t"\n").stdlib.count(_.starts(t"one.txt")) == 1)

            // Issue #1783, `incomplete` too broad: `Pathname` marks every candidate that
            // differs from the argument as incomplete, so even plain files get zsh's
            // suffix-suppressing `compadd` twin; only directories should.
            test(m"zsh emits no suffix-suppressing twin for a plain file"):
              sh"$tool '{completions}' zsh 4 0 /dev/null -- abcd files verify ''".exec[Text]()
            .assert(_.cut(t"\n").stdlib.count(_.contains(t"one.txt")) == 1)

            test(m"zsh still emits the suffix-suppressing twin for a directory"):
              sh"$tool '{completions}' zsh 4 0 /dev/null -- abcd files verify ''".exec[Text]()
            .assert(_.cut(t"\n").stdlib.count(_.contains(t"src/")) == 2)

            // Issue #1782: `Pathname` discards the `prior` suggestions, so a subcommand and
            // a path matched against the same argument should offer the union, but the
            // extractor evaluated last erases the other's suggestions.
            test(m"subcommands and paths at one position combine on fish"):
              sh"$tool '{completions}' fish 2 0 /dev/null -- abcd files ''".exec[Text]()
            .assert { out => out.contains(t"verify") && out.contains(t"one.txt") }

            test(m"subcommands and paths at one position combine on bash"):
              sh"$tool '{completions}' bash 2 0 /dev/null -- abcd files ''".exec[Text]()
            .assert { out => out.contains(t"verify") && out.contains(t"one.txt") }

            test(m"subcommands and paths at one position combine on zsh"):
              sh"$tool '{completions}' zsh 3 0 /dev/null -- abcd files ''".exec[Text]()
            .assert { out => out.contains(t"verify") && out.contains(t"one.txt") }

            // Issue #1782, worst case: a partial word matching no file must still offer the
            // subcommand it matches, not an empty list.
            test(m"a partial word matching no file still offers the subcommand"):
              sh"$tool '{completions}' fish 2 3 /dev/null -- abcd files ver".exec[Text]()
            .assert(_.contains(t"verify"))

            // Issue #1086 guard, end-to-end: a unique directory candidate must complete
            // progressively — inserted without a trailing space, not advancing to the next
            // argument.
            test(m"fish inserts a unique directory without advancing"):
              scala.caps.unsafe.unsafeAssumeSeparate:
                Fish.tmux():
                  Tmux.enter(t"cd ${fixture.encode}")
                  Tmux.enter('\r')
                  Tmux.progress(t"files verify sr")
            .assert(_ == t"files verify src/^")

            test(m"fish menu shows each path entry once"):
              scala.caps.unsafe.unsafeAssumeSeparate:
                Fish.tmux(width = 120):
                  Tmux.enter(t"cd ${fixture.encode}")
                  Tmux.enter('\r')
                  Tmux.completions(t"files verify ")
            .assert { out => out.cut(t"one.txt").stdlib.length == 2 && out.cut(t"src/").stdlib.length == 2 }

      object HelpApp:
        import interpreters.posixInterpreter
        import stdios.muteStdio

        val admin = CommandGroup(t"Admin commands", t"Commands for user administration.")

        class Delay(text: Text)

        given Delay is Interpretable:
          override def placeholder: Optional[Text] = t"seconds"

          def interpret(arguments: List[Argument]): Optional[Delay] = arguments match
            case argument :: _ => Delay(argument())
            case _             => Unset

        val Alpha = Subcommand(t"alpha", e"a command to run")
        val Beta = Subcommand(t"beta", e"another command to run")
        val Gamma = Subcommand(t"gamma", e"a hidden command", hidden = true)
        val Distribution = Subcommand(t"distribution", e"a different command to run")
        val Ubuntu = Subcommand(t"ubuntu", e"Ubuntu")
        val RedHat = Subcommand(t"red hat", e"Red Hat Linux")
        val UserAdd = Subcommand(t"useradd", e"add a user account", group = admin)
        val UserDel = Subcommand(t"userdel", e"remove a user account", group = admin)

        def app(using cli: Cli): Execution =
          Flag(t"verbose", description = t"verbose output")()

          arguments match
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

            case UserAdd() :: _ =>
              Flag(t"home", description = t"specify the home directory")()
              Flag(t"groups", repeatable = true, description = t"add the user to a group")()
              Flag(t"force", description = t"do not ask for confirmation")()

              // Read outside the `execute` block, so a help-tree probe observes it.
              val config = cli.environment.variable(t"MYTOOL_CONFIG")

              execute:
                if config.absent then CannotConnect.exit else BadConfig.exit

            case UserDel() :: _ =>
              Flag(t"force", description = t"do not ask for confirmation")()
              Flag[Delay](t"wait", description = t"delay the deletion")()
              cli.environment.variable(t"MYTOOL_HOME")
              execute(CannotConnect.exit)

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
      .assert(_ == List(t"alpha", t"beta", t"distribution", t"useradd", t"userdel"))

      test(m"Flags checked before subcommand dispatch are global root parameters"):
        HelpApp.tree.parameters
      .assert(_ == List(Help.Param(t"--verbose", Nil, t"verbose output", false, true, t"value")))

      test(m"Operand placeholders are derived from the operand type"):
        HelpApp.tree.subcommands
         .filter(_.command == t"userdel").bind(_.parameters)
         .filter(_.name == t"--wait").map(_.operand)
      .assert(_ == List(t"seconds"))

      test(m"Global flags are not repeated in subcommand nodes"):
        HelpApp.tree.subcommands.bind(_.parameters).map(_.name).has(t"--verbose")
      .assert(_ == false)

      test(m"Subcommands carry their command group"):
        HelpApp.tree.subcommands.filter(_.command == t"useradd").map(_.group)
      .assert(_ == List(HelpApp.admin))

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

      test(m"Statuses a subcommand can return are discovered from its execute block"):
        HelpApp.tree.subcommands.filter(_.command == t"useradd").bind(_.statuses).map(_.code)
         .sort(identity)
      .assert(_ == List(1, 2))

      test(m"A single status is discovered without widening to Status"):
        HelpApp.tree.subcommands.filter(_.command == t"userdel").bind(_.statuses).map(_.code)
      .assert(_ == List(1))

      test(m"A command returning a plain Exit contributes no statuses"):
        HelpApp.tree.subcommands.filter(_.command == t"alpha").bind(_.statuses)
      .assert(_ == Nil)

      test(m"Status descriptions are carried through to the help tree"):
        HelpApp.tree.subcommands.filter(_.command == t"userdel").bind(_.statuses)
         .map(_.description)
      .assert(_ == List(t"the server could not be reached"))

      test(m"Environment variables read before execute are discovered"):
        HelpApp.tree.subcommands.filter(_.command == t"useradd").bind(_.variables)
      .assert(_.has(t"MYTOOL_CONFIG"))

      test(m"Environment variables are attributed to the command that reads them"):
        HelpApp.tree.subcommands.filter(_.command == t"alpha").bind(_.variables)
      .assert(_ == Nil)

      test(m"Help renders as Printable text mentioning a subcommand"):
        summon[Help is Printable].print(HelpApp.tree, stdios.muteStdio.termcap)
      .assert(_.contains(t"alpha"))

      test(m"Help wraps descriptions at the terminal width, aligned to the description column"):
        val narrow: Termcap = new Termcap:
          def ansi: Boolean = false
          def color: ColorDepth = ColorDepth.NoColor
          override def width: Int = 45

        summon[Help is Printable].print(HelpApp.tree, narrow).cut(t"\n")
      .assert: lines =>
        lines.has(t"    --force <value>      do not ask for")
          && lines.has(t"                         confirmation")

      test(m"Help renders in aligned man-page style"):
        summon[Help is Printable].print(HelpApp.tree, stdios.muteStdio.termcap)
      .assert:
        _.cut(t"\n") == List
              (t"Usage: mytool [--verbose <value>] <command> [options]",
               t"",
               t"Global options:",
               t"  --verbose <value>      verbose output",
               t"",
               t"Commands:",
               t"  alpha                  a command to run",
               t"  beta                   another command to run",
               t"",
               t"  distribution           a different command to run",
               t"",
               t"    red hat              Red Hat Linux",
               t"      --only <value>     there is only one",
               t"",
               t"    ubuntu               Ubuntu",
               t"      --one <value>      the first one",
               t"      --two <value>      the second one",
               t"",
               t"Admin commands:",
               t"  Commands for user administration.",
               t"",
               t"  useradd                add a user account",
               t"    --groups <value>...  add the user to a group",
               t"    --home <value>       specify the home directory",
               t"",
               t"  userdel                remove a user account",
               t"    --wait <seconds>     delay the deletion",
               t"",
               t"  Common options:",
               t"    --force <value>      do not ask for confirmation")

      suite(m"Manpage tests"):
        val manual = Manual
          ( version  = v"1.2.3",
            authors  = List(t"Jon Pretty"),
            examples = List(Manual.Example(t"Run the tool", t"demo --verbose")),
            seeAlso  = List(Manual.Reference(t"bash")),
            homepage = url"https://soundness.dev/" )

        val leafHelp = Help
          ( t"demo",
            t"a demonstration tool",
            List(Help.Param(t"--verbose", List(t"-v"), t"print more detail", false, false,
                t"value")),
            Nil )

        test(m"A leaf command renders as a complete manpage"):
          leafHelp.roff(using manual).serialize.cut(t"\n")
        . assert(_ == List
                       (t".TH \"DEMO\" \"1\" \"\" \"demo 1.2.3\" \"User Commands\"",
                        t".SH \"NAME\"",
                        t"demo \\- a demonstration tool",
                        t".SH \"SYNOPSIS\"",
                        t"\\fBdemo\\fP [\\-\\-verbose <value>]",
                        t".SH \"DESCRIPTION\"",
                        t"a demonstration tool",
                        t".SH \"OPTIONS\"",
                        t".TP",
                        t"\\fB\\-\\-verbose, \\-v <value>\\fP",
                        t"print more detail",
                        t".SH \"EXAMPLES\"",
                        t"Run the tool",
                        t".EX",
                        t"demo \\-\\-verbose",
                        t".EE",
                        t".SH \"AUTHORS\"",
                        t"Jon Pretty",
                        t".SH \"SEE ALSO\"",
                        t"\\fBbash\\fP(1)",
                        t".P",
                        t"Homepage: https://soundness.dev/",
                        t""))

        test(m"The discovered help tree renders global options and grouped commands"):
          HelpApp.tree.roff.serialize
        . assert: page =>
            page.contains(t".TH \"MYTOOL\" \"1\" \"\" \"\" \"User Commands\"")
              && page.contains(t".SH \"GLOBAL OPTIONS\"")
              && page.contains(t"\\fB\\-\\-verbose <value>\\fP")
              && page.contains(t".SS \"Admin commands\"")
              && page.contains(t"\\fBuseradd\\fP")

        test(m"Discovered statuses render as an EXIT STATUS section"):
          HelpApp.tree.roff.serialize
        . assert: page =>
            page.contains(t".SH \"EXIT STATUS\"")
              && page.contains(t"\\fB1\\fP")
              && page.contains(t"the server could not be reached")

        test(m"Discovered environment variables render as an ENVIRONMENT section"):
          HelpApp.tree.roff.serialize
        . assert: page =>
            page.contains(t".SH \"ENVIRONMENT\"")
              && page.contains(t"\\fBMYTOOL_CONFIG\\fP")
              && page.contains(t"\\fBMYTOOL_HOME\\fP")

        test(m"A hand-written description overrides a discovered status's"):
          val overridden =
            Manual(exitStatuses = List(Manual.ExitStatus(1, t"could not reach the daemon")))

          HelpApp.tree.roff(using overridden).serialize
        . assert: page =>
            page.contains(t"could not reach the daemon")
              && !page.contains(t"the server could not be reached")

        test(m"A hand-written environment description is attached to a discovered variable"):
          val described =
            Manual(environment = List(Manual.EnvironmentVariable(t"MYTOOL_HOME", t"the home dir")))

          HelpApp.tree.roff(using described).serialize.cut(t"\n")
        . assert(_.has(t"the home dir"))

        test(m"The manpage synopsis matches the help text usage line"):
          HelpApp.tree.roff.serialize.cut(t"\n")
        . assert(_.has(t"\\fBmytool\\fP [\\-\\-verbose <value>] <command> [options]"))
