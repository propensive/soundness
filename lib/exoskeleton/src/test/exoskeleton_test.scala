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
import soundness.collationOrdering
import soundness.collations.codepoints

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
    InterpreterTests()
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
            val Home = Subcommand("home", e"check a path against the home directory", hidden = true)

            class Hue(name: Text)
            class Segment(name: Text)
            class Node(name: Text)

            cli:
              arguments match
                case Alpha() :: _ => execute(Exit.Ok)
                case Beta() :: _  => execute(Exit.Ok)

                case Gamma() :: _ =>
                  given Hue is Discoverable = (_, _) => List(Suggestion(t"red"), Suggestion(t"green"), Suggestion(t"blue"))
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
                      given Hue is Discoverable = (_, _) => List(Suggestion(t"red"), Suggestion(t"green"), Suggestion(t"blue"))
                      given Hue is Interpretable =
                        case argument :: Nil => Hue(argument())
                        case _               => Hue(t"unknown")

                      Flag[Hue]("color", aliases = List('f'), description = "red, green or blue")()
                      execute(Exit.Ok)

                    case _             => execute(Exit.Ok)

                case Tree() :: _ =>
                  given Segment is Discoverable = (_, _) => List(Suggestion(t"src/", incomplete = true))
                  given Segment is Interpretable =
                    case argument :: Nil => Segment(argument())
                    case _               => Segment(t"")

                  given Node is Discoverable = (_, _) => List(Suggestion(t"key.", incomplete = true))
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
                    // Issue #1807: a leading `~` must name the home directory, so the exit
                    // status reports where the argument actually resolved to.
                    case Home() :: Pathname(file) :: _ =>
                      execute(if file.encode == Directories.homeText then Exit.Ok else Exit.Fail(2))

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

            // Issue #1807: a leading `~` was resolved as an ordinary relative name, so it
            // named a nonexistent `~` entry in the working directory instead of the home
            // directory, and offered no completions beneath it.
            test(m"a bare tilde resolves to the home directory"):
              sh"$tool files home '~'".exec[Exit]()
            .assert(_ == Exit.Ok)

            test(m"a tilde with a trailing slash resolves to the home directory"):
              sh"$tool files home '~/'".exec[Exit]()
            .assert(_ == Exit.Ok)

            test(m"tilde completions list the home directory and keep the tilde"):
              sh"$tool '{completions}' fish 3 0 /dev/null -- abcd files verify '~/'".exec[Text]()
            .assert: output =>
                val lines = output.cut(t"\n").stdlib.filter(!_.nil)
                lines.nonEmpty && lines.forall(_.starts(t"~/"))

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
            case Beta() :: rest => execute(if rest == Nil then CannotConnect else Exit.Fail(3))
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
                if config.absent then CannotConnect else BadConfig

            case UserDel() :: _ =>
              Flag(t"force", description = t"do not ask for confirmation")()
              Flag[Delay](t"wait", description = t"delay the deletion")()
              cli.environment.variable(t"MYTOOL_HOME")
              execute(CannotConnect)

            case _ => execute(Exit.Fail(1))

        lazy val tree: Help =
          helpTree
           (t"mytool",
            summon[Environment],
            summon[WorkingDirectory],
            summon[Stdio],
            Login(t"tester", Unset))
           (app)

        // Run the application for real (in invocation mode, not completion mode) and return the
        // spent invocation, so its recorded state can be inspected.
        def invoke(textArguments: Text*): Invocation =
          val invocation =
            Invocation
              (Cli.arguments(textArguments),
               summon[Environment],
               summon[WorkingDirectory],
               summon[Stdio],
               true,
               Login(t"tester", Unset))

          app(using invocation)
          invocation

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
         .order(identity)
      .assert(_ == List(1, 2))

      test(m"A single status is discovered without widening to Status"):
        HelpApp.tree.subcommands.filter(_.command == t"userdel").bind(_.statuses).map(_.code)
      .assert(_ == List(1))

      test(m"A status unioned with a plain Exit is discovered, and the Exit is skipped"):
        HelpApp.tree.subcommands.filter(_.command == t"beta").bind(_.statuses).map(_.code)
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
               t"    --force <value>      do not ask for confirmation",
               t"",
               t"Exit statuses:",
               t"  1                      the server could not be reached",
               t"  2                      the configuration file was invalid")

      test(m"A matched subcommand is recorded on the invocation"):
        HelpApp.invoke(t"useradd", t"--home", t"/home/x").matches
      .assert(_ == List(t"useradd"))

      test(m"Nested matched subcommands are recorded in order"):
        HelpApp.invoke(t"distribution", t"ubuntu").matches
      .assert(_ == List(t"distribution", t"ubuntu"))

      test(m"An unrecognized subcommand records no matches"):
        HelpApp.invoke(t"bogus").matches
      .assert(_ == Nil)

      test(m"An empty invocation records no matches"):
        HelpApp.invoke().matches
      .assert(_ == Nil)

      test(m"A local view selects the subtree for a matched prefix"):
        HelpApp.tree.local(List(t"useradd")).let(_.command)
      .assert(_ == t"mytool useradd")

      test(m"A local view carries ancestor flags as global options"):
        HelpApp.tree.local(List(t"useradd")).let(_.parameters.filter(_.global).map(_.name))
         .or(Nil)
      .assert(_ == List(t"--verbose"))

      test(m"A local view keeps the subcommand's own flags local"):
        HelpApp.tree.local(List(t"useradd"))
         .let(_.parameters.filter(!_.global).map(_.name).order(identity)).or(Nil)
      .assert(_ == List(t"--force", t"--groups", t"--home"))

      test(m"A nested local view joins the full command path"):
        HelpApp.tree.local(List(t"distribution", t"ubuntu")).let(_.command)
      .assert(_ == t"mytool distribution ubuntu")

      test(m"A local view of an unknown path is unset"):
        HelpApp.tree.local(List(t"bogus"))
      .assert(_ == Unset)

      test(m"A local view of the empty path is the whole tree"):
        HelpApp.tree.local(Nil)
      .assert(_ == HelpApp.tree)

      test(m"An invocation's matched prefix selects its local help"):
        HelpApp.tree.local(HelpApp.invoke(t"useradd", t"extra").matches).let(_.command)
      .assert(_ == t"mytool useradd")

      test(m"Local help for a leaf renders its section with ancestor globals"):
        HelpApp.tree.local(List(t"useradd")).let: local =>
          summon[Help is Printable].print(local, stdios.muteStdio.termcap).cut(t"\n")
        . or(Nil)
      .assert:
        _ == List
              (t"Usage: mytool useradd [--verbose <value>] [options]",
               t"",
               t"add a user account",
               t"",
               t"Global options:",
               t"  --verbose <value>    verbose output",
               t"",
               t"Options:",
               t"  --groups <value>...  add the user to a group",
               t"  --force <value>      do not ask for confirmation",
               t"  --home <value>       specify the home directory",
               t"",
               t"Exit statuses:",
               t"  1                    the server could not be reached",
               t"  2                    the configuration file was invalid")

      test(m"Local help for a non-leaf renders its subcommands"):
        HelpApp.tree.local(List(t"distribution")).let: local =>
          summon[Help is Printable].print(local, stdios.muteStdio.termcap).cut(t"\n")
        . or(Nil)
      .assert:
        _ == List
              (t"Usage: mytool distribution [--verbose <value>] <command> [options]",
               t"",
               t"a different command to run",
               t"",
               t"Global options:",
               t"  --verbose <value>  verbose output",
               t"",
               t"Commands:",
               t"  red hat            Red Hat Linux",
               t"    --only <value>   there is only one",
               t"",
               t"  ubuntu             Ubuntu",
               t"    --one <value>    the first one",
               t"    --two <value>    the second one")

      // Clustered short flags (#1888). The `Interpreter` that completion consults is resolved
      // where the `Cli` is built, not per subcommand, so exercising clustering needs an
      // application of its own. Three single-character flags, so typing `-ab` leaves exactly
      // `-c` to offer.
      Enclave(t"clstr").dispatch:
        ' {
            import executives.completions
            import interpreters.posixClusteringInterpreter

            cli:
              Flag('a', description = t"the first flag")()
              Flag('b', description = t"the second flag")()
              Flag('c', description = t"the third flag")()
              execute(Exit.Ok)

            t"finished"
          }

      . sandbox:
          val tool = summon[Enclave.Tool].path

          // Typing `-ab` and choosing `-c` must extend the word to `-abc`, not replace it with
          // `-c`, and must not terminate the word — the next letter may follow. Each shell is
          // driven through the completions executive directly, so these assert on exactly what
          // the shell is told.
          suite(m"Clustered short-flag completion"):
            // Each `compadd` invocation is one line of NUL-separated arguments.
            def zsh(text: Text): List[List[Text]] =
              sh"$tool '{completions}' zsh 2 3 /dev/null -- clstr $text".exec[Text]()
              . cut(t"\n").stdlib.filter(_.length > 0)
              . map(_.cut(t"\u0000").stdlib.to(List)).to(List)

            def adjacent(line: List[Text], first: Text, second: Text): Boolean =
              line.stdlib.sliding(2).exists: pair =>
                pair.headOption == Some(first) && pair.lastOption == Some(second)

            // `compadd -p <prefix>` is zsh's *hidden* prefix: inserted, but neither displayed
            // nor matched against, so the word grows by the single new character.
            test(m"zsh takes the typed cluster as a hidden prefix"):
              zsh(t"-ab").stdlib.exists(adjacent(_, t"-p", t"-ab"))
            . assert(_ == true)

            test(m"zsh inserts only the character being added"):
              zsh(t"-ab").stdlib.exists(adjacent(_, t"--", t"c"))
            . assert(_ == true)

            // The menu still names the flag, rather than showing the bare letter.
            test(m"zsh names the flag in full in the menu"):
              zsh(t"-ab").prim.let(_.prim).or(t"").starts(t"-c ")
            . assert(_ == true)

            test(m"zsh offers a no-trailing-space variant, so the cluster can grow"):
              zsh(t"-ab").stdlib.exists(adjacent(_, t"-S", t""))
            . assert(_ == true)

            // Fish and bash insert whole words, so the candidate is the extended cluster.
            test(m"fish offers the extended cluster"):
              sh"$tool '{completions}' fish 2 3 /dev/null -- clstr -ab".exec[Text]()
              . cut(t"\n").stdlib.to(List).map(_.cut(t"\t").prim.or(t""))
            . assert(_.contains(t"-abc"))

            test(m"bash offers the extended cluster"):
              sh"$tool '{completions}' bash 2 3 /dev/null -- clstr -ab".exec[Text]()
              . cut(t"\n").stdlib.to(List)
            . assert(_.contains(t"-abc"))

            // A two-character argument is not a cluster: the interpreter only expands beyond
            // two characters, so `-a` still completes as an ordinary short flag.
            test(m"a two-character flag is completed without a prefix"):
              zsh(t"-a").stdlib.exists(adjacent(_, t"-p", t"-a"))
            . assert(_ == false)

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

      // A missing `Inspectable` is never a compile error — `derived` always succeeds and
      // substitutes a marked `toString`, `Showable` or `Encodable` rendering — so coverage can
      // only be held in place by asserting on the renderings themselves.
      suite(m"Native-rendering coverage"):
        test(m"exoskeleton's types inspect natively"):
          Inspectable.fallbacks
           ( Flag[Text](t"count").inspect,
             Flag[Text]('c', aliases = List(t"count"), description = t"how many").inspect )
        . assert(_ == Nil)

        test(m"a flag inspects with the state which governs its parsing"):
          Flag[Text](t"count", repeatable = true, aliases = List('c')).inspect
        . assert:
            _ == Text
                  ( "Flag(--count ╱ aliases:[-c] ╱ repeatable:true ╱ secret:false ╱ "
                    +"description:○)" )

      suite(m"Prospective and requisite flags"):
        import interpreters.posixInterpreter
        import stdios.muteStdio

        // Run the application in invocation mode and return its exit status along with the
        // spent invocation, so accrued state can be inspected.
        def invoke(app: Cli ?=> Execution)(textArguments: Text*): (Exit, Invocation) =
          val invocation =
            Invocation
              (Cli.arguments(textArguments),
               summon[Environment],
               summon[WorkingDirectory],
               summon[Stdio],
               true,
               Login(t"tester", Unset))

          (app(using invocation).exitStatus, invocation)

        test(m"A flag applied in the pure section yields a prospective handle"):
          def app(using cli: Cli): Execution =
            val count: Prospective[Text] = Flag[Text](t"count")()
            execute(if count().let(_ == t"4").or(false) then Exit.Ok else Exit.Fail(1))

          invoke(app)(t"--count", t"4")(0)
        .assert(_ == Exit.Ok)

        test(m"A prospective flag's presence is visible in the pure section"):
          def app(using cli: Cli): Execution =
            val count = Flag[Text](t"count")()
            if count.present then execute(Exit.Ok) else execute(Exit.Fail(1))

          invoke(app)(t"--count", t"4")(0)
        .assert(_ == Exit.Ok)

        test(m"A flag applied inside execute resolves directly to an optional value"):
          def app(using cli: Cli): Execution =
            execute:
              val count: Optional[Text] = Flag[Text](t"count")()
              if count == t"4" then Exit.Ok else Exit.Fail(1)

          invoke(app)(t"--count", t"4")(0)
        .assert(_ == Exit.Ok)

        test(m"A required flag which is present yields its value inside execute"):
          def app(using cli: Cli): Execution =
            val count: Requisite[Text] = Flag[Text](t"count").require()
            execute(if count() == t"4" then Exit.Ok else Exit.Fail(1))

          invoke(app)(t"--count", t"4")(0)
        .assert(_ == Exit.Ok)

        test(m"A missing required flag precludes execution with a usage error"):
          def app(using cli: Cli): Execution =
            Flag[Text](t"count").require()
            execute(Exit.Ok)

          invoke(app)()(0)
        .assert(_ == Exit.Fail(2))

        test(m"All missing required flags are accrued in order"):
          def app(using cli: Cli): Execution =
            Flag[Text](t"alpha").require()
            Flag[Text](t"beta").require()
            execute(Exit.Ok)

          invoke(app)()(1).missingRequisites.map(_.name)
        .assert(_ == List(t"alpha", t"beta"))

        test(m"Requiring a present flag inside execute yields the value directly"):
          def app(using cli: Cli): Execution =
            execute:
              val count: Text = Flag[Text](t"count").require()
              if count == t"4" then Exit.Ok else Exit.Fail(1)

          invoke(app)(t"--count", t"4")(0)
        .assert(_ == Exit.Ok)

        test(m"Requiring a missing flag inside execute raises MissingFlagError"):
          def app(using cli: Cli): Execution =
            execute(Flag[Text](t"count").require() yet Exit.Ok)

          try invoke(app)() yet t"returned" catch case error: MissingFlagError => t"raised"
        .assert(_ == t"raised")

        class Wait(val text: Text)

        given Wait is Interpretable:
          override def placeholder: Optional[Text] = t"seconds"

          def interpret(arguments: List[Argument]): Optional[Wait] = arguments match
            case argument :: _ => Wait(argument())
            case _             => Unset

        val requiredTree: Help =
          helpTree
           (t"reqtool",
            summon[Environment],
            summon[WorkingDirectory],
            summon[Stdio],
            Login(t"tester", Unset)):
            Flag[Text](t"home", description = t"the home directory").require()
            Flag[Wait](t"wait", description = t"how long to wait").validate()
            execute(Exit.Ok)

        test(m"Required flags are marked in the help tree"):
          requiredTree.parameters.filter(_.name == t"--home").map(_.required)
        .assert(_ == List(true))

        test(m"Required flags are annotated in rendered help"):
          summon[Help is Printable].print(requiredTree, stdios.muteStdio.termcap)
        .assert(_.contains(t"the home directory (required)"))

        test(m"A validated flag's operand placeholder is taken from its Interpretable"):
          requiredTree.parameters.filter(_.name == t"--wait").map(_.operand)
        .assert(_ == List(t"seconds"))

        test(m"A validated flag is marked required in the help tree"):
          requiredTree.parameters.filter(_.name == t"--wait").map(_.required)
        .assert(_ == List(true))

        test(m"A validated flag with a well-formed value yields it inside execute"):
          def app(using cli: Cli): Execution =
            val count: Requisite[Int] = Flag[Int](t"count").validate()
            execute(if count() == 42 then Exit.Ok else Exit.Fail(1))

          invoke(app)(t"--count", t"42")(0)
        .assert(_ == Exit.Ok)

        test(m"A validated flag's decoded value is visible in the pure section"):
          def app(using cli: Cli): Execution =
            val count = Flag[Int](t"count").validate()
            if count.value == 42 then execute(Exit.Ok) else execute(Exit.Fail(1))

          invoke(app)(t"--count", t"42")(0)
        .assert(_ == Exit.Ok)

        test(m"A malformed value precludes execution with a usage error"):
          def app(using cli: Cli): Execution =
            Flag[Int](t"count").validate()
            execute(Exit.Ok)

          invoke(app)(t"--count", t"4x")(0)
        .assert(_ == Exit.Fail(2))

        test(m"A malformed value accrues a fault with the decoder's explanation"):
          def app(using cli: Cli): Execution =
            Flag[Int](t"count").validate()
            execute(Exit.Ok)

          invoke(app)(t"--count", t"4x")(1).faults.map(_(1))
        .assert(_.prim.let(_.contains(t"not a valid int")).or(false))

        test(m"A missing validated flag is accrued as missing, not as a fault"):
          def app(using cli: Cli): Execution =
            Flag[Int](t"count").validate()
            execute(Exit.Ok)

          val (exit, invocation) = invoke(app)()
          (exit, invocation.missingRequisites.map(_.name), invocation.faults)
        .assert(_ == (Exit.Fail(2), List(t"count"), Nil))

        test(m"Validating a well-formed flag inside execute yields the value directly"):
          def app(using cli: Cli): Execution =
            execute:
              val count: Int = Flag[Int](t"count").validate()
              if count == 42 then Exit.Ok else Exit.Fail(1)

          invoke(app)(t"--count", t"42")(0)
        .assert(_ == Exit.Ok)

        test(m"Validating a malformed flag inside execute raises InvalidFlagError"):
          def app(using cli: Cli): Execution =
            execute(Flag[Int](t"count").validate() yet Exit.Ok)

          try invoke(app)(t"--count", t"4x") yet t"returned"
          catch case error: InvalidFlagError => t"raised"
        .assert(_ == t"raised")

        test(m"Validating a missing flag inside execute raises MissingFlagError"):
          def app(using cli: Cli): Execution =
            execute(Flag[Int](t"count").validate() yet Exit.Ok)

          try invoke(app)() yet t"returned"
          catch case error: MissingFlagError => t"raised"
        .assert(_ == t"raised")

      suite(m"Pathname operand completion"):
        import interfaces.paths.pathOnLinux
        import interpreters.posixInterpreter
        import stdios.muteStdio

        // A directory of known content, used as the working directory below so that
        // `Pathname.complete`'s listings are deterministic.
        val fixture: Path on Linux = unsafely:
          val dir: Path on Linux =
            temporaryDirectory[Path on Linux]/t"exoskeleton-operand-${Uuid()}"

          dir.create[Directory]()
          (dir/t"one.txt").create[File]()
          (dir/t"two.txt").create[File]()
          (dir/t"src").create[Directory]()
          (dir/t"src"/t"inner.txt").create[File]()
          dir

        given WorkingDirectory = () => fixture.encode

        test(m"An empty operand lists the working directory's visible entries"):
          Pathname.complete(t"", Prim).map(_.core).order(identity)
        .assert(_ == List(t"one.txt", t"src/", t"two.txt"))

        test(m"A partial name narrows the candidates"):
          Pathname.complete(t"on", Prim).map(_.core)
        .assert(_ == List(t"one.txt"))

        test(m"A directory operand descends into the directory"):
          Pathname.complete(t"src/", Prim).map { s => t"${s.prefix}${s.core}" }
        .assert(_ == List(t"src/inner.txt"))

        test(m"The pathname Discoverable offers the extractor's candidates for a flag operand"):
          pathnameDiscoverable.discover(t"src/", Prim).map(_.core)
        .assert(_ == List(t"inner.txt"))

        test(m"A flag operand's partial text reaches its Discoverable"):
          val args = Cli.arguments(List(t"--hue", t"re"), 1, 2)

          val completion =
            Completion
              (args,
               args,
               summon[Environment],
               summon[WorkingDirectory],
               Shell.Zsh,
               1,
               2,
               summon[Stdio],
               t"",
               Prim,
               Login(t"tester", Unset))

          given Text is Discoverable = (operand, _) => List(Suggestion(t"[$operand]"))

          def app(using cli: Cli): Unit = Flag[Text](t"hue")() yet ()
          app(using completion)
          completion.cursorSuggestions.map(_.core)
        .assert(_ == List(t"[re]"))
