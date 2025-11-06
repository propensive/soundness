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
┃    Soundness, version 0.46.0.                                                                    ┃
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

import unsafeExceptions.canThrowAny
import classloaders.system
import environments.jre
import systemProperties.jre
import temporaryDirectories.systemProperties
import workingDirectories.jre
import supervisors.global
import logging.silent
import embeddings.automatic
import autopsies.contrastExpectations
import threading.platform

import strategies.throwUnsafely
import backstops.silent
import errorDiagnostics.stackTraces
import stdioSources.virtualMachine.ansi

import Shell.*

object Tests extends Suite(m"Exoskeleton Tests"):
  def run(): Unit =
    val foo: Text = "hello"
    Sandbox(t"abcd").dispatch:
      '{  import executives.completions
          import interpreters.posix

          val Alpha = Subcommand("alpha", e"a command to run")
          val Beta = Subcommand("beta", e"another command to run")
          val Gamma = Subcommand("gamma", e"a third command to run", hidden = true)
          val Distribution = Subcommand("distribution", e"a different command to run")
          val RedHat = Subcommand("red hat", e"Red Hat Linux")
          val Ubuntu = Subcommand("ubuntu", e"Ubuntu")
          val Gentoo = Subcommand("gentoo", e"Gentoo Linux")

          class Color(name: Text)

          cli:
            arguments match
              case Alpha() :: _ => execute(Exit.Ok)
              case Beta() :: _  => execute(Exit.Ok)
              case Gamma() :: _ =>
                given Color is Discoverable = _ => List(t"red", t"green", t"blue").map(Suggestion(_))
                given Color is Interpretable =
                  case arg :: Nil => Color(arg())
                  case _          => Color(t"unknown")

                Flag[Color]("colors", repeatable = true, aliases = List('c'), description = "multiple reds, greens or blues")()
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
                    given Color is Discoverable = _ => List(t"red", t"green", t"blue").map(Suggestion(_))
                    given Color is Interpretable =
                      case arg :: Nil => Color(arg())
                      case _          => Color(t"unknown")

                    Flag[Color]("color", aliases = List('f'), description = "red, green or blue")()
                    execute(Exit.Ok)

                  case _             => execute(Exit.Ok)

              case _            => execute(Exit.Fail(1))

          t"finished"  }

    . sandbox:
        // Warmup runs to avoid timing issues in CI
        Bash.tmux()(Tmux.completions(t""))
        Zsh.tmux()(Tmux.completions(t""))
        Fish.tmux(width = 120)(Tmux.completions(t""))

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

        test(m"Test flags on bash"):
          Bash.tmux()(Tmux.completions(t"distribution ubuntu "))
        . assert(_ == t"--one  --two")

        test(m"Test flags on fish"):
          Fish.tmux(width = 120)(Tmux.completions(t"distribution ubuntu "))
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

        test(m"Test flags on zsh"):
          Zsh.tmux()(Tmux.completions(t"distribution ubuntu --"))
        . assert(_ == t"--one   -- the first one\n--two   -- the second one")

        test(m"Test capture 1"):
          tool.completions:
            Zsh.tmux()(Tmux.completions(t"distribution ubuntu "))

        . assert()

        test(m"Test subcommands with spaces on zsh"):
          Zsh.tmux()(Tmux.completions(t"distribution "))
        . assert(_ == t"gentoo    -- Gentoo Linux\nred hat   -- Red Hat Linux\nubuntu    -- Ubuntu")

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
