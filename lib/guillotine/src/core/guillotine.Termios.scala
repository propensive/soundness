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
package guillotine

import anticipation.*
import gossamer.*
import rudiments.*
import spectacular.*

object Termios:
  // Everything on: line editing, echo, signals and the translations a shell expects.
  val cooked: Termios = Termios()

  // Everything off: each byte typed is delivered at once, unechoed and untranslated, and `^C`
  // and `^Z` are ordinary bytes — what a full-screen program sets before it draws.
  val raw: Termios =
    Termios
      ( echo            = false,
        canonical       = false,
        signals         = false,
        extensions      = false,
        translateReturn = false,
        flowControl     = false,
        postprocessing  = false )

  given showable: Termios is Showable = termios =>
    val switches =
      List
        ( t"echo"      -> termios.echo,
          t"icanon"    -> termios.canonical,
          t"isig"      -> termios.signals,
          t"iexten"    -> termios.extensions,
          t"icrnl"     -> termios.translateReturn,
          t"ixon"      -> termios.flowControl,
          t"opost"     -> termios.postprocessing )

    switches.map { (name, on) => if on then name else t"-$name" }.join(t" ")

// The line discipline of a terminal, as `stty` would set it: the switches a program most often
// changes, named after the POSIX flags they stand for. `canonical` is `icanon`, whether input is
// gathered and edited a line at a time or delivered as typed; `signals` is `isig`, whether `^C`
// and `^Z` signal the foreground program; `extensions` is `iexten`, the extra editing characters;
// `translateReturn` is `icrnl`, whether a typed carriage return arrives as a newline;
// `flowControl` is `ixon`, whether `^S` and `^Q` pause and resume output; and `postprocessing` is
// `opost` with `onlcr`, whether a newline written by the program is sent as a carriage return and
// newline. Turning `canonical` off also sets the read to return each byte as it arrives.
case class Termios
  ( echo:            Boolean = true,
    canonical:       Boolean = true,
    signals:         Boolean = true,
    extensions:      Boolean = true,
    translateReturn: Boolean = true,
    flowControl:     Boolean = true,
    postprocessing:  Boolean = true )
