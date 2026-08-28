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

import scala.language.experimental.pureFunctions

import anticipation.*
import escapade.*
import gossamer.*
import symbolism.*
import vacuous.*

object Suggestion:
  def apply
    ( core:        Text,
      description: Optional[Text | Teletype] = Unset,
      hidden:      Boolean                   = false,
      incomplete:  Boolean                   = false,
      aliases:     List[Text]                = Nil,
      prefix:      Text                      = t"",
      suffix:      Text                      = t"",
      expanded:    Boolean                   = false,
      group:       Optional[CommandGroup]    = Unset,
      operand:     Boolean                   = false,
      display:     Optional[Text]            = Unset )
  :   Suggestion =

    new Suggestion
      (core, description, hidden, incomplete, aliases, prefix, suffix, expanded, group, operand,
       display)


case class Suggestion
  ( core:        Text,
    description: Optional[Text | Teletype],
    hidden:      Boolean,
    incomplete:  Boolean,
    aliases:     List[Text],
    prefix:      Text,
    suffix:      Text,
    expanded:    Boolean,
    group:       Optional[CommandGroup],
    // A candidate *value* for an operand — a filename, or one of a `select`'s options — rather
    // than a subcommand. Both are offered identically at the cursor, but only a subcommand is
    // part of the command's interface, and the help tree is built by probing these same
    // suggestions: without this distinction, `--help` enumerates the working directory (or
    // whatever else the machine happens to hold) as though it were syntax.
    operand:     Boolean = false,
    // What to show in the menu when it differs from what is inserted. Completing inside a
    // clustered short flag is the case that needs it: typing `-ab` and choosing `-c` inserts
    // only `c` (behind the hidden prefix `-ab`, so the word becomes `-abc`), but the menu should
    // still name the flag as `-c`. Only shells that can separate the two honour it — zsh, via
    // `compadd`'s display array; elsewhere the whole word is shown, as those shells do anyway.
    display:     Optional[Text] = Unset ):

  def text: Text = prefix+core+suffix
