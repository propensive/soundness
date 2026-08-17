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

import anticipation.*
import aviation.*
import gossamer.*
import revolution.*
import urticose.*
import vacuous.*

// The static metadata a manpage needs beyond what the `Help` tree discovers at runtime.
// Declared once as a `given Manual` in the application object; every field is optional, so an
// application without one still renders a complete, if spartan, page. Per-command and
// per-flag descriptions do NOT belong here: they stay on `Subcommand`, `Flag` and
// `CommandGroup` declarations, which the `Help` tree already carries.
object Manual:
  enum Section:
    case UserCommands, SystemCalls, LibraryFunctions, Devices, FileFormats, Games,
      Miscellanea, SystemAdministration

    def number: Int = ordinal + 1

    def title: Text = this match
      case UserCommands         => t"User Commands"
      case SystemCalls          => t"System Calls"
      case LibraryFunctions     => t"Library Functions"
      case Devices              => t"Devices"
      case FileFormats          => t"File Formats"
      case Games                => t"Games"
      case Miscellanea          => t"Miscellanea"
      case SystemAdministration => t"System Administration"

  case class Reference(name: Text, section: Int = 1)
  case class ExitStatus(code: Int, description: Text)
  case class EnvironmentVariable(name: Text, description: Text)
  case class ManFile(path: Text, description: Text)
  case class Example(caption: Optional[Text], command: Text)

case class Manual
  ( synopsisName: Optional[Text]                   = Unset,
    section:      Manual.Section                   = Manual.Section.UserCommands,
    version:      Optional[Semver]                 = Unset,
    date:         Optional[Date]                   = Unset,
    prose:        Optional[Text]                   = Unset,
    authors:      List[Text]                       = Nil,
    examples:     List[Manual.Example]             = Nil,
    exitStatuses: List[Manual.ExitStatus]          = Nil,
    environment:  List[Manual.EnvironmentVariable] = Nil,
    files:        List[Manual.ManFile]             = Nil,
    seeAlso:      List[Manual.Reference]           = Nil,
    bugs:         Optional[Text]                   = Unset,
    homepage:     Optional[HttpUrl]                = Unset )
