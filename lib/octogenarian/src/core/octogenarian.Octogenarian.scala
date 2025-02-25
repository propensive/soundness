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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package octogenarian

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import kaleidoscope.*
import prepositional.*
import rudiments.*
import spectacular.*

import GitError.Reason.*

object Octogenarian:
  opaque type Refspec = Text
  opaque type GitTag <: Refspec = Text
  opaque type Branch <: Refspec = Text
  opaque type CommitHash <: Refspec = Text

  object Refspec:
    def head(n: Int = 0): Refspec = t"HEAD~$n"

    def parse(text: Text)(using Tactic[GitRefError]): Text =
      text.cut(t"/").each: part =>
        if part.starts(t".") || part.ends(t".") then raise(GitRefError(text)) yet text
        if part.ends(t".lock") then raise(GitRefError(text)) yet text
        if part.contains(t"@{") then raise(GitRefError(text)) yet text
        if part.contains(t"..") then raise(GitRefError(text)) yet text
        if part.length == 0 then raise(GitRefError(text)) yet text

        for char <- List('*', '[', '\\', ' ', '^', '~', ':', '?')
        do if part.contains(char) then raise(GitRefError(text)) yet text

      text

    given encodable: Refspec is Encodable in Text = identity(_)
    given Refspec is Showable = identity(_)

  object GitTag:
    def unsafe(text: Text): GitTag = text
    def apply(text: Text)(using Tactic[GitRefError]): GitTag = Refspec.parse(text)
    given decoder: Tactic[GitRefError] => GitTag is Decodable in Text = apply(_)
    given GitTag is Showable = identity(_)

  object Branch:
    def unsafe(text: Text): Branch = text
    def apply(text: Text)(using Tactic[GitRefError]): Branch = Refspec.parse(text)
    given decoder: Tactic[GitRefError] => Branch is Decodable in Text = apply(_)
    given Branch is Showable = identity(_)

  object CommitHash:
    def apply(text: Text)(using Tactic[GitRefError]): CommitHash = text match
      case r"[a-f0-9]{40}" => text
      case _               => raise(GitRefError(text)) yet text

    def unsafe(text: Text): CommitHash = text

    given decoder: Tactic[GitRefError] => CommitHash is Decodable in Text = apply(_)
    given CommitHash is Showable = identity(_)
