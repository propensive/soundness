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
package octogenarian

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import guillotine.*
import kaleidoscope.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*

object internal:
  opaque type Refspec = anticipation.Text
  opaque type GitTag <: Refspec = anticipation.Text
  opaque type GitBranch <: Refspec = anticipation.Text
  opaque type GitHash <: Refspec = anticipation.Text

  object Refspec:
    def head(n: Int = 0): Refspec = t"HEAD~$n"

    def unsafe(text: Text): Refspec = text

    def parse(text: Text)(using Tactic[GitRefError]): Text =
      def fail(reason: GitRefError.Reason): Text = abort(GitRefError(text, reason))

      text.cut(t"/").each: part =>
        if part.starts(t".") || part.ends(t".") then fail(GitRefError.Reason.LeadingOrTrailingDot)
        if part.ends(t".lock")                  then fail(GitRefError.Reason.ReservedSuffix)
        if part.contains(t"@{")                 then fail(GitRefError.Reason.ReservedSequence)
        if part.contains(t"..")                 then fail(GitRefError.Reason.DoubleDot)
        if part.length == 0                     then fail(GitRefError.Reason.EmptySegment)

        for char <- List('*', '[', '\\', ' ', '^', '~', ':', '?')
        do if part.contains(char) then fail(GitRefError.Reason.InvalidCharacter)

      text

    given encodable: Refspec is Encodable in Text = identity(_)
    given parameterizable: Refspec is Parameterizable = identity(_)
    given showable: Refspec is Showable = identity(_)

  object GitTag:
    def unsafe(text: Text): GitTag = text
    def apply(text: Text)(using Tactic[GitRefError]): GitTag = Refspec.parse(text)

    given decoder: Tactic[GitRefError] => GitTag is Decodable in Text = apply(_)
    given showable: GitTag is Showable = identity(_)

  object GitBranch:
    def unsafe(text: Text): GitBranch = text
    def apply(text: Text)(using Tactic[GitRefError]): GitBranch = Refspec.parse(text)

    given decoder: Tactic[GitRefError] => GitBranch is Decodable in Text = apply(_)
    given showable: GitBranch is Showable = identity(_)

  object GitHash:
    def apply(text: Text)(using Tactic[GitRefError]): GitHash = text match
      case r"[a-f0-9]{40}" => text
      case _               => abort(GitRefError(text, GitRefError.Reason.BadHash))

    def unsafe(text: Text): GitHash = text

    given decoder: Tactic[GitRefError] => GitHash is Decodable in Text = apply(_)
    given showable: GitHash is Showable = identity(_)

    // `commit / t"namespace"` lifts a `GitHash` into a `NoteRef` at the
    // ref path `refs/notes/<namespace>`. Lives here (rather than on
    // `NoteRef`'s companion) so that Symbolism's `/` implicit search,
    // which visits the dividend type's companion, can find it.
    given noteRefDivisible: Tactic[GitRefError]
    =>  GitHash is Divisible by Text to NoteRef =

      Divisible: (commit, namespace) =>
        NoteRef(commit, GitRefs.notes(namespace))
