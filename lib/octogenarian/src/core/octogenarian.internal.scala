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
package octogenarian

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import guillotine.*
import kaleidoscope.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*

object internal:

  // `Refspec` is the umbrella type for anything that can be passed to git
  // as a revision argument: branch names, tag names, commit hashes,
  // `HEAD~N`, `master..feature`, …  It used to be `opaque type Refspec =
  // Text`; making it a trait lets `GitHash` extend Serpentine's `Root`
  // (and therefore `Path`) while still flowing into every
  // `repo.foo(refspec: Refspec)` call site that previously accepted
  // opaque-typed hashes.
  object Refspec:
    def head(n: Int = 0): Refspec = unsafe(t"HEAD~$n")
    def unsafe(text: Text): Refspec = RawRef(text)

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

    given encodable: Refspec is Encodable in Text = _.text
    given parameterizable: Refspec is Parameterizable = _.text
    given showable: Refspec is Showable = _.text

  trait Refspec:
    def text: Text

  // Concrete `Refspec` for revision specifiers that aren't named refs:
  // `HEAD~N`, `master..feature`, raw revspecs decoded from text, etc.
  private case class RawRef(text: Text) extends Refspec

  object GitTag:
    def unsafe(text: Text): GitTag = new GitTag(text)
    def parse(text: Text)(using Tactic[GitRefError]): GitTag = new GitTag(Refspec.parse(text))

    given decoder: Tactic[GitRefError] => GitTag is Decodable in Text = parse(_)
    given showable: GitTag is Showable = _.text

  case class GitTag(text: Text) extends Refspec

  object GitBranch:
    def unsafe(text: Text): GitBranch = new GitBranch(text)
    def parse(text: Text)(using Tactic[GitRefError]): GitBranch = new GitBranch(Refspec.parse(text))

    given decoder: Tactic[GitRefError] => GitBranch is Decodable in Text = parse(_)
    given showable: GitBranch is Showable = _.text

  case class GitBranch(text: Text) extends Refspec

  // `GitHash` extends Serpentine's `Root` (and therefore `Path`), so a hash
  // IS a notes-plane path root: `hash / t"foo" / t"bar"` invokes Serpentine's
  // own `Path.def /` directly, with no Conversion or entry-point extension
  // needed.  Equality is by hash (Drive-style override).
  object GitHash:
    def apply(text: Text)(using Tactic[GitRefError]): GitHash = text match
      case r"[a-f0-9]{40}" => new GitHash(text)
      case _               => abort(GitRefError(text, GitRefError.Reason.BadHash))

    def unsafe(text: Text): GitHash = new GitHash(text)

    given decoder: Tactic[GitRefError] => GitHash is Decodable in Text = apply(_)
    given showable: GitHash is Showable = _.text

  class GitHash(val text: Text) extends Root(t"$text/"), Refspec:
    type Plane = Notes
    type Limit = GitHash

    override def hashCode: Int = text.hashCode

    override def equals(any: Any): Boolean = any match
      case other: GitHash => text == other.text
      case _              => false
