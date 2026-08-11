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
  // Text`; making it a trait lets `Git.Hash` extend Serpentine's `Root`
  // (and therefore `Path`) while still flowing into every
  // `repo.foo(refspec: Refspec)` call site that previously accepted
  // opaque-typed hashes.
  object Refspec:
    def head(n: Int = 0): Refspec = unsafe(t"HEAD~$n")
    def unsafe(text: Text): Refspec = RawRef(text)

    def parse(text: Text)(using Tactic[Git.RefError]): Text =
      def fail(reason: Git.RefError.Reason): Text = abort(Git.RefError(text, reason))

      text.cut(t"/").each: part =>
        if part.starts(t".") || part.ends(t".") then fail(Git.RefError.Reason.LeadingOrTrailingDot)
        if part.ends(t".lock")                  then fail(Git.RefError.Reason.ReservedSuffix)
        if part.contains(t"@{")                 then fail(Git.RefError.Reason.ReservedSequence)
        if part.contains(t"..")                 then fail(Git.RefError.Reason.DoubleDot)
        if part.length == 0                     then fail(Git.RefError.Reason.EmptySegment)

        for char <- List('*', '[', '\\', ' ', '^', '~', ':', '?')
        do if part.contains(char) then fail(Git.RefError.Reason.InvalidCharacter)

      text

    given encodable: Refspec is Encodable in Text = _.text
    given parameterizable: Refspec is Parameterizable = _.text
    given showable: Refspec is Showable = _.text

  trait Refspec:
    def text: Text

  // Concrete `Refspec` for revision specifiers that aren't named refs:
  // `HEAD~N`, `master..feature`, raw revspecs decoded from text, etc.
  private case class RawRef(text: Text) extends Refspec

