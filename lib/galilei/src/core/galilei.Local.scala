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
package galilei

import ambience.*
import anticipation.*
import contingency.*
import gossamer.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*

object Local:
  // Note that Mac OS rules subsume Linux rules
  type Rules = Windows.Rules & MacOs.Rules

  inline given nominative: Local is Nominative under Rules = !!

  inline given pathOnLocal: (Path on Local) is Representative of Paths = !!

  given filesystem: System => Local is Filesystem:
    type UniqueRoot = false

    val name: Text = System.properties.os.name().show
    val separator: Text = System.properties.file.separator().show
    val self: Text = t"."
    val parent: Text = t".."

  given radical: (%.type | Drive) is Radical:
    type Plane = Local

    def encode(root: %.type | Drive): Text = root match
      case Drive(letter) => t"$letter:\\"
      case %             => t"/"

  given eradicable: (tactic: Tactic[Path.Error])
  =>  ( ((%.type | Drive) is Eradicable on Local)^{tactic} ) =
    new Eradicable:
      type Self = %.type | Drive
      type Plane = Local

      def length(text: Text): 1 | 3 =
        if text.starts(t"/") then 1
        else if driven(text) then 3
        else abort(Path.Error(_.InvalidRoot))

      def decode(text: Text): %.type | Drive =
        length(text) match
          case 1 => %
          case 3 => Drive(text.s.charAt(0).lest(Path.Error(_.InvalidRoot)))

  // Whether the text begins with a drive's root, such as `C:\`.
  private def driven(text: Text): Boolean =
    text.length >= 3 && text.s.charAt(1) == ':' && text.s.charAt(2) == '\\'

  // A path rooted as some host roots its paths — at `/`, or at a drive — may be reinterpreted
  // on `Local`: `(% / "etc").on[Local]`, a `p"…"` literal, or a path on `Linux`, `MacOs` or
  // `Windows`. The instance is for any root type, and checks the root's text, since the type is
  // often not known: that of `Drive('C') / "x"` is abstract, as is that of a `Path on Linux`
  // received from elsewhere. As for a `Radical` root in `Path#on`, a root which is neither
  // cannot have come from a valid path on such a platform, and throws.
  given submissible: [root] => root is Submissible on Local = text =>
    if text != t"/" && !(text.length == 3 && driven(text))
    then unsafely(abort(Path.Error(_.InvalidRoot)))

sealed trait Local extends Platform
