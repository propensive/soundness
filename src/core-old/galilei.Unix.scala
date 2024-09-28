/*
    Galilei, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package galilei

import anticipation.*
import contextual.*
import contingency.*
import galilei.*
import gossamer.*
import rudiments.*
import serpentine.*
import spectacular.*
import vacuous.*

import scala.jdk.StreamConverters.*

import language.experimental.pureFunctions

object Unix:
  type Forbidden = ".*\\/.*" | ".*[\\cA-\\cZ].*" | "\\.\\." | "\\."

  @targetName("child")
  infix def / (name: Name[Forbidden]): Path = Path(List(name))

  @targetName("child2")
  inline infix def / (name: Text)(using Tactic[PathError]): Path = Path(List(Name(name)))

  object Path:
    given Path is Radical as radical = () => Path(Nil)

    inline given (using Tactic[PathError]) => Decoder[Path] as decoder:
      def decode(text: Text): Path = Navigable.decode(text)

    given rootParser: RootParser[Path, Unset.type] = text =>
      if text.starts(t"/") then (Unset, text.skip(1)) else Unset

    given creator: PathCreator[Path, Forbidden, Unset.type] = (root, descent) => Path(descent)

    given Path is Navigable[Forbidden, Unset.type] as navigable:
      def separator(path: Path): Text = t"/"
      def root(path: Path): Unset.type = Unset
      def prefix(root: Unset.type): Text = t"/"
      def descent(path: Path): List[Name[Forbidden]] = path.descent

    given Path is Showable as showable = _.render
    given encoder: Encoder[Path] = _.render
    given Path is Inspectable = _.render

  case class Path(descent: List[Name[Forbidden]]) extends galilei.Path:
    def root: Unset.type = Unset
    def name: Text = if descent.isEmpty then Path.navigable.prefix(Unset) else descent.head.show
    def fullname: Text = t"${Path.navigable.prefix(Unset)}${descent.reverse.map(_.render).join(t"/")}"

  class SafePath(val safeDescent: List[Name[GeneralForbidden]])
  extends Path(safeDescent.map(_.widen[Forbidden]))

  object Relative:
    given creator: PathCreator[Relative, Forbidden, Int] = Relative(_, _)

    given (using ValueOf["."]) => Relative is Followable[Forbidden, "..", "."] as followable:
      val separators: Set[Char] = Set('/')
      def separator(path: Relative): Text = t"/"
      def ascent(path: Relative): Int = path.ascent
      def descent(path: Relative): List[Name[Forbidden]] = path.descent

    inline given (using Tactic[PathError]) => Decoder[Relative] as decoder = Followable.decoder[Relative]
    given Relative is Showable as showable = _.render
    given encoder: Encoder[Relative] = _.render
    given Relative is Inspectable = _.render

  case class Relative(ascent: Int, descent: List[Name[Forbidden]]) extends galilei.Relative

  trait Entry extends galilei.Entry
