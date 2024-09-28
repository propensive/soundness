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
import fulminate.*
import galilei.*
import gossamer.*
import prepositional.*
import kaleidoscope.*
import rudiments.*
import denominative.*
import serpentine.*
import spectacular.*
import vacuous.*

import scala.compiletime.*
import scala.jdk.StreamConverters.*

import language.experimental.pureFunctions

object Windows:
  type Forbidden = ".*[\\cA-\\cZ].*" | "(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])(\\.*)?" |
      "\\.\\." | "\\." | ".*[:<>/\\\\|?\"*].*"

  object Path:
    inline given (using Tactic[PathError]) => Decoder[Path] as decoder:
      def decode(text: Text): Path = Navigable.decode(text)

    given Path is Navigable[Forbidden, Drive] as navigable:
      def root(path: Path): Drive = path.drive
      def prefix(drive: Drive): Text = t"${drive.letter}:\\"
      def descent(path: Path): List[Name[Forbidden]] = path.descent
      def separator(path: Path): Text = t"\\"

    given creator: PathCreator[Path, Forbidden, Drive] = Path(_, _)

    given rootParser: RootParser[Path, Drive] = text => text.only:
      case r"$letter([a-zA-Z]):\\.*" => (Drive(unsafely(letter.at(Prim).vouch).toUpper), text.skip(3))

    given Path is Showable as showable = _.render
    given encoder: Encoder[Path] = _.render
    given Path is Inspectable = _.render

  case class Path(drive: Drive, descent: List[Name[Forbidden]]) extends galilei.Path:
    def root: Drive = drive
    def name: Text = if descent.isEmpty then drive.name else descent.head.show
    def fullname: Text = t"${Path.navigable.prefix(drive)}${descent.reverse.map(_.render).join(t"\\")}"

  class SafePath(drive0: Drive, val safeDescent: List[Name[GeneralForbidden]])
  extends Path(drive0, safeDescent.map(_.widen[Forbidden]))

  object Relative:
    given creator: PathCreator[Relative, Forbidden, Int] = Relative(_, _)

    given (using ValueOf["."]) => Relative is Followable[Forbidden, "..", "."] as followable:
      val separators: Set[Char] = Set('\\')
      def separator(path: Relative): Text = t"\\"
      def ascent(path: Relative): Int = path.ascent
      def descent(path: Relative): List[Name[Forbidden]] = path.descent

    inline given decoder(using Tactic[PathError]): Decoder[Relative] = Followable.decoder[Relative]
    given Relative is Showable as showable = _.render
    given encoder: Encoder[Relative] = _.render
    given Relative is Inspectable = _.render

  object Drive:
    given Drive is Inspectable = drive => t"drive:${drive.letter}"
    given Drive is Showable as showable = drive => t"${drive.letter}"
    given default: Default[Drive] = () => Drive('C')

  case class Drive(letter: Char):
    def name: Text = t"$letter:"

    @targetName("child")
    infix def / (name: Name[Forbidden]): Path = Path(this, List(name))

    @targetName("child2")
    inline infix def / (name: Text)(using Tactic[PathError]): Path = Path(this, List(Name(name)))

  case class Relative(ascent: Int, descent: List[Name[Forbidden]]) extends galilei.Relative

  trait Entry extends galilei.Entry
