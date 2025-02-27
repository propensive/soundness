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
package serpentine

import scala.compiletime.*, ops.int.*

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import nomenclature.*
import prepositional.*
import proscenium.*
import rudiments.*

object Path:
  @targetName("Root")
  object % extends Path(t"/"):
    type Subject = EmptyTuple
    type Constraint = %.type

  def of[PlatformType, RootType, SubjectType <: Tuple](root: Text, descent: Text*)
  :     Path on PlatformType of SubjectType under RootType =

    new Path(root, descent*):
      type Subject = SubjectType
      type Platform = PlatformType
      type Constraint = RootType

  given [PlatformType: Filesystem] => Path on PlatformType is Encodable in Text =
    path => path.descent.reverse.join(path.root, PlatformType.separator(), t"")

  private def conversion[FromType, ToType](fn: FromType => ToType) =
    new Conversion[FromType, ToType]:
      def apply(from: FromType): ToType = fn(from)

  inline given [SubjectType, RootType, PlatformType]
  =>    Conversion
         [Path of SubjectType under RootType, Path of SubjectType under RootType on PlatformType] =
    conversion(_.on[PlatformType])

case class Path(root: Text, descent: Text*):
  type Platform
  type Subject <: Tuple
  type Constraint

  private inline def check[SubjectType, PlatformType](path: List[Text]): Unit =
    inline !![SubjectType] match
      case _: (head *: tail) =>
        summonInline[head is Admissible on PlatformType].check(path.head)
        check[tail, PlatformType](path.tail)

      case EmptyTuple =>

  inline def on[PlatformType]: Path of Subject under Constraint on PlatformType =
    check[Subject, PlatformType](descent.to(List))
    summonInline[Constraint is RootAdmissible on PlatformType].check(root)
    this.asInstanceOf[Path of Subject under Constraint on PlatformType]

  def graft[RootType: Radical on Platform](root: RootType): Path of Subject under root.type =
    Path.of[Platform, root.type, Subject](RootType.encode(root), descent*)

  transparent inline def parent = inline !![Subject] match
    case head *: tail => Path.of[Platform, Constraint, tail.type](root, descent.tail*)
    case EmptyTuple   => compiletime.error("Path has no parent")
    case _ =>
      given Tactic[PathError] = summonInline[Tactic[PathError]]

      if descent.isEmpty then
        raise(PathError(PathError.Reason.RootParent))
        Path.of[Platform, Constraint, Tuple](root, descent*)

      else Path.of[Platform, Constraint, Tuple](root, descent.tail*)

  transparent inline def / (child: Any)(using navigable: Navigable by child.type)
  :     Path of (child.type *: Subject) under Constraint =
    summonFrom:
      case given (child.type is Admissible on Platform) =>
        Path.of[Platform, Constraint, child.type *: Subject]
         (root, navigable.follow(child) +: descent*)
      case _ =>
        type Subject0 = Subject
        type Constraint0 = Constraint

        new Path(root, navigable.follow(child) +: descent*):
          type Subject = child.type *: Subject0
          type Constraint = Constraint0

export Path.`%`

object Drive:
  def apply(letter: Char): Drive = new Drive(letter)

class Drive(val letter: Char) extends Root(t"$letter:\\"):
  type Platform = Windows

case class RootError(root: Text)(using Diagnostics) extends Error(m"$root is not a valid root")

erased trait Linux

object Linux:
  type Rules = MustNotContain["/"] & MustNotEqual["."] & MustNotEqual[".."] & MustNotEqual[""]
  erased given Linux is Nominative under Rules = !!

erased trait Windows

object Windows:
  type Rules =
    MustNotContain["\\"] & MustNotContain["/"] & MustNotContain[":"]
    & MustNotContain["*"] & MustNotContain["?"] & MustNotContain["\""] & MustNotContain["<"]
    & MustNotContain[">"] & MustNotContain["|"] & MustNotEnd["."] & MustNotEnd[" "]
    & MustNotMatch["(?i)CON(\\.[^.]+)?"] & MustNotMatch["(?i)PRN(\\.[^.]+)?"]
    & MustNotMatch["(?i)AUX(\\.[^.]+)?"] & MustNotMatch["(?i)NUL(\\.[^.]+)?"]
    & MustNotMatch["(?i)COM[0-9](\\.[^.]+)?"] & MustNotMatch["(?i)LPT[0-9](\\.[^.]+)?"]

  erased given Windows is Nominative under Rules = !!

erased trait MacOs

object MacOs:
  type Rules =
    MustNotContain["/"] & MustNotEqual["."] & MustNotEqual[".."] & MustNotEqual[""]
    & MustNotEqual["Icon\r"] & MustNotContain[":"]

  erased given MacOs is Nominative under Rules = !!

given Realm = Realm(t"serpentine")

object Filesystem:
  given Windows is Filesystem = () => t"\\"
  given Linux is Filesystem = () => t"/"
  given MacOs is Filesystem = () => t"/"

trait Filesystem:
  type Self
  def separator(): Text



// object Path:
//   given decoder: [PlatformType: {Navigable, Radical}]
//   =>    (Path on PlatformType) is Decodable in Text =

//     Path.parse(_)

//   given showable: [PlatformType] => (Path on PlatformType) is Showable = _.text

//   given generic: [PlatformType] => (Path on PlatformType) is Abstractable across Paths into Text =
//     _.text

//   given nominable: [PlatformType] => (Path on PlatformType) is Nominable = path =>
//     path.textDescent.prim.or(path.textRoot)

//   given specific: [PlatformType: {Navigable, Radical}]
//   =>    Path on PlatformType is Instantiable across Paths from Text =
//     _.decode[Path on PlatformType]

//   given communicable: Path is Communicable = path =>
//     Message(path.textDescent.reverse.join(path.textRoot, path.separator, t""))

//   given addable: [PlatformType: Navigable] => Tactic[PathError]
//   =>    (Path on PlatformType) is Addable by (Relative by PlatformType.Operand) into
//         (Path on PlatformType) =
//     (left, right) =>
//       def recur(descent: List[Text], ascent: Int): Path on PlatformType =
//         if ascent > 0 then
//           if descent.isEmpty then
//             abort(PathError(PathError.Reason.RootParent))

//             Path.from[PlatformType]
//              (left.textRoot, Nil, left.separator, left.caseSensitivity)
//           else recur(descent.tail, ascent - 1)
//         else
//           Path.from[PlatformType]
//            (left.textRoot,
//             right.textDescent ++ descent,
//             PlatformType.separator,
//             PlatformType.caseSensitivity)

//       recur(left.textDescent, right.ascent)

//   def apply
//      [RootType <: Root on PlatformType,
//       ElementType,
//       PlatformType: {Navigable by ElementType, Radical from RootType}]
//      (root0: RootType, elements: List[ElementType])
//   :     Path on PlatformType =
//     if elements.isEmpty then root0 else
//       Path.from[PlatformType]
//        (PlatformType.rootText(root0),
//         elements.map(PlatformType.makeElement(_)),
//         PlatformType.separator,
//         PlatformType.caseSensitivity)

//   private def from[PlatformType]
//      (root0: Text, elements: List[Text], separator: Text, caseSensitivity: Case)
//   :     Path on PlatformType =
//     new Path(root0, elements, separator, caseSensitivity):
//       type Platform = PlatformType

//   def parse[PlatformType: {Navigable, Radical}](path: Text): Path on PlatformType =
//     val root = PlatformType.root(path)

//     val descent =
//       path
//       . skip(PlatformType.rootLength(path))
//       . cut(PlatformType.separator)
//       . filter(_ != t"")
//       . reverse
//       . map(PlatformType.element(_))

//     Path(root, descent)

//   given [PlatformType: Navigable]
//   =>    (Path on PlatformType) is Divisible by PlatformType.Operand into (Path on PlatformType) =
//     new Divisible:
//       type Operand = PlatformType.Operand
//       type Self = Path on PlatformType
//       type Result = Path on PlatformType

//       def divide(path: Path on PlatformType, child: PlatformType.Operand): Path on PlatformType =
//         Path.from[path.Platform]
//          (path.textRoot,
//           PlatformType.makeElement(child) :: path.textDescent,
//           PlatformType.separator,
//           PlatformType.caseSensitivity)

// open class Path
//    (val textRoot: Text,
//     val textDescent: List[Text],
//     val separator: Text,
//     val caseSensitivity: Case)
// extends Pathlike:
//   type Platform

//   def depth: Int = textDescent.length
//   def root(using radical: Platform is Radical): radical.Source = radical.root(textRoot)
//   def text: Text = textDescent.reverse.join(textRoot, separator, t"")

//   def name(using navigable: Platform is Navigable): Optional[navigable.Operand] =
//     textDescent.prim.let(navigable.element(_))

//   def peer(using navigable: Platform is Navigable)(name: navigable.Operand)
//   :     Path on Platform raises PathError =
//     parent.let(_ / name).lest(PathError(PathError.Reason.RootParent))

//   def ancestor(n: Int): Optional[Path on Platform] =
//     def recur(n: Int, current: Optional[Path on Platform]): Optional[Path on Platform] =
//       current.let: current =>
//         if n == 0 then current else recur(n - 1, current.parent)

//     recur(n, this)

//   def ancestors: List[Path on Platform] =
//     parent.let { parent => parent :: parent.ancestors }.or(Nil)

//   def conjunction(right: Path on Platform): Path on Platform =
//     val difference = depth - right.depth
//     val left0 = textDescent.drop(difference)
//     val right0 = right.textDescent.drop(-difference)

//     def recur(left: List[Text], right: List[Text], size: Int, count: Int)
//     :     Path on Platform =
//       if left.isEmpty
//       then Path.from(textRoot, left0.drop(size - count), separator, caseSensitivity)
//       else if left.head == right.head then recur(left.tail, right.tail, size + 1, count + 1)
//       else recur(left.tail, right.tail, size + 1, 0)

//     recur(left0, right0, 0, 0)

//   def precedes(path: Path on Platform): Boolean = textDescent == Nil || conjunction(path) == path

//   def retain(count: Int): Path on Platform =
//     Path.from
//      (textRoot, textDescent.drop(depth - count), separator, caseSensitivity)

//   def relativeTo(right: Path on Platform)(using navigable: Platform is Navigable)
//   :     Relative by navigable.Operand raises PathError =
//     if textRoot != right.textRoot then abort(PathError(PathError.Reason.DifferentRoots))
//     val common = conjunction(right).depth
//     Relative(right.depth - common, textDescent.dropRight(common).map(navigable.element(_)))

//   def resolve(text: Text)(using Platform is Navigable, Platform is Radical)
//   :     Path on Platform raises PathError =
//     safely(Path.parse(text)).or(safely(this + Relative.parse(text))).or:
//       abort(PathError(PathError.Reason.InvalidRoot))
