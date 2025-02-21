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

import scala.quoted.*

import anticipation.*
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import proscenium.*

object Navigable:
  given [PathType <: Path] => PathType is Navigable by String = new Navigable:
    type Self = PathType
    type Operand = String
    def navigate(name: String): Text = name.tt

trait Navigable:
  type Self
  type Operand

  def navigate(name: Operand): Text


object Path:
  @targetName("Root")
  object % extends Path():
    type Subject = EmptyTuple

  def of[PlatformType, SubjectType <: Tuple](descent: Text*)
  :     Path on PlatformType of SubjectType =

    new Path(descent*):
      type Subject = SubjectType
      type Platform = PlatformType

  given Path is Encodable in Text = _.descent.reverse.join(t"/", t"/", t"")

case class Path(descent: Text*):
  type Platform
  type Subject <: Tuple

  // override def hashCode: Int = descent.hashCode

  // override def equals(that: Any): Boolean = that match
  //   case that: Path if that.descent == descent => true
  //   case _ => false

  // override def toString(): String = descent.reverse.mkString("/", "/", "")

  @targetName("child")
  inline infix def / [NameType, PlatformType <: Platform](name: NameType)
     (using navigable: (Path on PlatformType) is Navigable by NameType)
  :     Path on Platform of (name.type *: Subject) =

    Path.of(navigable.navigate(name) +: descent*)

  inline def on[PlatformType]: Path of Subject on PlatformType =
    ${Serpentine.onPlatform[PlatformType, Subject]('this)}

export Path.`%`

erased trait Linux
erased trait Windows

given Realm = Realm(t"serpentine")

object Serpentine:
  def onPlatform[PlatformType: Type, SubjectType <: Tuple: Type](path: Expr[Path])(using Quotes)
  :     Expr[Path on PlatformType of SubjectType] =

    import quotes.reflect.*

    def check[SubjectType: Type]
    Type.of[SubjectType] match
      case '[head *: tail] =>
        halt(m"Don't know ${TypeRepr.of[head]}")



// object Path:
//   given encodable: [PathType <: Path] => PathType is Encodable in Text = _.text

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

//   def descent(using navigable: Platform is Navigable): List[navigable.Operand] =
//     textDescent.reverse.map(navigable.element(_))

//   def child(filename: Text)(using Unsafe): Path on Platform =
//     Path.from(textRoot, filename :: textDescent, separator, caseSensitivity)

//   override def toString(): String = text.s

//   override def equals(that: Any): Boolean = that.asMatchable match
//     case that: Path =>
//       (textRoot == that.textRoot) && caseSensitivity.equal(textDescent, that.textDescent)

//     case _ =>
//       false

//   override def hashCode: Int =
//     separator.hashCode + textRoot.toString.hashCode*31 + caseSensitivity.hash(textDescent)

//   def parent: Optional[Path on Platform] =
//     if textDescent == Nil then Unset
//     else Path.from(textRoot, textDescent.tail, separator, caseSensitivity)

//   def ancestor(n: Int): Optional[Path on Platform] =
//     def recur(n: Int, current: Optional[Path on Platform]): Optional[Path on Platform] =
//       current.let: current =>
//         if n == 0 then current else recur(n - 1, current.parent)

//     recur(n, this)

//   def ancestors: List[Path on Platform] =
//     parent.let { parent => parent :: parent.ancestors }.or(Nil)

//   transparent inline def on [PlatformType]: Path on PlatformType =
//     inline erasedValue[PlatformType & Matchable] match
//       case _: Platform => this.asInstanceOf[Path on PlatformType]
//       case _ =>
//         summonInline[PlatformType is Navigable].give:
//           summonInline[PlatformType is Radical].give(Path.parse(text))

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
