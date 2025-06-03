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
import distillate.*
import fulminate.*
import gossamer.*
import nomenclature.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

given Realm = Realm(t"serpentine")

object Path:
  @targetName("Root")
  object % extends Path(t"/"):
    type Subject = EmptyTuple
    type Constraint = %.type

  given decodable: [system: System, root]
        => (radical: root is Radical on system)
        =>  (Path on system) is Decodable in Text = text =>
    val parts = text.skip(radical.length(text)).cut(system.separator)
    val parts2 = if parts.last == t"" then parts.init else parts
    val root = radical.encode(radical.decode(text))

    Path.of(root, parts2.reverse*)

  given nominable: [system] => (Path on system) is Nominable = path =>
    path.descent.prim.or(path.root)

  given specific: [system: System, root]
        =>  root is Radical on system
        =>  (Path on system) is Instantiable across Paths from Text =
    _.decode[Path on system]


  def unplatformed[root, subject <: Tuple](root: Text, descent: Text*): Path of subject under root =
    new Path(root, descent*):
      type Subject = subject
      type Constraint = root


  def of[system, root, subject <: Tuple](root: Text, descent: Text*)
  : Path on system of subject under root =

      new Path(root, descent*):
        type Subject = subject
        type Platform = system
        type Constraint = root

  given encodable: [system: System] => Path on system is Encodable in Text =
    path => path.descent.reverse.join(path.root, system.separator, t"")

  given showable: [system: System] => Path on system is Showable = _.encode

  given communicable: [system: System] => Path on system is Communicable =
    path => Message(path.encode)

  given generic: [system: System] => (Path on system) is Abstractable across Paths into Text =
    _.encode

  private def conversion[from, to](lambda: from => to): Conversion[from, to] = lambda(_)

  inline given [subject, root, system]
         =>  Conversion[Path of subject under root, Path of subject under root on system] =
    conversion(_.on[system])

case class Path(root: Text, descent: Text*):
  type Platform
  type Subject <: Tuple
  type Constraint

  def name: Text = descent.prim.or(root)

  transparent inline def knownElementTypes: Boolean = inline !![Subject] match
    case _: Zero           => true
    case _: (head *: tail) => true
    case _                 => false

  transparent inline def knownElements: Boolean = known[Subject]

  protected transparent inline def known[subject <: Tuple]: Boolean =
    inline !![subject] match
      case _: Zero           => true
      case _: (head *: tail) => inline constValueOpt[head] match
        case None               => false
        case Some(value)        => known[tail]
      case _ => false

  transparent inline def depth: Int = inline !![Subject] match
    case Zero         => 0
    case head *: tail => valueOf[Tuple.Size[Subject]]
    case _            => descent.length

  private inline def check[subject, system](path: List[Text]): Unit =
    inline !![subject] match
      case _: (head *: tail) =>
        summonInline[head is Admissible on system].check(path.head)
        check[tail, system](path.tail)

      case _ =>

  inline def on[system]: Path of Subject under Constraint on system =
    check[Subject, system](descent.to(List))
    summonInline[Constraint is Submissible on system].check(root)
    this.asInstanceOf[Path of Subject under Constraint on system]

  def graft[radical: Radical on Platform](root: radical): Path of Subject under root.type =
    Path.of[Platform, root.type, Subject](radical.encode(root), descent*)

  transparent inline def sameRoot(right: Path): Boolean = summonFrom:
    case platform: (Platform is System) =>
      inline if !![platform.UniqueRoot] then true else root == right.root
    case _ =>
      root == right.root

  transparent inline def conjunction(right: Path): Optional[Path] =
    inline sameRoot(right) match
      case true  => certain(right)
      case false => Unset
      case _     => determine(right)

  transparent inline def relativeTo(right: Path): Optional[Relative] = inline sameRoot(right) match
    case true  => val path = certain(right)
                  inline val baseAscent: Int = count[Subject, right.Subject]
                  inline val ascent = constValue[Tuple.Size[right.Subject]] - baseAscent
                  inline val retain = constValue[Tuple.Size[Subject]] - baseAscent

                  type Subject2 = Tuple.Take[Subject, retain.type]
                  Relative[Platform, Subject2, ascent.type](ascent, descent.dropRight(baseAscent)*)

    case false => Unset
    case _     => determine(right) match
                    case Unset      => Unset
                    case path: Path =>
                      val ascent = right.depth - path.depth
                      Relative(ascent, descent.dropRight(path.depth)*)

  protected transparent inline def determine(right: Path): Optional[Path] = summonFrom:
    case given ValueOf[Constraint] => summonFrom:
      case given ValueOf[right.Constraint] => summonFrom:
        case given (Constraint =:= right.Constraint) => certain(right)
        case _                                       => Unset
      case _ => if root != right.root then Unset else certain(right)
    case _ => if root != right.root then Unset else certain(right)

  protected transparent inline def count[LeftType <: Tuple, RightType <: Tuple]: Int = summonFrom:
    case _: (Tuple.Last[LeftType] =:= Tuple.Last[RightType]) =>
      1 + count[Tuple.Init[LeftType], Tuple.Init[RightType]]

    case _ =>
      0

  protected transparent inline def certain(right: Path): Path =
    inline !![right.Subject] match
      case _: Zero => Path.of[Platform, Constraint, Zero](root)
      case _: (head *: tail) => inline !![Subject] match
        case _: Zero => Path.of[Platform, Constraint, Zero](root)
        case _: (head2 *: tail2) =>
          inline val n = count[head *: tail, head2 *: tail2]
          type Subject2 = Tuple.Reverse[Tuple.Take[Tuple.Reverse[Subject], n.type]]
          Path.of[Platform, Constraint, Subject2](root, descent.takeRight(n)*)
        case _ => calculate(right)
      case _ => calculate(right)


  protected def calculate(right: Path): Path =
    val difference = depth - right.depth
    val left0 = descent.drop(difference).to(List)
    val right0 = right.descent.drop(-difference).to(List)


    def recur(left: List[Text], right: List[Text], size: Int, count: Int)
    : Path on Platform =

        if left.isEmpty then Path.of(root, left0.drop(size - count)*)
        else if left.head == right.head then recur(left.tail, right.tail, size + 1, count + 1)
        else recur(left.tail, right.tail, size + 1, 0)


    recur(left0, right0, 0, 0)


  transparent inline def parent: Optional[Path on Platform under Constraint] =
    inline !![Subject] match
      case head *: tail => Path.of[Platform, Constraint, tail.type](root, descent.tail*)
      case EmptyTuple   => compiletime.error("Path has no parent")
      case _ =>
        given Tactic[PathError] = summonInline[Tactic[PathError]]

        if descent.isEmpty then
          raise(PathError(PathError.Reason.RootParent))
          Path.of[Platform, Constraint, Tuple](root, descent*)

        else Path.of[Platform, Constraint, Tuple](root, descent.tail*)

  inline def ancestors: List[Path on Platform under Constraint] =
    parent.let { parent => parent :: parent.ancestors }.or(Nil)


  def child(value: Text)(using Unsafe): Path on Platform under Constraint =
    Path.of[Platform, Constraint, Text *: Subject](root, value +: descent*)

  @targetName("slash")
  transparent inline def / (child: Any)
  : Path of (child.type *: Subject) under Constraint =

      summonFrom:
        case given (child.type is Admissible on Platform) =>
          Path.of[Platform, Constraint, child.type *: Subject]
            (root, summonInline[child.type is Navigable].follow(child) +: descent*)

        case _ =>
          Path.unplatformed[Constraint, child.type *: Subject]
           (root, summonInline[child.type is Navigable].follow(child) +: descent*)


  transparent inline def peer(child: Any)(using child.type is Admissible on Platform)
  :     Path on Platform under Constraint =
    inline !![Subject] match
      case _: (head *: tail) =>
        Path.of[Platform, Constraint, child.type *: tail]
         (root, summonInline[child.type is Navigable].follow(child) +: descent*)

      case _ =>
        Path.of[Platform, Constraint, Tuple]
         (root, summonInline[child.type is Navigable].follow(child) +: descent*)

  transparent inline def + (relative: Relative): Path =
    type Base = Tuple.Reverse[Tuple.Take[Tuple.Reverse[Subject], relative.Constraint]]
    type Subject2 = Tuple.Concat[relative.Subject, Base]
    Path.of[Platform, Constraint, Subject2]
     (root, relative.descent ++ descent.drop(relative.ascent)*)

// object Path:
//   given addable: [system: Navigable] => Tactic[PathError]
//   =>    (Path on system) is Addable by (Relative by system.Operand) into
//         (Path on system) =
//     (left, right) =>
//       def recur(descent: List[Text], ascent: Int): Path on system =
//         if ascent > 0 then
//           if descent.isEmpty then
//             abort(PathError(PathError.Reason.RootParent))

//             Path.from[system]
//              (left.textRoot, Nil, left.separator, left.caseSensitivity)
//           else recur(descent.tail, ascent - 1)
//         else
//           Path.from[system]
//            (left.textRoot,
//             right.textDescent ++ descent,
//             system.separator,
//             system.caseSensitivity)

//       recur(left.textDescent, right.ascent)

// open class Path
//    (val textRoot: Text,
//     val textDescent: List[Text],
//     val separator: Text,
//     val caseSensitivity: Case)
// extends Pathlike:
//   type Platform

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
