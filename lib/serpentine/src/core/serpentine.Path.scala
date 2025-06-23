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
┃    Soundness, version 0.34.0.                                                                    ┃
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
    type Topic = EmptyTuple
    type Constraint = %.type

  given decodable: [system: System, root]
        => (radical: root is Radical on system)
        =>  (Path on system) is Decodable in Text =

    text =>
      val root = radical.encode(radical.decode(text))
      val parts = text.skip(radical.length(text)).cut(system.separator)
      val parts2 = if parts.last == t"" then parts.init else parts

      Path.of(root, parts2.reverse*)

  given nominable: [system] => (Path on system) is Nominable = path =>
    path.descent.prim.or(path.root)

  given instantiable: [system: System]
        =>  Radical on system
        =>  (Path on system) is Instantiable across Paths from Text =
    _.decode[Path on system]

  def unplatformed[root, topic <: Tuple](root: Text, descent: Text*): Path of topic under root =
    new Path(root, descent*):
      type Topic = topic
      type Constraint = root


  def of[system, root, topic <: Tuple](root: Text, descent: Text*)
  : Path on system of topic under root =

      new Path(root, descent*):
        type Plane = system
        type Constraint = root
        type Topic = topic


  given encodable: [system: System] => Path on system is Encodable in Text =
    path => path.descent.reverse.join(path.root, system.separator, t"")

  given showable: [system: System] => Path on system is Showable = _.encode

  given communicable: [system: System] => Path on system is Communicable =
    path => Message(path.encode)

  given generic: [system: System, path <: Path on system]
        => path is Abstractable across Paths into Text =
    _.encode

  private def conversion[from, to](lambda: from => to): Conversion[from, to] = lambda(_)

  inline given convert: [topic, root, system, path <: Path of topic under root]
         =>  Conversion[path, Path of topic on system under root] =
    conversion(_.on[system])


  transparent inline given quotient: [system, path <: Path on system] => path is Quotient =
    ( path =>
        if path.empty then None
        else if path.descent.length == 1 then Some((path.root, path.descent.head))
        else Some((path.root, Relative(0, path.descent*))) )
    : path is Quotient of Text over (Relative on system) | Text


case class Path(root: Text, descent: Text*) extends Constrained:
  type Plane
  type Topic <: Tuple

  def name: Text = descent.prim.or(root)
  def empty: Boolean = descent.isEmpty

  inline def knownElementTypes: Boolean = inline !![Topic] match
    case _: Zero           => true
    case _: (head *: tail) => true
    case _                 => false

  transparent inline def knownElements: Boolean = known[Topic]

  protected inline def known[topic <: Tuple]: Boolean =
    inline !![topic] match
      case _: Zero           => true
      case _: (head *: tail) => inline constValueOpt[head] match
        case None               => false
        case Some(value)        => known[tail]
      case _ => false

  def resolve(text: Text)
       (using (Path on Plane) is Decodable in Text,
              (Relative on Plane) is Decodable in Text)
  : Path on Plane raises PathError =

      safely(text.decode[Path on Plane]).or(safely(this + text.decode[Relative on Plane])).or:
        abort(PathError(_.InvalidRoot))


  transparent inline def depth: Int = inline !![Topic] match
    case Zero         => 0
    case head *: tail => valueOf[Tuple.Size[Topic]]
    case _            => descent.length

  private inline def check[topic, system](path: List[Text]): Unit =
    inline !![topic] match
      case _: Zero => ()

      case _: (head *: tail) =>
        infer[head is Admissible on system].check(path.head)
        check[tail, system](path.tail)

      case _ =>
        path.each: element =>
          infer[Text is Admissible on system].check(element)

  inline def on[system]: Path of Topic under Constraint on system = summonFrom:
    case given (`system` =:= Plane) =>
      this.asInstanceOf[Path of Topic under Constraint on system]

    case _ =>
      check[Topic, system](descent.to(List))

      summonFrom:
        case constraint: (Constraint is Submissible on `system`)       => constraint.check(root)
        case radical: (Constraint is Radical on `system`)              => radical.decode(root)
        case system: (`system` is (System { type UniqueRoot = true })) =>
          infer[Plane is (System { type UniqueRoot = true })]

      this.asInstanceOf[Path of Topic under Constraint on system]

  def graft[radical: Radical on Plane](root: radical): Path of Topic under root.type =
    Path.of[Plane, root.type, Topic](radical.encode(root), descent*)

  transparent inline def sameRoot(right: Path): Boolean = summonFrom:
    case plane: (Plane is System) =>
      inline if !![plane.UniqueRoot] then true else root == right.root
    case _ =>
      root == right.root

  transparent inline def rename(lambda: (prior: Text) ?=> Text): Optional[Path] =
    parent.let: parent =>
      descent.prim.let(parent / lambda(using _))

  transparent inline def conjunction(right: Path): Optional[Path] =
    inline sameRoot(right) match
      case true  => certain(right)
      case false => Unset
      case _     => determine(right)

  def relative: Relative of Topic on Plane under 0 =
    Relative[Plane, Topic, 0](0, descent*)

  transparent inline def relativeTo[plane](right: Path on plane): Optional[Relative] =
    inline sameRoot(right) match
      case true  =>
        val path = certain(right)
        inline val baseAscent: Int = count[Topic, right.Topic]

        inline !![right.Topic] match
          case _: (_ *: _) | Zero =>
            inline val ascent = constValue[Tuple.Size[right.Topic]] - baseAscent

            inline !![Topic] match
              case _: (_ *: _) | Zero =>
                inline val retain = constValue[Tuple.Size[Topic]] - baseAscent
                type Topic2 = Tuple.Take[Topic, retain.type]
                summonFrom:
                  case given (Plane =:= `plane`) =>
                    Relative[Plane, Topic2, ascent.type]
                     (ascent, descent.dropRight(baseAscent)*)

                  case _ =>
                    Relative[Any, Topic2, ascent.type]
                     (ascent, descent.dropRight(baseAscent)*)

              case _ =>
                summonFrom:
                  case given (Plane =:= `plane`) =>
                    Relative[Plane, Tuple, Nat]
                     (right.depth - path.depth, descent.dropRight(path.depth)*)
                  case _ =>
                    Relative[Any, Tuple, Nat]
                     (right.depth - path.depth, descent.dropRight(path.depth)*)

          case _ =>
            summonFrom:
              case given (Plane =:= `plane`) =>
                Relative[Plane, Tuple, Nat]
                 (right.depth - path.depth, descent.dropRight(path.depth)*)
              case _ =>
                Relative[Any, Tuple, Nat]
                 (right.depth - path.depth, descent.dropRight(path.depth)*)

      case false =>
        Unset

      case _ =>
        determine(right) match
          case Unset      => Unset
          case path: Path =>
            summonFrom:
              case given (Plane =:= `plane`) =>
                Relative[Plane, Tuple, Nat]
                 (right.depth - path.depth, descent.dropRight(path.depth)*)

              case _ =>
                Relative[Any, Tuple, Nat](right.depth - path.depth, descent.dropRight(path.depth)*)


  protected transparent inline def determine(right: Path): Optional[Path] = summonFrom:
    case given ValueOf[Constraint] => summonFrom:
      case given ValueOf[right.Constraint] => summonFrom:
        case given (Constraint =:= right.Constraint) => certain(right)
        case _                                       => Unset
      case _ => if root != right.root then Unset else certain(right)
    case _ => if root != right.root then Unset else certain(right)

  protected transparent inline def count[left <: Tuple, right <: Tuple]: Int = summonFrom:
    case _: (Tuple.Last[`left`] =:= Tuple.Last[`right`]) =>
      1 + count[Tuple.Init[`left`], Tuple.Init[`right`]]

    case _ =>
      0

  protected transparent inline def certain(right: Path): Path =
    inline !![right.Topic] match
      case _: Zero => Path.of[Plane, Constraint, Zero](root)
      case _: (head *: tail) => inline !![Topic] match
        case _: Zero => Path.of[Plane, Constraint, Zero](root)
        case _: (head2 *: tail2) =>
          inline val n = count[head *: tail, head2 *: tail2]
          type Topic2 = Tuple.Reverse[Tuple.Take[Tuple.Reverse[Topic], n.type]]
          Path.of[Plane, Constraint, Topic2](root, descent.takeRight(n)*)
        case _ => calculate(right)
      case _ => calculate(right)


  protected def calculate(right: Path): Path =
    val difference = depth - right.depth
    val left0 = descent.drop(difference).to(List)
    val right0 = right.descent.drop(-difference).to(List)


    def recur(left: List[Text], right: List[Text], size: Int, count: Int)
    : Path on Plane =

        if left.isEmpty then Path.of(root, left0.drop(size - count)*)
        else if left.head == right.head then recur(left.tail, right.tail, size + 1, count + 1)
        else recur(left.tail, right.tail, size + 1, 0)


    recur(left0, right0, 0, 0)


  transparent inline def parent: Optional[Path on Plane under Constraint] =
    inline !![Topic] match
      case head *: tail => Path.of[Plane, Constraint, tail.type](root, descent.tail*)
      case EmptyTuple   => Unset

      case _ =>
        if descent.isEmpty then Unset
        else Path.of[Plane, Constraint, Tuple](root, descent.tail*)

  def ancestors: List[Path on Plane under Constraint] =
    safely(parent).let { parent => parent :: parent.ancestors }.or(Nil)

  def child(value: Text)(using Unsafe): Path on Plane under Constraint =
    Path.of[Plane, Constraint, Text *: Topic](root, value +: descent*)

  @targetName("slash")
  transparent inline infix def / (child: Any): Path of (child.type *: Topic) under Constraint =
    summonFrom:
      case given ((? >: child.type) is Admissible on Plane) =>
        Path.of[Plane, Constraint, child.type *: Topic]
          (root, infer[child.type is Navigable].follow(child) +: descent*)

      case _ =>
        Path.unplatformed[Constraint, child.type *: Topic]
          (root, infer[child.type is Navigable].follow(child) +: descent*)


  transparent inline def peer(child: Any)(using child.type is Admissible on Plane)
  :     Path on Plane under Constraint =
    inline !![Topic] match
      case _: (head *: tail) =>
        Path.of[Plane, Constraint, child.type *: tail]
         (root, infer[child.type is Navigable].follow(child) +: descent*)

      case _ =>
        Path.of[Plane, Constraint, Tuple]
         (root, infer[child.type is Navigable].follow(child) +: descent*)

  transparent inline def + (relative: Relative): Path =
    type Base = Tuple.Reverse[Tuple.Take[Tuple.Reverse[Topic], relative.Constraint]]
    type Topic2 = Tuple.Concat[relative.Topic, Base]
    Path.of[Plane, Constraint, Topic2]
     (root, relative.descent ++ descent.drop(relative.ascent)*)
