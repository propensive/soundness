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
package serpentine

import scala.compiletime.*

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object Path:
  inline given pathOnLinux: (Path on Linux) is Representative of Paths = !!
  inline given pathOnWindows: (Path on Windows) is Representative of Paths = !!
  inline given pathOnMacOs: (Path on MacOs) is Representative of Paths = !!
  inline given pathOnLocal: (Path on Local) is Representative of Paths = !!

  @targetName("Root")
  object % extends Path(t"/"):
    type Topic = EmptyTuple
    type Limit = %.type


  given decodable: [filesystem: Filesystem, root] => (radical: root is Radical on filesystem)
  =>  (Path on filesystem) is Decodable in Text =

    text =>
      val root = radical.encode(radical.decode(text))
      val parts = text.skip(radical.length(text)).cut(filesystem.separator)
      val parts2 = if parts.last == t"" then parts.init else parts

      Path.of(root, parts2.reverse.map(filesystem.unescape(_))*)


  given decodable2: [filesystem: Filesystem, root] => (radical: root is Radical on filesystem)
  =>  (Path on filesystem under root) is Decodable in Text =

    text =>
      val root = radical.encode(radical.decode(text))
      val parts = text.skip(radical.length(text)).cut(filesystem.separator)
      val parts2 = if parts.last == t"" then parts.init else parts

      Path.of(root, parts2.reverse.map(filesystem.unescape(_))*)


  given nominable: [filesystem] => (Path on filesystem) is Nominable = path =>
    path.descent.prim.or(path.root)


  given trustedInstantiable: [filesystem: Filesystem]
  =>  ( radical: Tactic[PathError] ?=> Radical on filesystem )
  =>  (Path on filesystem) is Instantiable across Paths from Paths.Trusted =

    given Radical on filesystem = radical(using strategies.throwUnsafely)
    _.text.decode[Path on filesystem]


  given instantiable: [filesystem: Filesystem]
  =>  Radical on filesystem
  =>  (Path on filesystem) is Instantiable across Paths from Text =

    _.decode[Path on filesystem]


  def unplatformed[root, topic <: Tuple](root: Text, descent: Text*): Path of topic under root =
    new Path(root, descent*):
      type Topic = topic
      type Limit = root


  def of[filesystem, root, topic <: Tuple](root: Text, descent: Text*)
  :   Path on filesystem of topic under root =

    new Path(root, descent*):
      type Plane = filesystem
      type Limit = root
      type Topic = topic


  given encodable: [filesystem: Filesystem] => Path on filesystem is Encodable in Text =
    path =>
      path.descent.map(filesystem.escape(_)).reverse.join(path.root, filesystem.separator, t"")

  given showable: [filesystem: Filesystem] => Path on filesystem is Showable = _.encode

  given communicable: [filesystem: Filesystem] => Path on filesystem is Communicable =
    path => Message(path.encode)


  given generic: [filesystem: Filesystem, path <: Path on filesystem]
  =>  path is Abstractable across Paths to Text =

    _.encode


  private def conversion[from, to](lambda: from => to): Conversion[from, to] = lambda(_)


  inline given convert: [topic, root, filesystem, path <: Path of topic under root]
  =>  Conversion[path, Path of topic on filesystem under root] =

    conversion(_.on[filesystem])


  transparent inline given quotient: [filesystem, root, path <: Path on filesystem under root]
  =>  ( radical: root is Radical on filesystem )
  =>  path is Quotient =

    ( path =>
        if path.empty then None
        else if path.descent.length == 1 then Some((radical.decode(path.root), path.descent.head))
        else Some((radical.decode(path.root), Relative(0, path.descent*))) )
    :   path is Quotient of root over (Relative on filesystem) | Text


case class Path(root: Text, descent: Text*) extends Limited, Topical, Planar:
  type Topic <: Tuple

  def name: Text = descent.prim.or(root)
  def empty: Boolean = descent.nil

  inline def knownElementTypes: Boolean = inline !![Topic] match
    case _: Zero           => true
    case _: (head *: tail) => true
    case _                 => false

  transparent inline def knownElements: Boolean = known[Topic]

  protected inline def known[topic <: Tuple]: Boolean =
    inline !![topic] match
      case _: Zero => true

      case _: (head *: tail) =>
        inline constValueOpt[head] match
          case None               => false
          case Some(value)        => known[tail]

      case _ =>
        false


  def resolve(text: Text)
    ( using (Path on Plane) is Decodable in Text, (Relative on Plane) is Decodable in Text )
  :   Path on Plane raises PathError =

    safely(text.decode[Path on Plane]).or(safely(this + text.decode[Relative on Plane])).or:
      abort(PathError(_.InvalidRoot))


  def precedes(path: Path on Plane): Boolean =
    path.root == root && path.descent.drop(path.descent.length - descent.length) == descent

  transparent inline def depth: Int = inline !![Topic] match
    case Zero         => 0
    case head *: tail => valueOf[Tuple.Size[Topic]]
    case _            => descent.length

  private inline def check[topic, filesystem](path: List[Text]): Unit =
    inline !![topic] match
      case _: Zero => ()

      case _: (head *: tail) =>
        infer[head is Admissible on filesystem].check(path.head)
        check[tail, filesystem](path.tail)

      case _ =>
        path.each: element =>
          infer[Text is Admissible on filesystem].check(element)

  inline def on[filesystem]: Path of Topic under Limit on filesystem = summonFrom:
    case given (`filesystem` =:= Plane) =>
      this.asInstanceOf[Path of Topic under Limit on filesystem]

    case _ =>
      check[Topic, filesystem](descent.to(List))

      summonFrom:
        case limit: (Limit is Submissible on `filesystem`)                 => limit.check(root)
        case radical: (Limit is Radical on `filesystem`)                   => radical.decode(root)
        case filesystem: (`filesystem` is (Filesystem { type UniqueRoot = true })) =>
          infer[Plane is (Filesystem { type UniqueRoot = true })]

      this.asInstanceOf[Path of Topic under Limit on filesystem]

  def graft[radical: Radical on Plane](root: radical): Path of Topic under root.type =
    Path.of(radical.encode(root), descent*)

  def shift(n: Int): Path on Plane under Limit =
    Path.of(root, descent.take(depth - n)*)

  transparent inline def sameRoot(right: Path): Boolean = summonFrom:
    case plane: (Plane is Filesystem) =>
      inline if caps.unsafe.unsafeErasedValue[plane.UniqueRoot] then true else root == right.root

    case _ =>
      root == right.root

  transparent inline def rename(lambda: (prior: Text) ?=> Text): Optional[Path] =
    parent.let: parent =>
      descent.prim.let(parent / lambda(using _))

  def relative: Relative of Topic on Plane under 0 =
    Relative[Plane, Topic, 0](0, descent*)

  private[serpentine] def calculate(right: Path): Path =
    val difference = depth - right.depth
    val left0 = descent.drop(difference).to(List)
    val right0 = right.descent.drop(-difference).to(List)


    def recur(left: List[Text], right: List[Text], size: Int, count: Int)
    :   Path on Plane =

      if left.nil then Path.of(root, left0.drop(size - count)*)
      else if left.head == right.head then recur(left.tail, right.tail, size + 1, count + 1)
      else recur(left.tail, right.tail, size + 1, 0)


    recur(left0, right0, 0, 0)

  transparent inline def parent: Optional[Path on Plane under Limit] =
    inline !![Topic] match
      case head *: tail => Path.of[Plane, Limit, tail.type](root, descent.tail*)
      case EmptyTuple   => Unset

      case _ =>
        if descent.nil then Unset
        else Path.of[Plane, Limit, Tuple](root, descent.tail*)

  def ancestors: List[Path on Plane under Limit] =
    safely(parent).let { parent => parent :: parent.ancestors }.or(Nil)

  def child(value: Text)(using erased Unsafe): Path on Plane under Limit =
    Path.of[Plane, Limit, Text *: Topic](root, value +: descent*)

  @targetName("slash")
  transparent inline infix def / (child: Any): Path of (child.type *: Topic) under Limit =
    summonFrom:
      case given ((? >: child.type) is Admissible on Plane) =>
        Path.of[Plane, Limit, child.type *: Topic]
          (root, infer[child.type is Navigable on Plane].follow(child) +: descent*)

      case _ =>
        Path.unplatformed[Limit, child.type *: Topic]
          (root, infer[child.type is Navigable on Plane].follow(child) +: descent*)


  transparent inline def peer(child: Any)(using child.type is Admissible on Plane)
  :   Path on Plane under Limit =

    inline caps.unsafe.unsafeErasedValue[Topic] match
      case _: (head *: tail) =>
        Path.of[Plane, Limit, child.type *: tail]
          ( root, infer[child.type is Navigable on Plane].follow(child) +: descent* )

      case _ =>
        Path.of[Plane, Limit, Tuple]
          ( root, infer[child.type is Navigable on Plane].follow(child) +: descent* )


  transparent inline def + (relative: Relative): Path =
    type Base = Tuple.Reverse[Tuple.Take[Tuple.Reverse[Topic], relative.Limit]]
    type Topic2 = Tuple.Concat[relative.Topic, Base]
    Path.of[Plane, Limit, Topic2]
      ( root, relative.descent ++ descent.drop(relative.ascent)* )
