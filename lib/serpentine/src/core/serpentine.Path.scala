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
┃    Soundness, version 0.32.0.                                                                    ┃
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

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import scala.compiletime.*

object Path:
  given encodable: [path <: Path] => path is Encodable in Text = _.text

  given decoder: [platform: {Navigable, Radical}] => (Path on platform) is Decodable in Text =
    Path.parse(_)

  given showable: [path <: Path] => path is Showable = _.text
  given generic: [platform] => (Path on platform) is Abstractable across Paths into Text = _.text

  given nominable: [platform] => (Path on platform) is Nominable = path =>
    path.textDescent.prim.or(path.textRoot)

  given specific: [platform: {Navigable, Radical}]
        =>  Path on platform is Instantiable across Paths from Text =
    _.decode[Path on platform]

  given addable: [platform,
                  operand,
                  relative <: Relative by operand,
                  path <: Path on platform]
        => Tactic[PathError]
        => (navigable: platform is Navigable)
        => operand <:< navigable.Operand
        => path is Addable:
    type Self = path
    type Operand = relative
    type Result = Path on platform

    def add(left: path, right: relative): Path on platform =
      def recur(descent: List[Text], ascent: Int): Path on platform =
        if ascent > 0 then
          if descent.isEmpty then
            abort(PathError(PathError.Reason.RootParent))

            Path.from[platform]
             (left.textRoot, Nil, left.separator, left.caseSensitivity)
          else recur(descent.tail, ascent - 1)
        else
          Path.from[platform]
           (left.textRoot,
            right.textDescent ++ descent,
            navigable.separator,
            navigable.caseSensitivity)

      recur(left.textDescent, right.ascent)


  def apply[root <: Root on platform, element, platform: {Navigable by element, Radical from root}]
       (root0: root, elements: List[element])
  : Path on platform =

      if elements.isEmpty then root0 else
        Path.from[platform]
         (platform.rootText(root0),
          elements.map(platform.makeElement(_)),
          platform.separator,
          platform.caseSensitivity)


  private def from[platform]
               (root0: Text, elements: List[Text], separator: Text, caseSensitivity: Case)
  : Path on platform =

      new Path(root0, elements, separator, caseSensitivity):
        type Platform = platform


  def parse[platform: {Navigable, Radical}](path: Text): Path on platform =
    val root = platform.root(path)

    val descent =
      path
      . skip(platform.rootLength(path))
      . cut(platform.separator)
      . filter(_ != t"")
      . reverse
      . map(platform.element(_))

    Path(root, descent)

  given divisible: [platform, operand, path <: Path on platform]
               => (navigable: platform is Navigable by operand)
               => path is Divisible by operand into (Path on platform) =
    new Divisible:
      type Operand = operand
      type Self = path
      type Result = Path on platform

      def divide(path: path, child: operand): Path on platform =
        Path.from[path.Platform]
         (path.textRoot,
          navigable.makeElement(child) :: path.textDescent,
          navigable.separator,
          navigable.caseSensitivity)

open class Path
   (val textRoot: Text,
    val textDescent: List[Text],
    val separator: Text,
    val caseSensitivity: Case)
extends Pathlike:
  type Platform

  def depth: Int = textDescent.length
  def root(using radical: Platform is Radical): radical.Source = radical.root(textRoot)
  def text: Text = textDescent.reverse.join(textRoot, separator, t"")

  def name(using navigable: Platform is Navigable): Optional[navigable.Operand] =
    textDescent.prim.let(navigable.element(_))

  def peer(using navigable: Platform is Navigable)(name: navigable.Operand)
  : Path on Platform raises PathError =
    parent.let(_ / name).lest(PathError(PathError.Reason.RootParent))

  def descent(using navigable: Platform is Navigable): List[navigable.Operand] =
    textDescent.reverse.map(navigable.element(_))

  def child(filename: Text)(using Unsafe): Path on Platform =
    Path.from(textRoot, filename :: textDescent, separator, caseSensitivity)

  override def toString(): String = text.s

  override def equals(that: Any): Boolean = that.asMatchable match
    case that: Path =>
      (textRoot == that.textRoot) && caseSensitivity.equal(textDescent, that.textDescent)

    case _ =>
      false

  override def hashCode: Int =
    separator.hashCode + textRoot.toString.hashCode*31 + caseSensitivity.hash(textDescent)

  def parent: Optional[Path on Platform] =
    if textDescent == Nil then Unset
    else Path.from(textRoot, textDescent.tail, separator, caseSensitivity)

  def ancestor(n: Int): Optional[Path on Platform] =
    def recur(n: Int, current: Optional[Path on Platform]): Optional[Path on Platform] =
      current.let: current =>
        if n == 0 then current else recur(n - 1, current.parent)

    recur(n, this)

  def ancestors: List[Path on Platform] =
    parent.let { parent => parent :: parent.ancestors }.or(Nil)

  transparent inline def on [platform]: Path on platform =
    inline erasedValue[platform & Matchable] match
      case _: Platform => this.asInstanceOf[Path on platform]
      case _ =>
        summonInline[platform is Navigable].give:
          summonInline[platform is Radical].give(Path.parse(text))

  def conjunction(right: Path on Platform): Path on Platform =
    val difference = depth - right.depth
    val left0 = textDescent.drop(difference)
    val right0 = right.textDescent.drop(-difference)

    def recur(left: List[Text], right: List[Text], size: Int, count: Int): Path on Platform =
      if left.isEmpty
      then Path.from(textRoot, left0.drop(size - count), separator, caseSensitivity)
      else if left.head == right.head then recur(left.tail, right.tail, size + 1, count + 1)
      else recur(left.tail, right.tail, size + 1, 0)

    recur(left0, right0, 0, 0)

  def precedes(path: Path on Platform): Boolean = textDescent == Nil || conjunction(path) == path

  def retain(count: Int): Path on Platform =
    Path.from
     (textRoot, textDescent.drop(depth - count), separator, caseSensitivity)


  def relativeTo(right: Path on Platform)(using navigable: Platform is Navigable)
  : Relative by navigable.Operand raises PathError =

      if textRoot != right.textRoot then abort(PathError(PathError.Reason.DifferentRoots))
      val common = conjunction(right).depth
      Relative(right.depth - common, textDescent.dropRight(common).map(navigable.element(_)))


  def resolve(text: Text)(using navigable: Platform is Navigable, radical: Platform is Radical)
  : Path on Platform raises PathError =

      def relative: Relative by navigable.Operand = Relative.parse(text)
      val path: Optional[Path on Platform] = safely(Path.parse[Platform](text))
      val base: Path on Platform = this

      path.or(safely(base + relative)).or:
        abort(PathError(PathError.Reason.InvalidRoot))
