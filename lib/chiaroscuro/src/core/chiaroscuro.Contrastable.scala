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
package chiaroscuro

import anticipation.*
import dissonance.*
import gossamer.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*
import wisteria.*

import scala.deriving.*
import scala.reflect.*
import java.text.CollationKey

trait Contrastable:
  type Self
  def contrast(left: Self, right: Self): Juxtaposition

object Contrastable:
  given int: Int is Contrastable = (left, right) =>
    if left == right then Juxtaposition.Same(left.show)
    else Juxtaposition.Different(left.show, right.show, t"${math.abs(right - left)}")

  given Double is Contrastable = (left, right) =>
    given Decimalizer = Decimalizer(3)
    if left == right then Juxtaposition.Same(left.show)
    else
      val size = 100*(right - left)/left
      val sizeText = if size.isFinite then t"${if size > 0 then t"+" else t""}$size%" else t""
      Juxtaposition.Different(left.show, right.show, sizeText)

  given Char is Contrastable = (left, right) =>
    if left == right then Juxtaposition.Same(left.show)
    else Juxtaposition.Different(left.show, right.show)

  given Text is Contrastable =
    (left, right) =>
      def decompose(chars: IArray[Char]): IArray[Decomposition] = chars.map: char =>
        Decomposition.Primitive(t"Char", char.show, char)
      compareSeq[Char](decompose(left.chars), decompose(right.chars), left, right)

  inline def nothing[ValueType]: ValueType is Contrastable = (left, right) =>
    compiletime.summonInline[ValueType is Decomposable].give:
      Juxtaposition.Same(left.decompose.text)

  given [ValueType: Decomposable] => ValueType is Contrastable = (left, right) =>
    juxtaposition(left.decompose, right.decompose)

  def juxtaposition(left: Decomposition, right: Decomposition): Juxtaposition =
    if left.ref == right.ref then Juxtaposition.Same(left.text) else (left, right) match
      case (Decomposition.Primitive(_, left, lRef), Decomposition.Primitive(_, right, rRef)) =>
        Juxtaposition.Different(left, right)

      case (Decomposition.Sequence(left, _), Decomposition.Sequence(right, _)) =>
        compareSeq(left, right, t"", t"")

      case (Decomposition.Product(_, left, _), Decomposition.Product(_, right, _)) =>
        Juxtaposition.Collation
         (IArray.from:
            left.keys.map: key =>
              key -> juxtaposition(left(key), right(key)),
          t"",
          t"")

  given Exception is Contrastable:
    def contrast(left: Exception, right: Exception): Juxtaposition =
      val leftMsg = Option(left.getMessage).fold(t"null")(_.nn.tt)
      val rightMsg = Option(right.getMessage).fold(t"null")(_.nn.tt)

      if left.getClass == right.getClass && leftMsg == rightMsg then Juxtaposition.Same(leftMsg)
      else Juxtaposition.Different(leftMsg, rightMsg)

  def compareSeq[ValueType]
     (left: IArray[Decomposition], right: IArray[Decomposition], leftDebug: Text, rightDebug: Text)
  :     Juxtaposition =
    if left == right then Juxtaposition.Same(leftDebug) else
      val comparison = IArray.from:
        diff(left, right).rdiff(_ == _).changes.map:
          case Par(leftIndex, rightIndex, value) =>
            val label =
              if leftIndex == rightIndex then leftIndex.show
              else t"${leftIndex.show.superscripts}⫽${rightIndex.show.subscripts}"

            label -> Juxtaposition.Same(value.let(_.text).or(t"?"))

          case Ins(rightIndex, value) =>
            t" ⧸${rightIndex.show.subscripts}"
            -> Juxtaposition.Different(t"—", value.text)

          case Del(leftIndex, value) =>
            t"${leftIndex.show.superscripts}⧸"
            -> Juxtaposition.Different(value.let(_.text).or(t"?"), t"—")

          case Sub(leftIndex, rightIndex, leftValue, rightValue) =>
            val label = t"${leftIndex.show.superscripts}⫽${rightIndex.show.subscripts}"

            label -> juxtaposition(Decomposition(leftValue), Decomposition(rightValue))

      Juxtaposition.Collation(comparison, leftDebug, rightDebug)

  // inline given collection: [CollectionType <: Iterable, ValueType: Contrastable]
  // =>    CollectionType[ValueType] is Contrastable:
  //   def apply(left: CollectionType[ValueType], right: CollectionType[ValueType]): Juxtaposition =
  //     compareSeq[ValueType]
  //      (left.to(IndexedSeq), right.to(IndexedSeq), left.inspect, right.inspect)
