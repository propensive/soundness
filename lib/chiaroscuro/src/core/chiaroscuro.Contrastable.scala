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
package chiaroscuro

import anticipation.*
import dissonance.*
import gossamer.*
import hieroglyph.*
import hypotenuse.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*
import wisteria.*

import scala.reflect.*
import scala.compiletime.*

trait Contrastable extends Typeclass:
  def juxtaposition(left: Self, right: Self): Juxtaposition

object Contrastable:

  inline given derived: [entity] => entity is Contrastable = summonFrom:
    case contrastable: (`entity` is Contrastable.Foundation) => contrastable
    case given ProductReflection[`entity`]                   => Derivation.derived[entity]
    case given SumReflection[`entity`]                       => Derivation.split[entity]

    case given (`entity` is Decomposable) =>
      (left, right) => juxtaposition(left.decompose, right.decompose)

  object Derivation extends Derivable[Contrastable]:
    inline def join[derivation <: Product: ProductReflection]: derivation is Contrastable =
      (left, right) =>
        def show(value: derivation) = summonFrom:
          case given (`derivation` is Showable)          => value.show
          case given (`derivation` is Encodable in Text) => value.encode
          case _                                         => value.toString.tt

        if left == right then Juxtaposition.Same(show(left)) else
          val map = contexts: [field] =>
            context => label -> context.juxtaposition(dereference(left), dereference(right))

          Juxtaposition.Collation(map.to(List), show(left), show(right))

    inline def split[derivation: SumReflection]: derivation is Contrastable =
      (left, right) =>
        def show(value: derivation) = summonFrom:
          case given (`derivation` is Showable)          => value.show
          case given (`derivation` is Encodable in Text) => value.encode
          case _                                         => value.toString.tt

        if left == right then Juxtaposition.Same(show(left)) else
          variant(left): [variant <: derivation] =>
            left2 =>
              complement(right).let: right2 =>
                context.juxtaposition(left2, right2)

              . or:
                  val leftName: Text = typeName
                  variant(right): [rightVariant <: derivation] =>
                    right2 =>
                      val rightName: Text = typeName
                      Juxtaposition.Different(leftName, rightName)


  trait Foundation extends Contrastable:
    def juxtaposition(left: Self, right: Self): Juxtaposition

  object Foundation extends Foundation2:
    given decomposition: Decomposition is Contrastable.Foundation = juxtaposition(_, _)

    given set: [element: Showable] => Set[element] is Contrastable.Foundation = (left, right) =>
      if left == right then Juxtaposition.Same(left.show) else
        val leftOnly = (left -- right).map(_.show)
        val rightOnly = (right -- left).map(_.show)

        def describe(set: Set[Text]): Text =
          (if set.size > 5 then set.take(4).to(List) :+ t"…${(set.size - 4).show.subscripts}" else set.to(List))
          . join(t"{", t", ", t"}")

        val message =
          if leftOnly.isEmpty then t"+${describe(rightOnly)}"
          else if rightOnly.isEmpty then t"-${describe(leftOnly)}"
          else t"-${describe(leftOnly)}╱+${describe(rightOnly)}"

        Juxtaposition.Different(left.show, right.show, message)

    given exception: Exception is Contrastable.Foundation:
      def juxtaposition(left: Exception, right: Exception): Juxtaposition =
        val leftMsg = Option(left.getMessage).fold(t"null")(_.nn.tt)
        val rightMsg = Option(right.getMessage).fold(t"null")(_.nn.tt)

        if left.getClass == right.getClass && leftMsg == rightMsg then Juxtaposition.Same(leftMsg)
        else Juxtaposition.Different(leftMsg, rightMsg)

    given double: Double is Contrastable.Foundation = (left, right) =>
      given Decimalizer(3)
      if left == right then Juxtaposition.Same(left.show)
      else
        val difference =
          given Decimalizer(plusSign = '+', significantFigures = 4)
          (right - left).show

        val (left2, right2) =
          given Decimalizer
                (decimalPlaces = (-log10((left - right).abs)).ceiling.double.toInt,
                  exponentThreshold = Unset)
          (left.show, right.show)

        Juxtaposition.Different(left2, right2, difference)

    given int: Int is Contrastable.Foundation = (left, right) =>
      if left == right then Juxtaposition.Same(left.show)
      else
        val plus = if right > left then t"+" else t""
        Juxtaposition.Different(left.show, right.show, t"$plus${(right - left)}")

    given text: Text is Contrastable.Foundation =
      (left, right) =>
        if left == right then Juxtaposition.Same(left) else
          def decompose(chars: IArray[Char]): IArray[Decomposition] = chars.map: char =>
             Decomposition.Primitive(t"Char", char.show, char)
          comparison[Char](decompose(left.chars), decompose(right.chars), left, right)

  inline def nothing[value]: value is Contrastable = (left, right) =>
    provide[value is Decomposable]:
      Juxtaposition.Same(left.decompose.text)

  def juxtaposition(left: Decomposition, right: Decomposition): Juxtaposition =
    if left.ref == right.ref then Juxtaposition.Same(left.text) else (left, right) match
      case (Decomposition.Primitive(_, left, lRef), Decomposition.Primitive(_, right, rRef)) =>
        Juxtaposition.Different(left, right)

      case (Decomposition.Sequence(name, left, _), Decomposition.Sequence(rightName, right, _)) =>
        comparison(IArray.from(left), IArray.from(right), name, rightName)

      case (Decomposition.Product(leftName, left, _), Decomposition.Product(rightName, right, _)) =>
        Juxtaposition.Collation
         (left.keys.to(List).map: key =>
            key -> juxtaposition(left(key), right(key)),
          leftName,
          rightName)

      case (left, right) =>
        def kind(value: Decomposition): Text = value match
          case Decomposition.Primitive(_, _, _) => t"<primitive>"
          case Decomposition.Sequence(_, _, _)  => t"<sequence>"
          case Decomposition.Product(_, _, _)   => t"<product>"
          case Decomposition.Sum(_, _, _)       => t"<sum>"

        Juxtaposition.Different(kind(left), kind(right))

  def comparison[value]
       (left:       IArray[Decomposition],
        right:      IArray[Decomposition],
        leftDebug:  Text,
        rightDebug: Text)
  : Juxtaposition =

      if left == right then Juxtaposition.Same(leftDebug) else
        val comparison = IArray.from:
          diff(left, right).rdiff(_ == _, 10).changes.map:
            case Par(leftIndex, rightIndex, value) =>
              val label =
                if leftIndex == rightIndex then leftIndex.show
                else t"${leftIndex.show.superscripts}╱${rightIndex.show.subscripts}"

              label -> Juxtaposition.Same(value.let(_.short).or(t"?"))

            case Ins(rightIndex, value) =>
              t" ╱${rightIndex.show.subscripts}"
              -> Juxtaposition.Different(t"", value.short)

            case Del(leftIndex, value) =>
              t"${leftIndex.show.superscripts}╱ "
              -> Juxtaposition.Different(value.let(_.short).or(t"?"), t"")

            case Sub(leftIndex, rightIndex, leftValue, rightValue) =>
              val label = t"${leftIndex.show.superscripts}╱${rightIndex.show.subscripts}"

              label -> juxtaposition(Decomposition(leftValue), Decomposition(rightValue))

        Juxtaposition.Collation(comparison.to(List), leftDebug, rightDebug)

  trait Foundation2:
    given showable: [value: Showable] => value is Contrastable = (left, right) =>
      if left == right then Juxtaposition.Same(left.show)
      else Juxtaposition.Different(left.show, right.show)
