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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package phoenicia

import scala.compiletime.asMatchable
import scala.quoted.*

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object internal:
  opaque type Glyph[font <: Sfnt & Singleton] = Int

  object Glyph:
    def apply(font: Sfnt, id: Int): Glyph[font.type] = id

  extension [font <: Sfnt & Singleton](glyph: Glyph[font])
    def id: Int = glyph

  object Weight:
    // CSS and OpenType's OS/2 table share the 1–1000 scale, so one type serves both. A value
    // outside the scale is brought to its nearer end: CSS would reject the declaration entirely,
    // which is a worse outcome than the nearest weight.
    def apply(value: Int): Weight = value.max(1).min(1000)

    val Thin: Weight = 100
    val ExtraLight: Weight = 200
    val Light: Weight = 300
    val Regular: Weight = 400
    val Medium: Weight = 500
    val Semibold: Weight = 600
    val Bold: Weight = 700
    val ExtraBold: Weight = 800
    val Black: Weight = 900

    // An inclusive span of weights, as a variable font's `wght` axis or a `@font-face` declaration
    // offers.
    case class Range(lower: Weight, upper: Weight):
      def contains(weight: Weight): Boolean = weight >= lower && weight <= upper

    extension (weight: Weight)
      def value: Int = weight
      infix def to(upper: Weight): Range = Range(weight, upper)
      def range: Range = Range(weight, weight)

    given showable: Weight is Showable = _.value.show
    given inspectable: Weight is Inspectable = weight => t"Weight(${weight.value.show})"

  opaque type Weight = Int

  object Stretch:
    // A percentage of a face's normal width, as CSS `font-stretch` measures it, from 50% to 200%.
    def apply(percentage: Double): Stretch = percentage.max(50.0).min(200.0)

    val UltraCondensed: Stretch = 50.0
    val ExtraCondensed: Stretch = 62.5
    val Condensed: Stretch = 75.0
    val SemiCondensed: Stretch = 87.5
    val Normal: Stretch = 100.0
    val SemiExpanded: Stretch = 112.5
    val Expanded: Stretch = 125.0
    val ExtraExpanded: Stretch = 150.0
    val UltraExpanded: Stretch = 200.0

    // The OS/2 table's `usWidthClass`, 1 to 9, names the same nine widths.
    def widthClass(value: Int): Stretch = value match
      case 1 => UltraCondensed
      case 2 => ExtraCondensed
      case 3 => Condensed
      case 4 => SemiCondensed
      case 6 => SemiExpanded
      case 7 => Expanded
      case 8 => ExtraExpanded
      case 9 => UltraExpanded
      case _ => Normal

    case class Range(lower: Stretch, upper: Stretch):
      def contains(stretch: Stretch): Boolean = stretch >= lower && stretch <= upper

    extension (stretch: Stretch)
      def percentage: Double = stretch
      infix def to(upper: Stretch): Range = Range(stretch, upper)
      def range: Range = Range(stretch, stretch)

    given showable: Stretch is Showable = stretch => t"${stretch.percentage.toString}%"

    given inspectable: Stretch is Inspectable =
      stretch => t"Stretch(${stretch.percentage.toString})"

  opaque type Stretch = Double

  // The compile-time check behind `Font(face)` and `Font.of[medium](face)`. When the provision's
  // type carries a `Locus` — the path of a font file the provision was made from — and that file
  // is on the compiler's classpath, it is read here, once per path, and the face's request, as far
  // as its type records it, is checked against the file's coverage; a refusal halts compilation
  // with the same message the runtime check would raise. A face or provision without static
  // information is left to the runtime check, which always runs.
  private val coverages: scala.collection.mutable.HashMap[Text, Optional[Coverage]] =
    scala.collection.mutable.HashMap()

  def font[family <: Label: Type, medium <: Medium: Type]
    ( face:      Expr[Face of family],
      provision: Expr[Typeface of family is Typesettable in (? >: medium)],
      tactic:    Expr[Tactic[Font.Error]] )
    ( using Quotes )
  :   Expr[Font in medium] =

    check(face, provision)
    '{Font.paired[family, medium]($face)(using $provision, $tactic)}

  // Every `type X = …` member of a (possibly nested) refinement type, as a map.
  private def refinements(using quotes: Quotes)(repr: quotes.reflect.TypeRepr)
  :   scala.collection.immutable.Map[Text, quotes.reflect.TypeRepr] =

    import quotes.reflect.*

    strip(repr) match
      case Refinement(parent, name, TypeBounds(_, hi)) => refinements(parent).updated(name.tt, hi)
      case Refinement(parent, name, info)              => refinements(parent).updated(name.tt, info)
      case AndType(left, right)                        => refinements(left) ++ refinements(right)
      case _                                           => scala.collection.immutable.Map()

  // Capture checking can wrap types in `AnnotatedType`s; strip them, and dealias.
  private def strip(using quotes: Quotes)(repr: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    repr.dealias.asMatchable match
      case AnnotatedType(parent, _) => strip(parent)
      case other                    => other

  private def check(using Quotes)(face: Expr[Face], provision: Expr[Typesettable]): Unit =
    import quotes.reflect.*

    val members = refinements(provision.asTerm.tpe) ++ refinements(provision.asTerm.tpe.widen)

    members.get(t"Locus").foreach: locus =>
      strip(locus).asMatchable match
        case ConstantType(StringConstant(path)) =>
          coverages.getOrElseUpdate(path.tt, read(path.tt)).let: coverage =>
            val request = refinements(face.asTerm.tpe) ++ refinements(face.asTerm.tpe.widen)

            val weight: Optional[Weight] = request.get(t"Weights").map(strip).flatMap:
              case ConstantType(IntConstant(value)) => Some(Weight(value))
              case _                                => None

            . getOrElse(Unset)

            val slant: Optional[Slant] = request.get(t"Slanting").map(strip).flatMap:
              case ConstantType(StringConstant("italic"))  => Some(Slant.Italic)
              case ConstantType(StringConstant("upright")) => Some(Slant.Upright)
              case _                                       => None

            . getOrElse(Unset)

            val enabled: List[Face.Feature.Setting] =
              request.get(t"Enabled").map(tags).getOrElse(Nil).map(Face.Feature(_).enabled)

            val name = refinements(face.asTerm.tpe.widen).get(t"Topic").map(strip).flatMap:
              case ConstantType(StringConstant(family)) => Some(family.tt)
              case _                                    => None

            . getOrElse(t"the typeface")

            coverage.complaint(name, weight, slant, Unset, Nil, enabled).let: reason =>
              halt(m"phoenicia: the font at $path cannot set this face: $reason")

        case _ =>
          ()

  // The literal tags in a union type such as `"onum" | "tnum"`; anything else in it — a feature
  // known only at runtime — is skipped.
  private def tags(using quotes: Quotes)(repr: quotes.reflect.TypeRepr): List[Text] =
    import quotes.reflect.*

    strip(repr).asMatchable match
      case OrType(left, right)               => tags(left) + tags(right)
      case ConstantType(StringConstant(tag)) => List(tag.tt)
      case _                                 => Nil

  // The coverage of the font file at a classpath path, or nothing if it is not there or cannot
  // be read, in which case the check is left to runtime.
  private def read(path: Text): Optional[Coverage] =
    Optional(getClass.getResourceAsStream(path.s)).let: stream =>
      val bytes = stream.readAllBytes().nn
      stream.close()
      Coverage.of(Sfnt(Array.unsafeFrozen(bytes)))
