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

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import prepositional.*
import spectacular.*
import vacuous.*

object Font:
  // FontError → Font.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case MissingTable(tag: Sfnt.Table.Tag)  extends Reason(1)
      case UnknownFormat(format: Int)   extends Reason(2)
      case MagicNumber                  extends Reason(3)
      case MissingEncoding              extends Reason(4)
      case UncoveredWeight(typeface: Text, weight: Weight)     extends Reason(5)
      case UncoveredSlant(typeface: Text, slant: Slant)        extends Reason(6)
      case UncoveredStretch(typeface: Text, stretch: Stretch)  extends Reason(7)
      case UnknownAxis(typeface: Text, axis: Variation.Axis)   extends Reason(8)

      case AxisOutOfRange(typeface: Text, axis: Variation.Axis, value: Double)
      extends Reason(9)

      case MissingFeature(typeface: Text, feature: Face.Feature) extends Reason(10)

    given communicable: Reason is Communicable =
      case Reason.MissingTable(tag)     => m"the table ${tag.text} was not found"
      case Reason.UnknownFormat(format) => m"the table contains data in unknown format $format"
      case Reason.MagicNumber           => m"the font did not contain expected check data"
      case Reason.MissingEncoding       => m"the font contains no usable character encoding"

      case Reason.UncoveredWeight(typeface, weight) =>
        m"$typeface is not provided at weight ${weight.show}"

      case Reason.UncoveredSlant(typeface, slant) =>
        m"$typeface is not provided in an ${slant.show} face"

      case Reason.UncoveredStretch(typeface, stretch) =>
        m"$typeface is not provided at a stretch of ${stretch.show}"

      case Reason.UnknownAxis(typeface, axis) =>
        m"$typeface has no variation axis ${axis.show}"

      case Reason.AxisOutOfRange(typeface, axis, value) =>
        m"$typeface does not admit ${value.toString} on its ${axis.show} axis"

      case Reason.MissingFeature(typeface, feature) =>
        m"$typeface does not implement the layout feature ${feature.show}"

  case class Error(reason: Font.Error.Reason)(using Diagnostics)
  extends fulminate.Error(564, reason.number)(m"the font could not be used because $reason")

  // Pairs a face with the provision in scope for its typeface, in the medium the expected type
  // names. The medium is a type member, so an expected type of `Font in Web` or
  // `Optional[Font in Web]` fixes it exactly — `Lettering(…, font = Font(Inter.bold))` needs
  // no annotation — and the provision is then requested `in (? >: medium)`, so one `in Medium`
  // serves every medium.
  //
  // A face the provision cannot set — a weight it lacks, a feature the file does not implement —
  // is refused. Where the provision's file is on the classpath (its `Locus`) and the face's
  // request is in its type, the refusal is a compile error; otherwise it is a `Font.Error` at
  // the pairing. Either way, it is never a synthesized or silently ignored rendering.
  inline def apply[family <: Label, medium <: Medium](inline face: Face of family)
    ( using provision: Typeface of family is Typesettable in (? >: medium),
            tactic:    Tactic[Font.Error] )
  :   Font in medium =

    ${phoenicia.internal.font[family, medium]('face, 'provision, 'tactic)}

  // The same pairing with the medium named, `Font.of[Web](face)`, for where the expected type is
  // not optional. The medium and the family are in separate clauses so that only the medium need
  // be written.
  def of[medium <: Medium]: Of[medium] = Of()

  class Of[medium <: Medium]():
    inline def apply[family <: Label](inline face: Face of family)
      ( using provision: Typeface of family is Typesettable in (? >: medium),
            tactic:    Tactic[Font.Error] )
    :   Font in medium =

      ${phoenicia.internal.font[family, medium]('face, 'provision, 'tactic)}

  // The runtime pairing, which the compile-time check expands to: the coverage check, then the
  // font.
  def paired[family <: Label, medium <: Medium](face: Face of family)
    ( using provision: Typeface of family is Typesettable in (? >: medium),
            tactic:    Tactic[Font.Error] )
  :   Font in medium =

    provision.coverage.complaint(face).let: reason => raise(Font.Error(reason))
    new Font(face, provision) { type Form = medium }

  given showable: [font <: Font] => font is Showable = _.face.show
  given inspectable: [font <: Font] => font is Inspectable = font => t"Font(${font.face.inspect})"

// A face together with the provision that makes its typeface available in a medium, which is
// the `Form`: `Font in Web`. A renderer accepts fonts only in this form, so a document cannot
// name a typeface it does not carry a provision for; the document collects the provisions of
// its fonts and emits whatever its medium needs — `@font-face` rules, an embedded program.
class Font(val face: Face, val provision: Typesettable) extends Formal:
  def typeface: Typeface = face.typeface
