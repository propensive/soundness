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
import denominative.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
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
      case TruncatedTable(tag: Sfnt.Table.Tag) extends Reason(11)

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

      case Reason.TruncatedTable(tag) =>
        m"the table ${tag.text} is shorter than its layout"

  case class Error(reason: Font.Error.Reason)(using Diagnostics)
  extends fulminate.Error(89, 1, reason.number)(m"the font could not be used because $reason")

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

  object Coverage:
    // One face a provision offers, or the range of faces a variable font offers: the weights and
    // widths it spans, whether it is upright, italic or both, the further axes it exposes, and
    // the layout features it implements. Features are unknown for a face declared without a file.
    case class Entry
      ( weights:   Weight.Range,
        upright:   Boolean,
        italic:    Boolean,
        stretches: Stretch.Range,
        axes:      List[Variation.Range],
        features:  Optional[List[Face.Feature]] ):

      def sets(face: Face): Boolean =
        weights.contains(face.weight) && (if face.slant.upright then upright else italic) &&
          stretches.contains(face.stretch)

      // The first variation or feature the face asks for that this entry cannot give.
      def complaint(name: Text, variations: List[Variation], settings: List[Face.Feature.Setting])
      :   Optional[Font.Error.Reason] =

        def axisComplaint(variation: Variation): Optional[Font.Error.Reason] =
          val ranges = rangesOf(variation.axis, axes)

          if ranges.nil then Font.Error.Reason.UnknownAxis(name, variation.axis)
          else if admits(ranges, variation.value) then Unset
          else Font.Error.Reason.AxisOutOfRange(name, variation.axis, variation.value)

        def featureComplaint(setting: Face.Feature.Setting): Optional[Font.Error.Reason] =
          features.lay[Optional[Font.Error.Reason]](Unset): available =>
            if available.has(setting.feature) then Unset
            else Font.Error.Reason.MissingFeature(name, setting.feature)

        val axisComplaints: List[Optional[Font.Error.Reason]] = variations.map(axisComplaint)
        val featureComplaints: List[Optional[Font.Error.Reason]] = settings.map(featureComplaint)

        first(axisComplaints + featureComplaints)

    // A declared coverage, for a source whose files are not to hand: a URL or an installed font.
    // Only what is declared is admitted, so a linked font declared at one weight rejects a request
    // for another rather than letting the browser synthesize it.
    def apply
      ( weights:   Weight.Range          = Weight.Regular.range,
        upright:   Boolean               = true,
        italic:    Boolean               = false,
        stretches: Stretch.Range         = Stretch.Normal.range,
        axes:      List[Variation.Range] = Nil )
    :   Coverage =

      Entries(List(Entry(weights, upright, italic, stretches, axes, Unset)))

    // What a font file offers, read from its tables: the `fvar` axes of a variable font, else the
    // single weight and width the OS/2 table records. Unreadable tables make the coverage unknown,
    // which admits everything, rather than rejecting faces the file may well have.
    def of(sfnt: Sfnt): Coverage = safely(entry(sfnt)).lay(Unknown): entry => Entries(List(entry))

    private def entry(sfnt: Sfnt): Entry raises Font.Error =
      val os2 = sfnt.os2
      val ranges: List[Variation.Range] = sfnt.fvar.lay(Nil)(axisRanges(_))

      def axis(axis: Variation.Axis): Optional[Variation.Range] = rangesOf(axis, ranges) match
        case head :: _ => head
        case _         => Unset

      val weights = axis(Variation.Axis.Weight).lay(Weight(os2.weightClass).range): range =>
        Weight(range.lower.toInt) to Weight(range.upper.toInt)

      val stretches = axis(Variation.Axis.Width).lay(Stretch.widthClass(os2.widthClass).range):
        range => Stretch(range.lower) to Stretch(range.upper)

      val slanting =
        axis(Variation.Axis.Italic).lay(false)(_.upper >= 1.0) || axis(Variation.Axis.Slant).present

      val italic = os2.italic || safely(sfnt.post.italicAngle).or(0.0) != 0.0

      Entry
        ( weights, upright = !italic || slanting, italic = italic || slanting, stretches, ranges,
          sfnt.features )

    private def axisRanges(fvar: Sfnt#FvarTable): List[Variation.Range] =
      def range(axis: fvar.Axis): Variation.Range =
        Variation.Range(Variation.Axis(axis.tag), axis.minimum, axis.maximum)

      fvar.axes.map(range)

    // The union of several files' coverage. Any unknown makes the whole unknown.
    def combine(coverages: List[Coverage]): Coverage =
      def recur(remaining: List[Coverage]): Optional[List[Entry]] = remaining match
        case Unknown :: _             => Unset
        case Entries(entries) :: tail => recur(tail).let(entries + _)
        case _                        => List()

      recur(coverages).lay(Unknown)(Entries(_))

    // List helpers written as plain recursion: a predicate lambda under a collection combinator is
    // where the compiler's `wildApprox` assertion fires (scala/scala3#24824).
    private def rangesOf(axis: Variation.Axis, ranges: List[Variation.Range])
    :   List[Variation.Range] =

      ranges match
        case head :: tail =>
          if head.axis.tag == axis.tag then List(head) + rangesOf(axis, tail)
          else rangesOf(axis, tail)

        case _ =>
          Nil

    private def admits(ranges: List[Variation.Range], value: Double): Boolean = ranges match
      case head :: tail => head.contains(value) || admits(tail, value)
      case _            => false

    private def first[element](list: List[Optional[element]]): Optional[element] = list match
      case head :: tail => head.or(first(tail))
      case _            => Unset

    private def select(entries: List[Entry], predicate: Entry => Boolean): List[Entry] =
      entries match
        case head :: tail =>
          if predicate(head) then List(head) + select(tail, predicate) else select(tail, predicate)

        case _ =>
          Nil

    private def anyAbsent(complaints: List[Optional[Font.Error.Reason]]): Boolean = complaints match
      case head :: tail => head.absent || anyAbsent(tail)
      case _            => false

  enum Coverage derives CanEqual:
    // Nothing is known of the faces a source offers, so every face is admitted.
    case Unknown
    case Entries(entries: List[Coverage.Entry])

    // Why the face cannot be set from this coverage, or nothing if it can. The weight, slant and
    // stretch are matched first, as a browser matches them, and the variations and features are
    // checked against the entries that matched.
    def complaint(face: Face): Optional[Font.Error.Reason] =
      complaint(face.typeface.name, face.weight, face.slant, face.stretch, face.variations,
          face.features)

    // The same check over only what is known: a part given as `Unset` is not checked. This is the
    // form the compile-time check uses, where a face's weight or slant may be decided at runtime.
    def complaint
      ( name:       Text,
        weight:     Optional[Weight],
        slant:      Optional[Slant],
        stretch:    Optional[Stretch],
        variations: List[Variation],
        features:   List[Face.Feature.Setting] )
    :   Optional[Font.Error.Reason] =

      this match
        case Unknown => Unset

        case Entries(entries) =>
          def weighs(entry: Coverage.Entry): Boolean =
            weight.lay(true)(entry.weights.contains(_))

          def slants(entry: Coverage.Entry): Boolean =
            slant.lay(true): slant => if slant.upright then entry.upright else entry.italic

          def stretches(entry: Coverage.Entry): Boolean =
            stretch.lay(true)(entry.stretches.contains(_))

          val byWeight = Coverage.select(entries, weighs)

          if byWeight.nil then Font.Error.Reason.UncoveredWeight(name, weight.or(Weight.Regular))
          else
            val bySlant = Coverage.select(byWeight, slants)

            if bySlant.nil then Font.Error.Reason.UncoveredSlant(name, slant.or(Slant.Upright))
            else
              val byStretch = Coverage.select(bySlant, stretches)

              if byStretch.nil
              then Font.Error.Reason.UncoveredStretch(name, stretch.or(Stretch.Normal))
              else
                def complaintOf(entry: Coverage.Entry): Optional[Font.Error.Reason] =
                  entry.complaint(name, variations, features)

                val complaints: List[Optional[Font.Error.Reason]] = byStretch.map(complaintOf)

                if Coverage.anyAbsent(complaints) then Unset
                else complaints match
                  case head :: _ => head
                  case _         => Unset

class Font(val face: Face, val provision: Typesettable) extends Formal:
  def typeface: Typeface = face.typeface
