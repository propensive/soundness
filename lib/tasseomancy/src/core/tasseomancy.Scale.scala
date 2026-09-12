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
package tasseomancy

import anticipation.*
import denominative.*
import gossamer.*
import hypotenuse.*
import rudiments.*
import vacuous.*

object Scale:
  enum Transform:
    case Linear, Logarithmic

  // How gradations are spaced along a linear scale: at multiples of 1, 2 and 5 across the
  // decades, as numbers are read, or at the sexagesimal steps of clock time.
  enum Spacing:
    case Decimal, Sexagesimal

  // How a gradation's value is written: as a number with a suffix, as an interval (`250ms`,
  // `2m30s`), or as a time of day.
  enum Labelling:
    case Number(suffix: Text)
    case Interval
    case Clock

  // How an axis of some type is read: how its gradations are spaced and written, what the
  // quantity is called (`distance`, `time`), and the unit its values are in (`m`, `kg·s⁻²`).
  // The name and unit make the axis title — `distance / m` — unless the style gives a title, in
  // which case the unit is appended to it.
  case class Notation
    ( spacing:   Spacing        = Spacing.Decimal,
      labelling: Labelling      = Labelling.Number(t""),
      name:      Optional[Text] = Unset,
      unit:      Optional[Text] = Unset ):

    def title(supplied: Optional[Text]): Optional[Text] =
      supplied.or(name).let { text => unit.lay(text) { unit => t"$text / $unit" } }.or(unit)

  // The slack allowed when a value is compared against a bound it was computed from, so that a
  // gradation at the end of a scale is not lost to a rounding error.
  private[tasseomancy] val tolerance: Double = 0.000000001

  // The steps of clock time that gradations may take, in seconds, from a millisecond up to a
  // day; beyond a day, days are counted decimally.
  private val sexagesimal: Sequence[Double] =
    Sequence
      ( 0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0, 15.0, 30.0,
        60.0, 120.0, 300.0, 600.0, 900.0, 1800.0, 3600.0, 7200.0, 10800.0, 21600.0, 43200.0,
        86400.0 )

  // The smallest of 1, 2 and 5 times a power of ten that is no less than `raw`.
  def step(raw: Double): Double =
    if raw <= 0.0 then 1.0 else
      val magnitude = 10.0 ** log10(raw).double.floor
      val ratio = raw/magnitude

      val factor =
        if ratio <= 1.0 then 1.0
        else if ratio <= 2.0 then 2.0
        else if ratio <= 5.0 then 5.0
        else 10.0

      factor*magnitude

  def sexagesimalStep(raw: Double): Double =
    if raw < 0.001 then step(raw) else sexagesimal.seek(_ >= raw).or(step(raw/86400.0)*86400.0)

  // The decimal places needed to write multiples of `step` exactly.
  def decimals(step: Double): Int =
    if step >= 1.0 then 0 else (-(log10(step).double + tolerance).floor).toInt

  // A number to a fixed count of decimal places, without a decimal converter: gradation labels
  // are multiples of a known step, so the places are known rather than chosen.
  def format(value: Double, decimals: Int): Text =
    val factor = 10.0 ** decimals.toDouble
    val scaled: Long = (value*factor).round

    if scaled == 0L then t"0" else
      val negative = scaled < 0L
      var digits: Text = (if negative then -scaled else scaled).toString.tt
      while digits.length < decimals + 1 do digits = t"0$digits"

      val body =
        if decimals == 0 then digits
        else t"${digits.keep(digits.length - decimals)}.${digits.skip(digits.length - decimals)}"

      if negative then t"-$body" else body

  // The shortest exact rendering of a value to at most three decimal places.
  private def trim(value: Double): Text =
    val thousandths = (value*1000.0).round

    val places =
      if thousandths%1000L == 0L then 0
      else if thousandths%100L == 0L then 1
      else if thousandths%10L == 0L then 2
      else 3

    format(value, places)

  // An interval in the unit that keeps it short: `250ms`, `41s`, `2m30s`, `1h05m`, `3d02h`.
  def interval(seconds: Double): Text =
    val sign = if seconds < 0.0 then t"-" else t""
    val abs = seconds.abs

    def pair(major: Double, majorUnit: Text, minor: Double, minorUnit: Text): Text =
      val minorText = format(minor, 0)
      val padded = if minorText.length < 2 then t"0$minorText" else minorText

      if minor < 0.5 then t"$sign${format(major, 0)}$majorUnit"
      else t"$sign${format(major, 0)}$majorUnit$padded$minorUnit"

    if abs == 0.0 then t"0s"
    else if abs < 0.000001 then t"$sign${trim(abs*1000000000.0)}ns"
    else if abs < 0.001 then t"$sign${trim(abs*1000000.0)}µs"
    else if abs < 1.0 then t"$sign${trim(abs*1000.0)}ms"
    else if abs < 60.0 then t"$sign${trim(abs)}s"
    else if abs < 3600.0 then
      val minutes = (abs/60.0).floor
      pair(minutes, t"m", abs - minutes*60.0, t"s")
    else if abs < 86400.0 then
      val hours = (abs/3600.0).floor
      pair(hours, t"h", (abs - hours*3600.0)/60.0, t"m")
    else
      val days = (abs/86400.0).floor
      pair(days, t"d", (abs - days*86400.0)/3600.0, t"h")

  // A time of day, from seconds since the epoch, as `HH:MM:SS` in UTC.
  def clock(seconds: Double): Text =
    val day = seconds - (seconds/86400.0).floor*86400.0
    val hours = (day/3600.0).floor
    val minutes = ((day - hours*3600.0)/60.0).floor
    val rest = (day - hours*3600.0 - minutes*60.0).floor

    def pad(value: Double): Text =
      val text = format(value, 0)
      if text.length < 2 then t"0$text" else text

    t"${pad(hours)}:${pad(minutes)}:${pad(rest)}"

// A fitted numeric axis: the range it shows, whether positions are linear or logarithmic in
// value, and how it is graduated and labelled. A scale is plain data, so a chart can tell whether
// a new point still fits the axis it was drawn with.
case class Scale(lower: Double, upper: Double, transform: Scale.Transform, notation: Scale.Notation)
extends Ruler:

  import Scale.*

  def spacing: Spacing = notation.spacing
  def labelling: Labelling = notation.labelling

  // Where a value lies along the axis, as a fraction of its length. On a logarithmic scale a
  // value at or below zero has no position, and is clipped to the axis's start.
  def unit(value: Double): Double = transform match
    case Transform.Linear =>
      if upper == lower then 0.5 else (value - lower)/(upper - lower)

    case Transform.Logarithmic =>
      if value <= 0.0 || lower <= 0.0 || upper <= lower then 0.0
      else (log10(value).double - log10(lower).double)/(log10(upper).double - log10(lower).double)

  def accommodates(value: Double): Boolean = value >= lower && value <= upper

  def label(value: Double, step: Double): Text = labelling match
    case Labelling.Number(suffix) => t"${format(value, decimals(step))}$suffix"
    case Labelling.Interval       => interval(value)
    case Labelling.Clock          => clock(value)

  def gradations(budget: Int): Sequence[Gradation] = transform match
    case Transform.Linear                     => linear(budget.max(1))
    case Transform.Logarithmic if lower > 0.0 => logarithmic(budget.max(1))
    case Transform.Logarithmic                => linear(budget.max(1))

  private def linear(budget: Int): Sequence[Gradation] =
    val span = upper - lower

    if span <= 0.0 then Sequence(Gradation(0.5, label(lower, 1.0))) else
      val raw = span/budget

      val step = spacing match
        case Spacing.Decimal     => Scale.step(raw)
        case Spacing.Sexagesimal => sexagesimalStep(raw)

      val first = (lower/step - tolerance).ceiling
      var index = 0L
      var marks: List[Gradation] = Nil

      while (first + index)*step <= upper + step*tolerance*1000.0 do
        val value = (first + index)*step
        marks = Gradation(unit(value), label(value, step)) :: marks
        index += 1

      marks.reverse.to[Sequence]

  private def logarithmic(budget: Int): Sequence[Gradation] =
    val low = log10(lower).double.floor.toInt
    val high = log10(upper).double.ceiling.toInt
    val decades = high - low + 1
    val stride = (decades + budget - 1)/budget
    var marks: List[Gradation] = Nil
    var decade = low

    while decade <= high do
      val value = 10.0 ** decade.toDouble

      if value >= lower*(1.0 - tolerance) && value <= upper*(1.0 + tolerance)
      then marks = Gradation(unit(value), label(value, value)) :: marks

      if stride == 1 && decades*2 <= budget then
        var multiple = 2

        while multiple <= 9 do
          val minor = multiple*value
          if minor > lower && minor < upper then marks = Gradation(unit(minor), t"", false) :: marks
          multiple += 1

      decade += stride

    marks.reverse.to[Sequence]
