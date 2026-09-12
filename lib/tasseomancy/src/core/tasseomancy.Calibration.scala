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

import hypotenuse.*
import prepositional.*

object Calibration:
  // The no-import default for an axis of any type: a linear scale, padded outward to whole
  // gradation steps, and anchored at zero when the chart kind asks for it. Any
  // `import calibrations.…` outranks this, because a lexically-scoped given beats a companion
  // one.
  given linear: [value] => value is Calibration = Calibration(Policy.Linear)

  // The choices a calibration makes: linear or logarithmic positions (falling back to linear
  // where a logarithm is undefined), adaptive between the two by the range's ratio, or linear
  // over exactly the data's own extent.
  enum Policy:
    case Linear, Logarithmic, Adaptive, Tight

  def apply[value](policy: Policy): value is Calibration = new Calibration:
    type Self = value

    def scale(lower: Double, upper: Double, anchored: Boolean, notation: Scale.Notation): Scale =
      policy match
        case Policy.Linear      => Calibration.linear(lower, upper, anchored, notation, false)
        case Policy.Tight       => Calibration.linear(lower, upper, anchored, notation, true)
        case Policy.Logarithmic => Calibration.logarithmic(lower, upper, anchored, notation)

        case Policy.Adaptive =>
          if lower > 0.0 && upper/lower >= 1000.0
          then Calibration.logarithmic(lower, upper, anchored, notation)
          else Calibration.linear(lower, upper, anchored, notation, false)

  private[tasseomancy] def linear
    ( lower0:   Double,
      upper0:   Double,
      anchored: Boolean,
      notation: Scale.Notation,
      tight:    Boolean )
  :   Scale =

    val lower1 = if anchored && lower0 > 0.0 then 0.0 else lower0
    val upper1 = if anchored && upper0 < 0.0 then 0.0 else upper0

    if tight then Scale(lower1, upper1, Scale.Transform.Linear, notation) else
      val span0 = upper1 - lower1
      val span = if span0 > 0.0 then span0 else if upper1 != 0.0 then upper1.abs else 1.0

      val step = notation.spacing match
        case Scale.Spacing.Decimal     => Scale.step(span/5.0)
        case Scale.Spacing.Sexagesimal => Scale.sexagesimalStep(span/5.0)

      val lower = (lower1/step + Scale.tolerance).floor*step
      val upper = (upper1/step - Scale.tolerance).ceiling*step
      val upper2 = if upper > lower then upper else lower + step
      Scale(lower, upper2, Scale.Transform.Linear, notation)

  private[tasseomancy] def logarithmic
    ( lower0: Double, upper0: Double, anchored: Boolean, notation: Scale.Notation )
  :   Scale =

    if lower0 <= 0.0 then linear(lower0, upper0, anchored, notation, false) else
      val lower = 10.0 ** log10(lower0).double.floor
      val upper = 10.0 ** log10(upper0).double.ceiling
      val upper2 = if upper > lower then upper else lower*10.0
      Scale(lower, upper2, Scale.Transform.Logarithmic, notation)

// How the extent of the data becomes the range of an axis: whether positions are linear or
// logarithmic, whether the range is padded out to whole gradations, and whether it must include
// zero. Keyed on the type of the values on the axis, so that a scoped given for one type changes
// one axis; the methods are independent of `Self`, so a calibration can also be passed explicitly
// to a chart kind for one of its axes.
trait Calibration extends Typeclass.Pure:
  def scale(lower: Double, upper: Double, anchored: Boolean, notation: Scale.Notation): Scale
