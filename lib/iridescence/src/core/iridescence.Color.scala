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
package iridescence

import anticipation.*
import prepositional.*
import symbolism.*

object Color:
  given chromatic: [form <: Color: Perceptual in Srgb as perceptual] => Color in form is Chromatic =
    color =>
      val srgb = color.to[Srgb]
      Chroma((srgb.red*255).toInt, (srgb.green*255).toInt, (srgb.blue*255).toInt)

  // Lifting a color to a `Daub` lives here, rather than in `Daub`'s companion, because `Daub`
  // appears only in the result: the implicit scope searched for `5*Red` is built from `Int` and
  // the color's type, and `Color`'s companion is the one place common to every space.
  //
  // `Multiplicable`'s `Operand` is an invariant type member, so it must be the operand's own type
  // rather than `Color in topic`, which would never match a concrete `Srgb`. The space comes from
  // the bound instead, which normalizes `5*WebColors.Red` (a `Color in Srgb`) and `5*Srgb(1, 0, 0)`
  // to the same `Daub[Srgb]`, so the two can be added to one another.
  given multiplicable: [topic <: Color, color <: Color in topic]
  =>  Double is Multiplicable by color to Daub[topic] =
    Multiplicable: (parts, color) => Daub(parts, color.to[topic])

  given wholeMultiplicable: [topic <: Color, color <: Color in topic]
  =>  Int is Multiplicable by color to Daub[topic] =
    Multiplicable: (parts, color) => Daub(parts, color.to[topic])


// A `Color` is a pure value (it holds no capabilities), so it extends `Pure`; this keeps `this.type`
// out of capture sets, which the `type Form >: this.type` self-bound requires its concrete
// subtypes (`type Form = Srgb`, etc.) to satisfy.
trait Color extends scala.caps.Pure:
  type Form >: this.type <: Color

  def to[color <: Color](using perceptual: Form is Perceptual in color): color =
    perceptual.convert(this)
