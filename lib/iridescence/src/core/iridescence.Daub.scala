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

import prepositional.*
import symbolism.*

// A `Daub` is an amount of paint: a color together with the number of parts of it going into a
// mix. `5*Red` is five parts red, and adding daubs lays each over the total accumulated so far,
// weighted by its share. Since a `Daub` is itself a `Color` — five parts red is still red — an
// expression like `5*Red + 3*Yellow` needs no unwrapping to be converted, rendered or measured;
// `parts` matters only when another daub is added to it.
object Daub:
  given addable: [topic <: Color]
  =>  ( blendable: topic is Blendable, mixing: topic is Mixing )
  =>  Daub[topic] is Addable by Daub[topic] to Daub[topic] =
    Addable: (left, right) =>
      // No paint of a color is not a faint wash of it: a daub of no parts takes no part in the
      // mix at all. Without this, `0*Red + 1*Blue` under `multiply` would still darken by red,
      // because the backdrop is consulted whatever its weight.
      if left.parts == 0.0 then right else if right.parts == 0.0 then left else
        val parts = left.parts + right.parts
        val share = right.parts/parts

        // The mode is applied at full strength, then mixed back in the new daub's share of the
        // total — a layer laid over the backdrop at that opacity.
        val blend = (backdrop: Double, layer: Double) =>
          backdrop + (mixing.blend(backdrop, layer) - backdrop)*share

        Daub(parts, blendable.zip(left.color, right.color, blend))

  given perceptual: [topic <: Color, form <: Color]
  =>  ( perceptual: topic is Perceptual in form )
  =>  Daub[topic] is Perceptual in form =
    daub => perceptual.convert(daub.color)

case class Daub[topic <: Color](parts: Double, color: topic) extends Color:
  type Form = Daub[topic]
