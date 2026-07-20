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
package archimedes

import anticipation.*
import contextual.*
import gossamer.*
import prepositional.*

import Mathml.*

// The MathML namespace URI, written as the `xmlns` attribute on the root
// `<math>` element.
val mathmlNamespace: Text = t"http://www.w3.org/1998/Math/MathML"

// Converts any type with an `Encodable in Math` instance to a `<math>` root or a
// single MathML node — `5.math`, `complex.mathml`, `quantity.math`, etc. Instances
// live in the `Math` companion; see `object Math` in `archimedes.Math.scala`.
extension [ValueType: Encodable in Math as encodable](value: ValueType)
  def math: Math = encodable.encoded(value)

  def mathml: Mathml = Mathml.atom(encodable.encoded(value))

// The `ergo""` interpolator: `ergo"(x↗y)"` parses an ergo shorthand literal into
// a `Math` value, checking it at compile time (see `internal.ergoInterpolator`).
inline given ergoInterpolable: Math is Interpolable:
  transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
    ( inline insertions: Any* )
  :   Math =

    ${archimedes.internal.ergoInterpolator[parts]('insertions)}

extension (inline context: StringContext)
  transparent inline def ergo: Interpolation = interpolation[Math](context)

// Renders a MathML node (or the `<math>` root) as a monospaced, box-drawing block
// of text for terminal display. `.cell` gives the composable `Cell` block (with its
// baseline); `.draw` flattens it to newline-separated `Text` ready for printing.
extension (node: Mathml)
  def cell: Cell = Cell.of(node)
  def draw: Text = Cell.of(node).render

extension (math: Math)
  def cell: Cell = Cell.of(Mrow(math.contents))
  def draw: Text = math.cell.render
