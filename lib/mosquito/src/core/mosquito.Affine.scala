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
package mosquito

import gossamer.*
import prepositional.*
import rudiments.*
import symbolism.*

object Affine:
  // The six live entries of a 2D affine transformation, in the order shared by SVG's
  // `matrix(a, b, c, d, e, f)` and PDF's `a b c d e f cm` (ISO 32000-2 §8.3.4). Here they form
  // the homogeneous matrix
  //   | a c e |
  //   | b d f |
  //   | 0 0 1 |
  // acting on column vectors. PDF documents the transposed layout acting on row vectors, but
  // the six numbers appear in the same spec-defined order in both, and the point-application
  // formula in `transform` is common to the two readings. Composition order is the one place
  // the conventions diverge, so it is a named operation (`andThen`) rather than a symbolic one.
  def apply[element: ClassTag]
    ( a: element, b: element, c: element, d: element, e: element, f: element )
    ( using zeroic: element is Zeroic, unital: element is Unital )
  :   Affine[element] =

    val array = Array.build[element](9): array =>
      array(0) = a
      array(1) = c
      array(2) = e
      array(3) = b
      array(4) = d
      array(5) = f
      array(6) = zeroic.zero
      array(7) = zeroic.zero
      array(8) = unital.one

    new Matrix[element, 3, 3](3, 3, array)

  def identity[element: ClassTag](using element is Zeroic, element is Unital): Affine[element] =
    Matrix.identity[element, 3]

  extension [element](affine: Affine[element])
    def a: element = affine(0, 0)
    def c: element = affine(0, 1)
    def e: element = affine(0, 2)
    def b: element = affine(1, 0)
    def d: element = affine(1, 1)
    def f: element = affine(1, 2)

    // Applies the transform to a point. Named `transform`, not `apply`: on a value which is
    // also a `Matrix`, an application to two integer literals would resolve to the inherited
    // element accessor `apply(row, column)` instead, silently.
    def transform(x: element, y: element)
      ( using multiplicable: element is Multiplicable by element to element,
              addable:       element is Addable by element to element )
    :   (element, element) =

      (a*x + c*y + e, b*x + d*y + f)

    // The composition which applies `affine` first and `next` second — the column-vector
    // product `next * affine`, and precisely PDF's row-vector `this * that`. Specialised to
    // the six live entries (the bottom row is fixed): twelve multiplications, not the general
    // twenty-seven.
    def andThen(next: Affine[element])
      ( using multiplicable: element is Multiplicable by element to element,
              addable:       element is Addable by element to element,
              zeroic:        element is Zeroic,
              unital:        element is Unital,
              classTag:      ClassTag[element] )
    :   Affine[element] =

      Affine
        ( next.a*affine.a + next.c*affine.b,
          next.b*affine.a + next.d*affine.b,
          next.a*affine.c + next.c*affine.d,
          next.b*affine.c + next.d*affine.d,
          next.a*affine.e + next.c*affine.f + next.e,
          next.b*affine.e + next.d*affine.f + next.f )

opaque type Affine[element] <: Matrix[element, 3, 3] = Matrix[element, 3, 3]
