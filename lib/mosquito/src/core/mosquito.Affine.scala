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
  // Not inline, unlike `Matrix`'s constructors: an inline body defined inside the opaque
  // scope expands with the alias visible, so a call site would see its result as the
  // `Matrix[element, 3, 3]` and find none of the `Affine` extensions on it. Construction
  // boxes its six stores once; the operations below do not.
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

  // The accessors and operations are inline, with the element arithmetic resolved at the call
  // site (`times` and `plus`) as `Matrix`'s instances are, so an `Affine[Double]` transforms a
  // point in six `dmul`s and four `dadd`s over `daload`s, with nothing boxed.
  extension [element](affine: Affine[element])
    inline def a: element = affine(0, 0)
    inline def c: element = affine(0, 1)
    inline def e: element = affine(0, 2)
    inline def b: element = affine(1, 0)
    inline def d: element = affine(1, 1)
    inline def f: element = affine(1, 2)

    // Applies the transform to a point. Named `transform`, not `apply`: on a value which is
    // also a `Matrix`, an application to two integer literals would resolve to the inherited
    // element accessor `apply(row, column)` instead, silently.
    inline def transform(x: element, y: element)
      ( using multiplicable: element is Multiplicable by element to element,
              addable:       element is Addable by element to element )
    :   (element, element) =

      import mosquito.internal.{plus, times}

      ( plus[element, element, element]
          ( plus[element, element, element]
              ( times[element, element, element](a, x), times[element, element, element](c, y) ),
            e ),
        plus[element, element, element]
          ( plus[element, element, element]
              ( times[element, element, element](b, x), times[element, element, element](d, y) ),
            f ) )

    // The composition which applies `affine` first and `next` second — the column-vector
    // product `next * affine`, and precisely PDF's row-vector `this * that`. Specialised to
    // the six live entries (the bottom row is fixed): twelve multiplications, not the general
    // twenty-seven.
    inline def andThen(next: Affine[element])
      ( using multiplicable: element is Multiplicable by element to element,
              addable:       element is Addable by element to element,
              zeroic:        element is Zeroic,
              unital:        element is Unital,
              classTag:      ClassTag[element] )
    :   Affine[element] =

      import mosquito.internal.{plus, times}

      // The entries in `Affine.apply`'s order; each product is a `times` and each sum a
      // `plus`, which resolve the element's own instance here.
      Affine
        ( plus[element, element, element]
            ( times[element, element, element](next.a, affine.a),
              times[element, element, element](next.c, affine.b) ),
          plus[element, element, element]
            ( times[element, element, element](next.b, affine.a),
              times[element, element, element](next.d, affine.b) ),
          plus[element, element, element]
            ( times[element, element, element](next.a, affine.c),
              times[element, element, element](next.c, affine.d) ),
          plus[element, element, element]
            ( times[element, element, element](next.b, affine.c),
              times[element, element, element](next.d, affine.d) ),
          plus[element, element, element]
            ( plus[element, element, element]
                ( times[element, element, element](next.a, affine.e),
                  times[element, element, element](next.c, affine.f) ),
              next.e ),
          plus[element, element, element]
            ( plus[element, element, element]
                ( times[element, element, element](next.b, affine.e),
                  times[element, element, element](next.d, affine.f) ),
              next.f ) )

opaque type Affine[element] <: Matrix[element, 3, 3] = Matrix[element, 3, 3]
