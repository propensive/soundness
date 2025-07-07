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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package austronesian

import anticipation.*
import contingency.*
import distillate.*
import prepositional.*
import probably.*
import rudiments.*
import wisteria.*

object internal:
  opaque type Pojo <: Object =
    Array[Object] | String | java.lang.Boolean | java.lang.Byte | java.lang.Character |
      java.lang.Short | java.lang.Integer | java.lang.Long | java.lang.Float | java.lang.Double

  object Pojo extends Pojo2:
    def apply
      ( pojo: Array[Object] | String | java.lang.Boolean | java.lang.Byte | java.lang.Character |
        java.lang.Short | java.lang.Integer | java.lang.Long | java.lang.Float |
        java.lang.Double )
    :   Pojo =

      pojo


    given text: Text is Encodable:
      type Form = Pojo

      inline def encoded(text: Text): Pojo = text.s

    given string: String is Encodable:
      type Form = Pojo

      inline def encoded(value: String): Pojo = value

    given int: Int is Encodable:
      type Form = Pojo

      inline def encoded(value: Int): Pojo = java.lang.Integer.valueOf(value).nn

    given long: Long is Encodable:
      type Form = Pojo

      inline def encoded(value: Long): Pojo = java.lang.Long.valueOf(value).nn

    given float: Float is Encodable:
      type Form = Pojo

      inline def encoded(value: Float): Pojo = java.lang.Float.valueOf(value).nn

    given double: Double is Encodable:
      type Form = Pojo

      inline def encoded(value: Double): Pojo = java.lang.Double.valueOf(value).nn

    given char: Char is Encodable:
      type Form = Pojo

      inline def encoded(value: Char): Pojo = java.lang.Character.valueOf(value).nn

    given boolean: Boolean is Encodable:
      type Form = Pojo

      inline def encoded(value: Boolean): Pojo = java.lang.Boolean.valueOf(value).nn

    given byte: Byte is Encodable:
      type Form = Pojo

      inline def encoded(value: Byte): Pojo = java.lang.Byte.valueOf(value).nn

    given list: [collection <: Iterable, element: Encodable in Pojo]
    =>  collection[element] is Encodable in Pojo =

      iterable => Array.from[Object](iterable.map(_.encode.asInstanceOf[Object]))


    given text2: Text is Decodable:
      type Form = Pojo

      inline def decoded(value: Pojo): Text = value.asInstanceOf[Text]

    given string2: String is Decodable:
      type Form = Pojo

      inline def decoded(value: Pojo): String = value.asInstanceOf[String]

    given int2: Int is Decodable:
      type Form = Pojo

      inline def decoded(value: Pojo): Int = value.asInstanceOf[Int]

    given long2: Long is Decodable:
      type Form = Pojo

      inline def decoded(value: Pojo): Long = value.asInstanceOf[Long]

    given float2: Float is Decodable:
      type Form = Pojo

      inline def decoded(value: Pojo): Float = value.asInstanceOf[Float]

    given double2: Double is Decodable:
      type Form = Pojo

      inline def decoded(value: Pojo): Double = value.asInstanceOf[Double]

    given char2: Char is Decodable:
      type Form = Pojo

      inline def decoded(value: Pojo): Char = value.asInstanceOf[Char]

    given boolean2: Boolean is Decodable:
      type Form = Pojo

      inline def decoded(value: Pojo): Boolean = value.asInstanceOf[Boolean]


    // The element decoder is taken by-name (not as a context bound) so that a type recursive
    // through a collection (`case class Tree(…, children: List[Tree])`) derives: wisteria's
    // `deriveGraph` recognises this single-by-name-element codec shape and ties the recursion
    // through a lazy sibling instead of structurally sum-deriving `List` (issue #1431).
    given collection: [collection <: Iterable, element]
    =>  ( factory: scala.collection.Factory[element, collection[element]],
          tactic:  Tactic[PojoError] )
    =>  ( decodable: => element is Decodable in Pojo )
    =>  collection[element] is Decodable in Pojo =

      // The by-name element codec and resolution-scoped tactic share this instance's
      // given-resolution lifetime; laundered pure (the codec-thunk seal pattern).
      caps.unsafe.unsafeAssumePure:
        case array: Array[Pojo @unchecked] =>
          factory.newBuilder.pipe: builder =>
            array.iterator.each(builder += decodable.decoded(_))
            builder.result()

        case other =>
          abort(PojoError())


    extension (pojo: Pojo)
      inline def as[entity: Decodable in Pojo]: entity = entity.decoded(pojo)

  trait Pojo2:
    given checkable: Pojo is Checkable:
      type Contrast = Pojo

      def check(left: Pojo, right: Pojo): Boolean = left match
        case left: Array[?] =>
          right match
            case right: Array[?] =>
              left.length == right.length && left.indices.forall: index =>
                left(index) match
                  case left: Pojo @unchecked => right(index) match
                    case right: Pojo @unchecked => checkable.check(left, right)
                    case _                      => false

                  case _                     => false

            case _               => false

        case left =>
          left == right

    inline given encodable: [value: Reflection] => value is Encodable in Pojo =
      protointernal.EncodableDerivation.derived

    inline given decodable: [value: Reflection] => value is Decodable in Pojo =
      protointernal.DecodableDerivation.derived
