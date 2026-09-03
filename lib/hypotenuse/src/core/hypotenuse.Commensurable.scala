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
package hypotenuse

import scala.annotation.*

import denominative.*
import prepositional.*
import rudiments.*
import symbolism.*

object Commensurable:
  inline given numeric: [operand <: Long | Int | Double | Char | Byte | Short | Float]
  =>  Boolean is Commensurable:

    type Contrast = operand


    inline def compare
      ( inline left:        Boolean,
        inline right:       operand,
        inline strict:      Boolean,
        inline greaterThan: Boolean )
    :   Boolean =

      ${hypotenuse.protointernal.commensurable('left, 'right, 'strict, 'greaterThan)}


  given bytes: Bytes is Orderable:
    inline def compare
      ( inline left:    Bytes,
        inline right:   Bytes,
        inline strict:  Boolean,
        inline greater: Boolean )
    :   Boolean =

      !strict && left.long == right.long || (left.long < right.long) ^ greater

  inline given ordinal: Ordinal is Orderable:
    inline def compare
      ( inline left:    Ordinal,
        inline right:   Ordinal,
        inline strict:  Boolean,
        inline greater: Boolean )
    :   Boolean =

      inline if greater then inline if strict then left.gt(right) else left.ge(right)
      else inline if strict then left.lt(right) else left.le(right)

  // Every runtime comparison confers the compile-time comparison operators. Types whose order
  // is still expressed as a stdlib `Ordering` reach this through `Comparable`'s own low-priority
  // bridge, so this single instance covers both.
  given orderable: [value: Comparable] => value is Orderable:
    inline def compare
      ( inline left:    value,
        inline right:   value,
        inline strict:  Boolean,
        inline greater: Boolean )
    :   Boolean =

      val comparison = value.compare(left, right)

      inline if greater
      then inline if strict then comparison.more else !comparison.less
      else inline if strict then comparison.less else !comparison.more


trait Commensurable extends Typeclass.Pure, Contrastive:
  inline def compare
    ( inline left:        Self,
      inline right:       Contrast,
      inline strict:      Boolean,
      inline greaterThan: Boolean )
  :   Boolean
