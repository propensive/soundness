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
┃    Soundness, version 0.46.0.                                                                    ┃
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

object Commensurable:
  inline given numeric: [operand <: Long | Int | Double | Char | Byte | Short | Float]
        =>  Boolean is Commensurable:
    type Contrast = operand


    inline def compare
                (inline left: Boolean,
                 inline right: operand,
                 inline strict: Boolean,
                 inline greaterThan: Boolean)
    : Boolean =

        ${Hypotenuse2.commensurable('left, 'right, 'strict, 'greaterThan)}


  given memory: Memory is Orderable:
    inline def compare
                (inline left:    Memory,
                 inline right:   Memory,
                 inline strict:  Boolean,
                 inline greater: Boolean)
    : Boolean =

        !strict && left.long == right.long || (left.long < right.long) ^ greater


  inline given ordinal: Ordinal is Orderable:
    inline def compare
                (inline left:    Ordinal,
                 inline right:   Ordinal,
                 inline strict:  Boolean,
                 inline greater: Boolean)
    : Boolean =

        inline if greater then inline if strict then left.gt(right) else left.ge(right)
        else inline if strict then left.lt(right) else left.le(right)

  given orderable: [value: Ordering] => value is Orderable:
    inline def compare
                (inline left:    value,
                  inline right:   value,
                  inline strict:  Boolean,
                  inline greater: Boolean)
    : Boolean =

        val n = value.compare(left, right)
        inline if greater
        then inline if strict then n > 0 else n >= 0
        else inline if strict then n < 0 else n <= 0


trait Commensurable extends Typeclass, Contrastive:
  inline def compare
              (inline left:        Self,
               inline right:       Contrast,
               inline strict:      Boolean,
               inline greaterThan: Boolean)
  : Boolean
