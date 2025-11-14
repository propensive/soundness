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
package zephyrine

import scala.collection.mutable as scm

import anticipation.*
import denominative.*
import fulminate.*
import prepositional.*
import rudiments.*
import vacuous.*

object Addressable:
  inline given Bytes is Addressable:
    type Operand = Byte
    type Target = Array[Byte]
    type Result = Int => Unit

    inline def length(bytes: Bytes): Int = bytes.length
    inline def address(bytes: Bytes, index: Ordinal): Byte = bytes(index.n0)

    inline def fragment(bytes: Bytes, start: Ordinal, end: Ordinal): Bytes =
      bytes.slice(start.n0, end.n1)
      
    inline def grab(source: Bytes, start: Ordinal, end: Ordinal)(target: Array[Byte]): Int => Unit =
      System.arraycopy(source.mutable(using Unsafe), start.n0, target, _, end - start)

  inline given Text is Addressable:
    type Operand = Char
    type Target = StringBuilder
    type Result = Unit

    inline def length(text: Text): Int = text.s.length
    inline def address(text: Text, index: Ordinal): Operand = text.s.charAt(index.n0)

    inline def fragment(text: Text, start: Ordinal, end: Ordinal): Text =
      text.s.substring(start.n0, end.n1).nn.tt
      
    inline def grab(source: Text, start: Ordinal, end: Ordinal)(target: StringBuilder): Unit =
      target.append(source.s, start.n0, end - start)

trait Addressable extends Typeclass, Operable, Targetable, Resultant:
  inline def length(block: Self): Int
  inline def address(block: Self, index: Ordinal): Operand
  inline def fragment(block: Self, start: Ordinal, end: Ordinal): Self
  inline def grab(source: Self, start: Ordinal, end: Ordinal)(target: Target): Result
