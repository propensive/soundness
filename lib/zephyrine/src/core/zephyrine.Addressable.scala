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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import java.io as ji
import java.lang as jl

import scala.collection.mutable as scm

import anticipation.*
import denominative.*
import fulminate.*
import prepositional.*
import rudiments.*
import vacuous.*

object Addressable:
  inline given bytes: Data is Addressable:
    type Operand = Byte
    type Target = ji.ByteArrayOutputStream

    val empty: Data = IArray.from(Nil)
    inline def blank(size: Int): ji.ByteArrayOutputStream = ji.ByteArrayOutputStream(size)

    inline def build(target: ji.ByteArrayOutputStream): Data =
      target.toByteArray.nn.immutable(using Unsafe)

    inline def length(bytes: Data): Int = bytes.length
    inline def address(bytes: Data, index: Ordinal): Byte = bytes(index.n0)
    inline def grab(bytes: Data, start: Ordinal, end: Ordinal): Data = bytes.slice(start.n0, end.n0)

    inline def clone(source: Data, start: Ordinal, end: Ordinal)(target: ji.ByteArrayOutputStream)
    : Unit =

        target.write(source.mutable(using Unsafe), start.n0, end.n0 - start.n0 + 1)


  inline given text: Text is Addressable:
    type Operand = Char
    type Target = jl.StringBuilder

    val empty: Text = ""

    inline def build(target: jl.StringBuilder): Text = target.toString.tt
    inline def blank(size: Int): jl.StringBuilder = jl.StringBuilder(size)
    inline def length(text: Text): Int = text.s.length
    inline def address(text: Text, index: Ordinal): Operand = text.s.charAt(index.n0)

    inline def grab(text: Text, start: Ordinal, end: Ordinal): Text =
      text.s.substring(start.n0, end.n1).nn.tt


    inline def clone(source: Text, start: Ordinal, end: Ordinal)(target: java.lang.StringBuilder)
    : Unit =

        target.append(source.s, start.n0, end.n1)


trait Addressable extends Typeclass, Operable, Targetable:
  def empty: Self
  inline def blank(size: Int): Target
  inline def build(target: Target): Self
  inline def length(block: Self): Int
  inline def address(block: Self, index: Ordinal): Operand
  inline def clone(source: Self, start: Ordinal, end: Ordinal)(target: Target): Unit
  inline def grab(text: Self, start: Ordinal, end: Ordinal): Self
