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

import anticipation.*
import denominative.*
import prepositional.*
import rudiments.*
import vacuous.*

object Addressable:
  inline given bytes: Data is Addressable:
    type Operand = Byte
    type Target = ji.ByteArrayOutputStream
    type Storage = Array[Byte]

    val empty: Data = IArray.from(Nil)

    inline def blank(size: Int): ji.ByteArrayOutputStream = ji.ByteArrayOutputStream(size)

    inline def build(target: ji.ByteArrayOutputStream): Data =
      target.toByteArray.nn.immutable(using Unsafe)

    inline def length(bytes: Data): Int = bytes.length
    inline def address(bytes: Data, index: Ordinal): Byte = bytes(index.n0)
    inline def grab(bytes: Data, start: Ordinal, end: Ordinal): Data = bytes.slice(start.n0, end.n0)


    inline def clone(source: Data, start: Ordinal, end: Ordinal)(target: ji.ByteArrayOutputStream)
    :   Unit =

      target.write(source.mutable(using Unsafe), start.n0, end.n0 - start.n0 + 1)

    inline def allocate(size: Int): Array[Byte] = new Array[Byte](size)
    inline def storageSize(storage: Array[Byte]): Int = storage.length
    inline def storageAddress(storage: Array[Byte], index: Int): Byte = storage(index)

    inline def copyChunk
                (source:  Data,
                 srcOff:  Int,
                 dest:    Array[Byte],
                 destOff: Int,
                 len:     Int)
    :   Unit =
      System.arraycopy(source.mutable(using Unsafe), srcOff, dest, destOff, len)

    inline def transfer
                (src:     Array[Byte],
                 srcOff:  Int,
                 dest:    Array[Byte],
                 destOff: Int,
                 len:     Int)
    :   Unit = System.arraycopy(src, srcOff, dest, destOff, len)

    inline def materialize(storage: Array[Byte], off: Int, len: Int): Data =
      java.util.Arrays.copyOfRange(storage, off, off + len).nn.immutable(using Unsafe)

    inline def cloneStorage
                (storage: Array[Byte], off: Int, len: Int)(target: ji.ByteArrayOutputStream)
    :   Unit = target.write(storage, off, len)


  inline given text: Text is Addressable:
    type Operand = Char
    type Target = jl.StringBuilder
    type Storage = Array[Char]

    val empty: Text = ""

    inline def build(target: jl.StringBuilder): Text = target.toString.tt
    inline def blank(size: Int): jl.StringBuilder = jl.StringBuilder(size)
    inline def length(text: Text): Int = text.s.length
    inline def address(text: Text, index: Ordinal): Operand = text.s.charAt(index.n0)

    inline def grab(text: Text, start: Ordinal, end: Ordinal): Text =
      text.s.substring(start.n0, end.n1).nn.tt


    inline def clone(source: Text, start: Ordinal, end: Ordinal)(target: java.lang.StringBuilder)
    :   Unit =

      target.append(source.s, start.n0, end.n1)

    inline def allocate(size: Int): Array[Char] = new Array[Char](size)
    inline def storageSize(storage: Array[Char]): Int = storage.length
    inline def storageAddress(storage: Array[Char], index: Int): Char = storage(index)

    inline def copyChunk
                (source:  Text,
                 srcOff:  Int,
                 dest:    Array[Char],
                 destOff: Int,
                 len:     Int)
    :   Unit = source.s.getChars(srcOff, srcOff + len, dest, destOff)

    inline def transfer
                (src:     Array[Char],
                 srcOff:  Int,
                 dest:    Array[Char],
                 destOff: Int,
                 len:     Int)
    :   Unit = System.arraycopy(src, srcOff, dest, destOff, len)

    inline def materialize(storage: Array[Char], off: Int, len: Int): Text =
      String(storage, off, len).tt

    inline def cloneStorage
                (storage: Array[Char], off: Int, len: Int)(target: jl.StringBuilder)
    :   Unit = target.append(storage, off, len)


trait Addressable extends Typeclass, Operable, Targetable:
  // Mutable backing storage for `Cursor`'s single-buffer model. For `Data`,
  // this is `Array[Byte]`; for `Text`, `Array[Char]`. Hot-path reads in
  // `Cursor.peek` / `Cursor.datum` go through `storageAddress` and lower
  // to a single array access.
  type Storage

  def empty: Self
  // All operations are declared non-inline at the trait level so non-inline
  // call sites (e.g. inside `Cursor.forward`, or in parser plumbing that
  // wraps Cursor calls) can still dispatch through them. Concrete instances
  // are still `inline def`, so any hot-path inline call site (e.g. inside
  // `Cursor.next` / `Cursor.datum` / `Cursor.grab`) where the typeclass is
  // resolved at compile time still lowers to direct primitive operations.
  def blank(size: Int): Target
  def build(target: Target): Self
  def length(block: Self): Int
  def address(block: Self, index: Ordinal): Operand
  def clone(source: Self, start: Ordinal, end: Ordinal)(target: Target): Unit
  def grab(text: Self, start: Ordinal, end: Ordinal): Self

  def allocate(size: Int): Storage
  def storageSize(storage: Storage): Int
  def storageAddress(storage: Storage, index: Int): Operand
  def copyChunk
       (source: Self, srcOff: Int, dest: Storage, destOff: Int, len: Int)
  :   Unit

  def transfer
       (src: Storage, srcOff: Int, dest: Storage, destOff: Int, len: Int)
  :   Unit

  def materialize(storage: Storage, off: Int, len: Int): Self
  def cloneStorage(storage: Storage, off: Int, len: Int)(target: Target): Unit
