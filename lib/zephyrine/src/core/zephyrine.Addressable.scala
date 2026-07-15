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
package zephyrine

import java.io as ji
import java.lang as jl

import scala.collection.mutable as scm

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

    inline def substrate: Substrate = Substrate.Bytes
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

    inline def storageUpdate(storage: Array[Byte]^, index: Int, operand: Byte): Unit =
      storage(index) = operand

    inline def append(target: ji.ByteArrayOutputStream, operand: Byte): Unit =
      target.write(operand.toInt)

    inline def copyChunk
      ( source:  Data,
       srcOff:  Int,
       dest:    Array[Byte]^,
       destOff: Int,
       len:     Int )
    :   Unit =

      System.arraycopy(source.mutable(using Unsafe), srcOff, dest, destOff, len)

    inline def transfer
      ( src:     Array[Byte],
       srcOff:  Int,
       dest:    Array[Byte]^,
       destOff: Int,
       len:     Int )
    :   Unit = System.arraycopy(src, srcOff, dest, destOff, len)

    inline def materialize(storage: Array[Byte], off: Int, len: Int): Data =
      java.util.Arrays.copyOfRange(storage, off, off + len).nn.immutable(using Unsafe)

    override inline def backing(value: Data): Optional[Array[Byte]] =
      value.asInstanceOf[Array[Byte]]

    inline def cloneStorage
      (storage: Array[Byte], off: Int, len: Int)(target: ji.ByteArrayOutputStream)
    :   Unit =

      target.write(storage, off, len)


  // Heap-object media: a chunk of records (parsed rows, JSON events) is an
  // `IArray` of them, stored in an erased `Array[AnyRef]` — so credit is
  // counted in records, and `Buffering` sizes these buffers by reference
  // count (`Substrate.Boxes`), since their memory usage isn't
  // deterministically bounded. Erasure means the medium's element type must
  // be a reference type; the `IArray`s this instance materializes are backed
  // by `Array[AnyRef]`, which is indistinguishable at erased use sites.
  given boxed: [element <: AnyRef] => IArray[element] is Addressable:
    type Operand = element
    type Target = scm.ArrayBuffer[element]
    type Storage = Array[AnyRef]

    val empty: IArray[element] = IArray.empty[AnyRef].asInstanceOf[IArray[element]]

    def substrate: Substrate = Substrate.Boxes
    def blank(size: Int): scm.ArrayBuffer[element] = scm.ArrayBuffer[element]()

    def build(target: scm.ArrayBuffer[element]): IArray[element] =
      target.toArray[AnyRef].asInstanceOf[IArray[element]]

    def length(block: IArray[element]): Int = block.length
    def address(block: IArray[element], index: Ordinal): element = block(index.n0)

    def grab(block: IArray[element], start: Ordinal, end: Ordinal): IArray[element] =
      block.slice(start.n0, end.n0)

    def clone(source: IArray[element], start: Ordinal, end: Ordinal)
      ( target: scm.ArrayBuffer[element] )
    :   Unit =

      var index = start.n0

      while index <= end.n0 do
        target += source(index)
        index += 1

    def allocate(size: Int): Array[AnyRef] = new Array[AnyRef](size)
    def storageSize(storage: Array[AnyRef]): Int = storage.length

    def storageAddress(storage: Array[AnyRef], index: Int): element =
      storage(index).asInstanceOf[element]

    def storageUpdate(storage: Array[AnyRef]^, index: Int, operand: element): Unit =
      storage(index) = operand

    def append(target: scm.ArrayBuffer[element], operand: element): Unit = target += operand

    def copyChunk
      ( source:  IArray[element],
       srcOff:  Int,
       dest:    Array[AnyRef]^,
       destOff: Int,
       len:     Int )
    :   Unit =

      System.arraycopy(source.asInstanceOf[Array[AnyRef]], srcOff, dest, destOff, len)

    def transfer
      ( src:     Array[AnyRef],
       srcOff:  Int,
       dest:    Array[AnyRef]^,
       destOff: Int,
       len:     Int )
    :   Unit = System.arraycopy(src, srcOff, dest, destOff, len)

    def materialize(storage: Array[AnyRef], off: Int, len: Int): IArray[element] =
      val array = new Array[AnyRef](len)
      System.arraycopy(storage, off, array, 0, len)
      array.asInstanceOf[IArray[element]]

    def cloneStorage
      (storage: Array[AnyRef], off: Int, len: Int)(target: scm.ArrayBuffer[element])
    :   Unit =

      var index = off

      while index < off + len do
        target += storage(index).asInstanceOf[element]
        index += 1


  // Chunks of `Text` records (lines, tokens): `Text` is opaquely
  // `String & caps.Pure`, not `<: AnyRef`, so the generic `boxed` instance
  // above does not admit it, and it gets the same treatment spelled out. The
  // element bound on `boxed` is load-bearing (it keeps `IArray[Byte]` — the
  // transparent `Data` alias — from resolving ambiguously), so it cannot
  // simply be relaxed.
  given texts: IArray[Text] is Addressable:
    type Operand = Text
    type Target = scm.ArrayBuffer[Text]
    type Storage = Array[AnyRef]

    // `IArray[Text]` erases to `String[]` (unlike `boxed`'s `Object[]`, whose
    // element type is generic), so materialized chunks must really be
    // `String[]`s; the storage stays `Array[AnyRef]`, which `String[]` enters
    // covariantly.
    val empty: IArray[Text] = new Array[String](0).asInstanceOf[IArray[Text]]

    def substrate: Substrate = Substrate.Boxes
    def blank(size: Int): scm.ArrayBuffer[Text] = scm.ArrayBuffer[Text]()

    def build(target: scm.ArrayBuffer[Text]): IArray[Text] =
      val array = new Array[String](target.length)
      var index = 0

      while index < target.length do
        array(index) = target(index).s
        index += 1

      array.asInstanceOf[IArray[Text]]

    def length(block: IArray[Text]): Int = block.length
    def address(block: IArray[Text], index: Ordinal): Text = block(index.n0)

    def grab(block: IArray[Text], start: Ordinal, end: Ordinal): IArray[Text] =
      block.slice(start.n0, end.n0)

    def clone(source: IArray[Text], start: Ordinal, end: Ordinal)
      ( target: scm.ArrayBuffer[Text] )
    :   Unit =

      var index = start.n0

      while index <= end.n0 do
        target += source(index)
        index += 1

    def allocate(size: Int): Array[AnyRef] = new Array[AnyRef](size)
    def storageSize(storage: Array[AnyRef]): Int = storage.length

    def storageAddress(storage: Array[AnyRef], index: Int): Text =
      storage(index).asInstanceOf[Text]

    def storageUpdate(storage: Array[AnyRef]^, index: Int, operand: Text): Unit =
      storage(index) = operand.asInstanceOf[AnyRef]

    def append(target: scm.ArrayBuffer[Text], operand: Text): Unit = target += operand

    def copyChunk
      ( source:  IArray[Text],
       srcOff:  Int,
       dest:    Array[AnyRef]^,
       destOff: Int,
       len:     Int )
    :   Unit =

      System.arraycopy(source.asInstanceOf[Array[AnyRef]], srcOff, dest, destOff, len)

    def transfer
      ( src:     Array[AnyRef],
       srcOff:  Int,
       dest:    Array[AnyRef]^,
       destOff: Int,
       len:     Int )
    :   Unit = System.arraycopy(src, srcOff, dest, destOff, len)

    def materialize(storage: Array[AnyRef], off: Int, len: Int): IArray[Text] =
      val array = new Array[String](len)
      System.arraycopy(storage, off, array, 0, len)
      array.asInstanceOf[IArray[Text]]

    def cloneStorage
      (storage: Array[AnyRef], off: Int, len: Int)(target: scm.ArrayBuffer[Text])
    :   Unit =

      var index = off

      while index < off + len do
        target += storage(index).asInstanceOf[Text]
        index += 1


  inline given text: Text is Addressable:
    type Operand = Char
    type Target = jl.StringBuilder
    type Storage = Array[Char]

    val empty: Text = ""

    inline def substrate: Substrate = Substrate.Chars
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

    inline def storageUpdate(storage: Array[Char]^, index: Int, operand: Char): Unit =
      storage(index) = operand

    inline def append(target: jl.StringBuilder, operand: Char): Unit = target.append(operand)

    inline def copyChunk
      ( source:  Text,
       srcOff:  Int,
       dest:    Array[Char]^,
       destOff: Int,
       len:     Int )
    :   Unit = source.s.getChars(srcOff, srcOff + len, dest, destOff)

    inline def transfer
      ( src:     Array[Char],
       srcOff:  Int,
       dest:    Array[Char]^,
       destOff: Int,
       len:     Int )
    :   Unit = System.arraycopy(src, srcOff, dest, destOff, len)

    inline def materialize(storage: Array[Char], off: Int, len: Int): Text =
      String(storage, off, len).tt

    inline def cloneStorage
      (storage: Array[Char], off: Int, len: Int)(target: jl.StringBuilder)
    :   Unit =

      target.append(storage, off, len)


trait Addressable extends Typeclass.Pure, Operable, Targetable:
  // Mutable backing storage for `Cursor`'s single-buffer model. For `Data`,
  // this is `Array[Byte]`; for `Text`, `Array[Char]`. Hot-path reads in
  // `Cursor.peek` / `Cursor.datum` go through `storageAddress` and lower
  // to a single array access.
  type Storage

  def empty: Self

  // How this medium's storage is physically represented, for `Buffering` to
  // size stage buffers appropriately.
  def substrate: Substrate
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

  // Honest capture typing for the storage protocol: `allocate` mints a fresh
  // exclusive buffer; writers take it exclusively (`Storage^`); readers take a
  // read-only view (`Storage^{caps.any.rd}`).
  def allocate(size: Int): Storage^
  def storageSize(storage: Storage^{caps.any.rd}): Int
  def storageAddress(storage: Storage^{caps.any.rd}, index: Int): Operand

  // Single-`Operand` writes: one element into the chunk storage (`storageUpdate`) or appended to
  // the builder (`append`). These back `Producer.push` for element-at-a-time producers.
  def storageUpdate(storage: Storage^, index: Int, operand: Operand): Unit
  def append(target: Target, operand: Operand): Unit

  def copyChunk
    ( source: Self, srcOff: Int, dest: Storage^, destOff: Int, len: Int )
  :   Unit

  def transfer
    ( src: Storage^{caps.any.rd}, srcOff: Int, dest: Storage^, destOff: Int, len: Int )
  :   Unit

  def materialize(storage: Storage^{caps.any.rd}, off: Int, len: Int): Self
  def cloneStorage(storage: Storage^{caps.any.rd}, off: Int, len: Int)(target: Target): Unit

  // The value's backing storage, when the medium is immutable and its erased
  // representation *is* its `Storage` type, so a whole chunk can be exposed as
  // a window — or handed across an asynchronous boundary — without copying.
  // `Data` returns its backing array; media without a directly-exposable
  // backing (`Text`, whose `String` is not an `Array[Char]`) return `Unset`,
  // and callers copy. Exposed backing must never be mutated.
  def backing(value: Self): Optional[Storage]^{caps.any.rd} = Unset
