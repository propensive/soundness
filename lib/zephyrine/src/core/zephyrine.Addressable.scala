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
package zephyrine

import proscenium.compat.*

import scala.caps

import java.io as ji
import java.lang as jl

import scala.collection.mutable as scm
import scala.reflect.ClassTag

import anticipation.*
import denominative.*
import prepositional.*
import rudiments.*
import vacuous.*

object Addressable:
  inline given bytes: Data is Addressable:
    type Operand = Byte
    type Target = ji.ByteArrayOutputStream
    type Storage = scala.Array[Byte]

    val empty: Data = Array.from(Nil.stdlib)

    inline def substrate: Substrate = Substrate.Bytes
    inline def blank(size: Int): ji.ByteArrayOutputStream = ji.ByteArrayOutputStream(size)

    inline def build(target: ji.ByteArrayOutputStream): Data =
      Array.unsafeFrozen(target.toByteArray.nn)

    inline def length(bytes: Data): Int = bytes.length
    inline def address(bytes: Data, index: Ordinal): Byte = bytes(index.n0)
    inline def grab(bytes: Data, start: Ordinal, end: Ordinal): Data = bytes.slice(start.n0, end.n0)


    inline def clone(source: Data, start: Ordinal, end: Ordinal)(target: ji.ByteArrayOutputStream)
    :   Unit =

      target.write(Array.unsafeJvm(source), start.n0, end.n0 - start.n0 + 1)

    inline def allocate(size: Int): scala.Array[Byte] = new scala.Array[Byte](size)
    inline def storageSize(storage: scala.Array[Byte]): Int = storage.length
    inline def storageAddress(storage: scala.Array[Byte], index: Int): Byte = storage(index)

    inline def storageUpdate(storage: scala.Array[Byte]^, index: Int, operand: Byte): Unit =
      storage(index) = operand

    inline def append(target: ji.ByteArrayOutputStream, operand: Byte): Unit =
      target.write(operand.toInt)

    inline def copyChunk
      ( source:  Data,
       srcOff:  Int,
       dest:    scala.Array[Byte]^,
       destOff: Int,
       len:     Int )
    :   Unit =

      System.arraycopy(Array.unsafeJvm(source), srcOff, dest, destOff, len)

    inline def transfer
      ( src:     scala.Array[Byte],
       srcOff:  Int,
       dest:    scala.Array[Byte]^,
       destOff: Int,
       len:     Int )
    :   Unit = System.arraycopy(src, srcOff, dest, destOff, len)

    inline def materialize(storage: scala.Array[Byte], off: Int, len: Int): Data =
      Array.unsafeFrozen(java.util.Arrays.copyOfRange(storage, off, off + len).nn)

    override inline def backing(value: Data): Optional[scala.Array[Byte]] =
      value.asInstanceOf[scala.Array[Byte]]

    inline def cloneStorage
      (storage: scala.Array[Byte], off: Int, len: Int)(target: ji.ByteArrayOutputStream)
    :   Unit =

      target.write(storage, off, len)


  // Heap-object media: a chunk of records (parsed rows, JSON events) is an
  // frozen array of them, stored in an erased `Array[AnyRef]` — so credit is
  // counted in records, and `Buffering` sizes these buffers by reference
  // count (`Substrate.Boxes`), since their memory usage isn't
  // deterministically bounded. Erasure means the medium's element type must
  // be a reference type, and the frozen arrays this instance MATERIALIZES must be
  // genuine `element[]`s (an `Array[element]^{}` value checkcasts to one at any
  // concretely-typed use site) — hence the `ClassTag`; only the working
  // storage is an erased `Array[AnyRef]`.
  given boxed: [element <: AnyRef: ClassTag] => (Array[element]^{}) is Addressable:
    type Operand = element
    type Target = scm.ArrayBuffer[element]
    type Storage = scala.Array[AnyRef]

    val empty: Array[element]^{} = Array.empty[element]

    def substrate: Substrate = Substrate.Boxes
    def blank(size: Int): scm.ArrayBuffer[element] = scm.ArrayBuffer[element]()

    def build(target: scm.ArrayBuffer[element]): Array[element]^{} =
      Array.unsafeFrozen(target.toArray[element])

    def length(block: Array[element]^{}): Int = block.length
    def address(block: Array[element]^{}, index: Ordinal): element = block(index.n0)

    def grab(block: Array[element]^{}, start: Ordinal, end: Ordinal): Array[element]^{} =
      block.slice(start.n0, end.n0)

    def clone(source: Array[element]^{}, start: Ordinal, end: Ordinal)
      ( target: scm.ArrayBuffer[element] )
    :   Unit =

      var index = start.n0

      while index <= end.n0 do
        target += source(index)
        index += 1

    def allocate(size: Int): scala.Array[AnyRef] = new scala.Array[AnyRef](size)
    def storageSize(storage: scala.Array[AnyRef]): Int = storage.length

    def storageAddress(storage: scala.Array[AnyRef], index: Int): element =
      storage(index).asInstanceOf[element]

    def storageUpdate(storage: scala.Array[AnyRef]^, index: Int, operand: element): Unit =
      storage(index) = operand

    def append(target: scm.ArrayBuffer[element], operand: element): Unit = target += operand

    def copyChunk
      ( source:  Array[element]^{},
       srcOff:  Int,
       dest:    scala.Array[AnyRef]^,
       destOff: Int,
       len:     Int )
    :   Unit =

      System.arraycopy(source.asInstanceOf[scala.Array[AnyRef]], srcOff, dest, destOff, len)

    def transfer
      ( src:     scala.Array[AnyRef],
       srcOff:  Int,
       dest:    scala.Array[AnyRef]^,
       destOff: Int,
       len:     Int )
    :   Unit = System.arraycopy(src, srcOff, dest, destOff, len)

    def materialize(storage: scala.Array[AnyRef], off: Int, len: Int): Array[element]^{} =
      val array = Array[element](len)
      System.arraycopy(storage, off, array.raw, 0, len)
      Array.freeze(array)

    def cloneStorage
      (storage: scala.Array[AnyRef], off: Int, len: Int)(target: scm.ArrayBuffer[element])
    :   Unit =

      var index = off

      while index < off + len do
        target += storage(index).asInstanceOf[element]
        index += 1


  // Chunks of `Text` records (lines, tokens): `Text` is opaquely
  // `String & caps.Pure`, not `<: AnyRef`, so the generic `boxed` instance
  // above does not admit it, and it gets the same treatment spelled out. The
  // element bound on `boxed` is load-bearing (it keeps `Array[Byte]^{}` — the
  // transparent `Data` alias — from resolving ambiguously), so it cannot
  // simply be relaxed.
  given texts: (Array[Text]^{}) is Addressable:
    type Operand = Text
    type Target = scm.ArrayBuffer[Text]
    type Storage = scala.Array[AnyRef]

    // `Array[Text]^{}` erases to `String[]` (unlike `boxed`'s `Object[]`, whose
    // element type is generic), so materialized chunks must really be
    // `String[]`s; the storage stays `Array[AnyRef]`, which `String[]` enters
    // covariantly.
    val empty: Array[Text]^{} = Array.freeze(Array[String](0)).asInstanceOf[Array[Text]^{}]

    def substrate: Substrate = Substrate.Boxes
    def blank(size: Int): scm.ArrayBuffer[Text] = scm.ArrayBuffer[Text]()

    def build(target: scm.ArrayBuffer[Text]): Array[Text]^{} =
      val array = Array[String](target.length)
      var index = 0

      while index < target.length do
        array(index) = target(index).s
        index += 1

      Array.freeze(array).asInstanceOf[Array[Text]^{}]

    def length(block: Array[Text]^{}): Int = block.length
    def address(block: Array[Text]^{}, index: Ordinal): Text = block(index.n0)

    def grab(block: Array[Text]^{}, start: Ordinal, end: Ordinal): Array[Text]^{} =
      block.slice(start.n0, end.n0)

    def clone(source: Array[Text]^{}, start: Ordinal, end: Ordinal)
      ( target: scm.ArrayBuffer[Text] )
    :   Unit =

      var index = start.n0

      while index <= end.n0 do
        target += source(index)
        index += 1

    def allocate(size: Int): scala.Array[AnyRef] = new scala.Array[AnyRef](size)
    def storageSize(storage: scala.Array[AnyRef]): Int = storage.length

    def storageAddress(storage: scala.Array[AnyRef], index: Int): Text =
      storage(index).asInstanceOf[Text]

    def storageUpdate(storage: scala.Array[AnyRef]^, index: Int, operand: Text): Unit =
      storage(index) = operand.asInstanceOf[AnyRef]

    def append(target: scm.ArrayBuffer[Text], operand: Text): Unit = target += operand

    def copyChunk
      ( source:  Array[Text]^{},
       srcOff:  Int,
       dest:    scala.Array[AnyRef]^,
       destOff: Int,
       len:     Int )
    :   Unit =

      System.arraycopy(source.asInstanceOf[scala.Array[AnyRef]], srcOff, dest, destOff, len)

    def transfer
      ( src:     scala.Array[AnyRef],
       srcOff:  Int,
       dest:    scala.Array[AnyRef]^,
       destOff: Int,
       len:     Int )
    :   Unit = System.arraycopy(src, srcOff, dest, destOff, len)

    def materialize(storage: scala.Array[AnyRef], off: Int, len: Int): Array[Text]^{} =
      val array = Array[String](len)
      System.arraycopy(storage, off, array.raw, 0, len)
      Array.freeze(array).asInstanceOf[Array[Text]^{}]

    def cloneStorage
      (storage: scala.Array[AnyRef], off: Int, len: Int)(target: scm.ArrayBuffer[Text])
    :   Unit =

      var index = off

      while index < off + len do
        target += storage(index).asInstanceOf[Text]
        index += 1


  // Chunks of `Data` records (frames, messages): like `texts`, the element
  // type is concrete and not `<: AnyRef` (`Data` is transparently
  // `Array[Byte]^{}`), so the generic `boxed` instance does not admit it, and
  // materialized chunks must genuinely be `byte[][]`s.
  given frames: (Array[Data]^{}) is Addressable:
    type Operand = Data
    type Target = scm.ArrayBuffer[Data]
    type Storage = scala.Array[AnyRef]

    val empty: Array[Data]^{} = Array.freeze(Array[Data](0))

    def substrate: Substrate = Substrate.Boxes
    def blank(size: Int): scm.ArrayBuffer[Data] = scm.ArrayBuffer[Data]()

    def build(target: scm.ArrayBuffer[Data]): Array[Data]^{} =
      val array = Array[Data](target.length)
      var index = 0

      while index < target.length do
        array(index) = target(index)
        index += 1

      Array.freeze(array)

    def length(block: Array[Data]^{}): Int = block.length
    def address(block: Array[Data]^{}, index: Ordinal): Data = block(index.n0)

    def grab(block: Array[Data]^{}, start: Ordinal, end: Ordinal): Array[Data]^{} =
      block.slice(start.n0, end.n0)

    def clone(source: Array[Data]^{}, start: Ordinal, end: Ordinal)
      ( target: scm.ArrayBuffer[Data] )
    :   Unit =

      var index = start.n0

      while index <= end.n0 do
        target += source(index)
        index += 1

    def allocate(size: Int): scala.Array[AnyRef] = new scala.Array[AnyRef](size)
    def storageSize(storage: scala.Array[AnyRef]): Int = storage.length

    def storageAddress(storage: scala.Array[AnyRef], index: Int): Data =
      storage(index).asInstanceOf[Data]

    def storageUpdate(storage: scala.Array[AnyRef]^, index: Int, operand: Data): Unit =
      storage(index) = operand.asInstanceOf[AnyRef]

    def append(target: scm.ArrayBuffer[Data], operand: Data): Unit = target += operand

    def copyChunk
      ( source:  Array[Data]^{},
       srcOff:  Int,
       dest:    scala.Array[AnyRef]^,
       destOff: Int,
       len:     Int )
    :   Unit =

      System.arraycopy(source.asInstanceOf[scala.Array[AnyRef]], srcOff, dest, destOff, len)

    def transfer
      ( src:     scala.Array[AnyRef],
       srcOff:  Int,
       dest:    scala.Array[AnyRef]^,
       destOff: Int,
       len:     Int )
    :   Unit = System.arraycopy(src, srcOff, dest, destOff, len)

    def materialize(storage: scala.Array[AnyRef], off: Int, len: Int): Array[Data]^{} =
      val array = Array[Data](len)
      System.arraycopy(storage, off, array.raw, 0, len)
      Array.freeze(array)

    def cloneStorage
      (storage: scala.Array[AnyRef], off: Int, len: Int)(target: scm.ArrayBuffer[Data])
    :   Unit =

      var index = off

      while index < off + len do
        target += storage(index).asInstanceOf[Data]
        index += 1


  inline given text: Text is Addressable:
    type Operand = Char
    type Target = jl.StringBuilder
    type Storage = scala.Array[Char]

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

    inline def allocate(size: Int): scala.Array[Char] = new scala.Array[Char](size)
    inline def storageSize(storage: scala.Array[Char]): Int = storage.length
    inline def storageAddress(storage: scala.Array[Char], index: Int): Char = storage(index)

    inline def storageUpdate(storage: scala.Array[Char]^, index: Int, operand: Char): Unit =
      storage(index) = operand

    inline def append(target: jl.StringBuilder, operand: Char): Unit = target.append(operand)

    inline def copyChunk
      ( source:  Text,
       srcOff:  Int,
       dest:    scala.Array[Char]^,
       destOff: Int,
       len:     Int )
    :   Unit = source.s.getChars(srcOff, srcOff + len, dest, destOff)

    inline def transfer
      ( src:     scala.Array[Char],
       srcOff:  Int,
       dest:    scala.Array[Char]^,
       destOff: Int,
       len:     Int )
    :   Unit = System.arraycopy(src, srcOff, dest, destOff, len)

    inline def materialize(storage: scala.Array[Char], off: Int, len: Int): Text =
      String(storage, off, len).tt

    inline def cloneStorage
      (storage: scala.Array[Char], off: Int, len: Int)(target: jl.StringBuilder)
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
  // a region — or handed across an asynchronous boundary — without copying.
  // `Data` returns its backing array; media without a directly-exposable
  // backing (`Text`, whose `String` is not an `Array[Char]`) return `Unset`,
  // and callers copy. Exposed backing must never be mutated.
  def backing(value: Self): Optional[Storage]^{caps.any.rd} = Unset
