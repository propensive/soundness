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
package xenophile

import anticipation.*
import distillate.*
import hypotenuse.*
import prepositional.*

// The uniform form of a value marshalled across a WIT function boundary (see the `Wasm` class). A
// single form lets a codec be summoned for any Scala type without knowing its carrier in advance;
// the concrete carrier (a type the scala-wasm compiler accepts in a WIT signature) is recovered
// from the codec's `Carrier` member. `Encodable in Wasm` / `Decodable in Wasm` are summoned per WIT
// parameter/result by the emission backend; a value lacking a codec cannot cross a WIT boundary,
// which is reported at compile time.
object Wasm:
  def apply(value: Any): Wasm = new Wasm(value)

  extension (wasm: Wasm) def as[carrier]: carrier = wasm.value.asInstanceOf[carrier]

  // Builds an `Encodable in Wasm` that also exposes the `Carrier` type the value is passed as.
  private def enc[self, carrier0](lambda: self => carrier0)
  :   (self is Encodable in Wasm) { type Carrier = carrier0 } =

    new Encodable:
      type Self = self
      type Form = Wasm
      type Carrier = carrier0
      def encoded(value: self): Wasm = Wasm(lambda(value))

  private def dec[self, carrier0](lambda: carrier0 => self)
  :   (self is Decodable in Wasm) { type Carrier = carrier0 } =

    new Decodable:
      type Self = self
      type Form = Wasm
      type Carrier = carrier0
      type Locus = Text
      def decoded(value: Wasm): self = lambda(value.as[carrier0])

  given s8Encodable: ((S8 is Encodable in Wasm) { type Carrier = Byte }) = enc(_.byte)
  given s8Decodable: ((S8 is Decodable in Wasm) { type Carrier = Byte }) = dec(_.bits.s8)
  given s16Encodable: ((S16 is Encodable in Wasm) { type Carrier = Short }) = enc(_.short)
  given s16Decodable: ((S16 is Decodable in Wasm) { type Carrier = Short }) = dec(_.bits.s16)
  given s32Encodable: ((S32 is Encodable in Wasm) { type Carrier = Int }) = enc(_.int)
  given s32Decodable: ((S32 is Decodable in Wasm) { type Carrier = Int }) = dec(_.bits.s32)
  given s64Encodable: ((S64 is Encodable in Wasm) { type Carrier = Long }) = enc(_.long)
  given s64Decodable: ((S64 is Decodable in Wasm) { type Carrier = Long }) = dec(_.bits.s64)
  given u8Encodable: ((U8 is Encodable in Wasm) { type Carrier = Byte }) = enc(_.byte)
  given u8Decodable: ((U8 is Decodable in Wasm) { type Carrier = Byte }) = dec(_.bits.u8)
  given u16Encodable: ((U16 is Encodable in Wasm) { type Carrier = Short }) = enc(_.bits.s16.short)
  given u16Decodable: ((U16 is Decodable in Wasm) { type Carrier = Short }) = dec(_.bits.u16)
  given u32Encodable: ((U32 is Encodable in Wasm) { type Carrier = Int }) = enc(_.bits.s32.int)
  given u32Decodable: ((U32 is Decodable in Wasm) { type Carrier = Int }) = dec(_.bits.u32)
  given u64Encodable: ((U64 is Encodable in Wasm) { type Carrier = Long }) = enc(_.bits.s64.long)
  given u64Decodable: ((U64 is Decodable in Wasm) { type Carrier = Long }) = dec(_.bits.u64)
  given boolEncodable: ((Boolean is Encodable in Wasm) { type Carrier = Boolean }) = enc(identity)
  given boolDecodable: ((Boolean is Decodable in Wasm) { type Carrier = Boolean }) = dec(identity)
  given charEncodable: ((Char is Encodable in Wasm) { type Carrier = Char }) = enc(identity)
  given charDecodable: ((Char is Decodable in Wasm) { type Carrier = Char }) = dec(identity)
  given textEncodable: ((Text is Encodable in Wasm) { type Carrier = String }) = enc(_.s)
  given textDecodable: ((Text is Decodable in Wasm) { type Carrier = String }) = dec(_.tt)

  // TODO: `F32`/`F64` (need the `Float`/`Double`->`F32`/`F64` constructors) and a WIT `list<T>`
  // codec (crosses the boundary as an `Array` of the element carrier) are not yet provided.

// A value marshalled to the carrier type it crosses a WIT function boundary as.
final class Wasm(val value: Any)
