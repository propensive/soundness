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
package xenophile

import anticipation.*
import distillate.*
import hypotenuse.*
import prepositional.*

// The uniform form of a value marshalled across a JavaScript call boundary — the JS analogue of the
// `Wasm` form. A single form lets a codec be summoned for any Scala type without knowing its JS
// carrier in advance; the carrier (a type Scala.js accepts as a `js.Any`) is recovered from the
// codec's `Carrier` member. `Encodable in Js` / `Decodable in Js` are summoned per argument/result
// by `JsInvoke`; a value lacking a codec cannot cross a JS boundary, reported at compile time.
object Js:
  def apply(value: Any): Js = new Js(value)
  extension (js: Js) def as[carrier]: carrier = js.value.asInstanceOf[carrier]

  // Builds an `Encodable in Js` that also exposes the `Carrier` type the value is passed as.
  private def enc[self, carrier0](lambda: self => carrier0)
  :   (self is Encodable in Js) { type Carrier = carrier0 } =

    new Encodable:
      type Self = self
      type Form = Js
      type Carrier = carrier0
      def encoded(value: self): Js = Js(lambda(value))

  private def dec[self, carrier0](lambda: carrier0 => self)
  :   (self is Decodable in Js) { type Carrier = carrier0 } =

    new Decodable:
      type Self = self
      type Form = Js
      type Carrier = carrier0
      type Locus = Text
      def decoded(value: Js): self = lambda(value.as[carrier0])

  // JavaScript's sole number type is an IEEE-754 double, which Scala.js represents faithfully for
  // integral carriers up to 32 bits; the fixed-width Hypotenuse types canonicalise onto `Byte`/
  // `Short`/`Int` (all Scala.js `number`s), exactly as the `Wasm` form does. (`S64`/`U64` are
  // omitted: JS has no native 64-bit integer, so they cannot cross faithfully without `BigInt` — a
  // follow-up, mirroring the `Wasm` form's `list<T>`/float TODO.)
  given s8Encodable: ((S8 is Encodable in Js) { type Carrier = Byte }) = enc(_.byte)
  given s8Decodable: ((S8 is Decodable in Js) { type Carrier = Byte }) = dec(_.bits.s8)
  given s16Encodable: ((S16 is Encodable in Js) { type Carrier = Short }) = enc(_.short)
  given s16Decodable: ((S16 is Decodable in Js) { type Carrier = Short }) = dec(_.bits.s16)
  given s32Encodable: ((S32 is Encodable in Js) { type Carrier = Int }) = enc(_.int)
  given s32Decodable: ((S32 is Decodable in Js) { type Carrier = Int }) = dec(_.bits.s32)
  given u8Encodable: ((U8 is Encodable in Js) { type Carrier = Byte }) = enc(_.byte)
  given u8Decodable: ((U8 is Decodable in Js) { type Carrier = Byte }) = dec(_.bits.u8)
  given u16Encodable: ((U16 is Encodable in Js) { type Carrier = Short }) = enc(_.bits.s16.short)
  given u16Decodable: ((U16 is Decodable in Js) { type Carrier = Short }) = dec(_.bits.u16)
  given u32Encodable: ((U32 is Encodable in Js) { type Carrier = Int }) = enc(_.bits.s32.int)
  given u32Decodable: ((U32 is Decodable in Js) { type Carrier = Int }) = dec(_.bits.u32)
  given boolEncodable: ((Boolean is Encodable in Js) { type Carrier = Boolean }) = enc(identity)
  given boolDecodable: ((Boolean is Decodable in Js) { type Carrier = Boolean }) = dec(identity)
  given textEncodable: ((Text is Encodable in Js) { type Carrier = String }) = enc(_.s)
  given textDecodable: ((Text is Decodable in Js) { type Carrier = String }) = dec(_.tt)

// A value marshalled to the carrier type it crosses a JavaScript call boundary as.
final class Js(val value: Any)
