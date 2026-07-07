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
package xenophile

import anticipation.*
import hypotenuse.*
import prepositional.*
import vacuous.*

// The WIT (WebAssembly Interface Types) ecosystem: `Interoperable` markers associating Scala types
// with the WIT types `WitDialect` reads from `.wit` files. WIT's sized integers map to Hypotenuse's
// fixed-width numeric types, preserving signedness and width; no runtime representation is used.
object Wit:
  given s8: (S8 is Interoperable in Wit of "s8") = Interoperable[S8, Wit, "s8"]()
  given s16: (S16 is Interoperable in Wit of "s16") = Interoperable[S16, Wit, "s16"]()
  given s32: (S32 is Interoperable in Wit of "s32") = Interoperable[S32, Wit, "s32"]()
  given s64: (S64 is Interoperable in Wit of "s64") = Interoperable[S64, Wit, "s64"]()
  given u8: (U8 is Interoperable in Wit of "u8") = Interoperable[U8, Wit, "u8"]()
  given u16: (U16 is Interoperable in Wit of "u16") = Interoperable[U16, Wit, "u16"]()
  given u32: (U32 is Interoperable in Wit of "u32") = Interoperable[U32, Wit, "u32"]()
  given u64: (U64 is Interoperable in Wit of "u64") = Interoperable[U64, Wit, "u64"]()
  given f32: (F32 is Interoperable in Wit of "f32") = Interoperable[F32, Wit, "f32"]()
  given f64: (F64 is Interoperable in Wit of "f64") = Interoperable[F64, Wit, "f64"]()

  // `flags` types resolve (in `WitDialect`) to a Hypotenuse bit-vector sized to hold their members.
  given b8: (B8 is Interoperable in Wit of "b8") = Interoperable[B8, Wit, "b8"]()
  given b16: (B16 is Interoperable in Wit of "b16") = Interoperable[B16, Wit, "b16"]()
  given b32: (B32 is Interoperable in Wit of "b32") = Interoperable[B32, Wit, "b32"]()
  given b64: (B64 is Interoperable in Wit of "b64") = Interoperable[B64, Wit, "b64"]()
  given boolean: (Boolean is Interoperable in Wit of "bool") = Interoperable[Boolean, Wit, "bool"]()
  given char: (Char is Interoperable in Wit of "char") = Interoperable[Char, Wit, "char"]()
  given string: (Text is Interoperable in Wit of "string") = Interoperable[Text, Wit, "string"]()

  // A WIT `list<T>` corresponds to a Scala `List` of the element type.
  given list: [element, topic]
  =>  ( element is Interoperable in Wit of topic )
  =>  ( List[element] is Interoperable in Wit of ("list" over topic) ) =
    Interoperable[List[element], Wit, ("list" over topic)]()

  // WIT `none` (produced by reading `option<T>` as `T | none`) is the absent `Optional` value.
  given none: (Unset.type is Interoperable in Wit of "none") =
    Interoperable[Unset.type, Wit, "none"]()

  // A WIT `option<T>` (read as `T | none`) corresponds to a Scala `Optional`. The `Mandatable`
  // constraint identifies the mandatory type `inner`, so the instance applies only to genuine
  // optionals and never competes with `inner`'s instance.
  given optional: [inner <: value, value >: Unset.type: Mandatable to inner, topic]
  =>  ( inner is Interoperable in Wit of topic )
  =>  ( value is Interoperable in Wit of (topic | "none") ) =
    Interoperable[value, Wit, (topic | "none")]()

trait Wit extends Ecosystem:
  type Grammar = WitDialect.type
