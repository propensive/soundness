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
import hypotenuse.*
import prepositional.*
import vacuous.*

// The WebIDL ecosystem: `Interoperable` markers associating Scala types with the WebIDL types
// `WebIdlDialect` reads from `.idl` files. WebIDL's integer types map to Hypotenuse's fixed-width
// numeric types (preserving signedness and width, as canonicalised by the dialect), and its three
// string types all map to `Text`; no runtime representation is used.
object WebIdl:
  given s8: (S8 is Interoperable in WebIdl of "s8") = Interoperable[S8, WebIdl, "s8"]()
  given s16: (S16 is Interoperable in WebIdl of "s16") = Interoperable[S16, WebIdl, "s16"]()
  given s32: (S32 is Interoperable in WebIdl of "s32") = Interoperable[S32, WebIdl, "s32"]()
  given s64: (S64 is Interoperable in WebIdl of "s64") = Interoperable[S64, WebIdl, "s64"]()
  given u8: (U8 is Interoperable in WebIdl of "u8") = Interoperable[U8, WebIdl, "u8"]()
  given u16: (U16 is Interoperable in WebIdl of "u16") = Interoperable[U16, WebIdl, "u16"]()
  given u32: (U32 is Interoperable in WebIdl of "u32") = Interoperable[U32, WebIdl, "u32"]()
  given u64: (U64 is Interoperable in WebIdl of "u64") = Interoperable[U64, WebIdl, "u64"]()
  given f32: (F32 is Interoperable in WebIdl of "f32") = Interoperable[F32, WebIdl, "f32"]()
  given f64: (F64 is Interoperable in WebIdl of "f64") = Interoperable[F64, WebIdl, "f64"]()

  given boolean: (Boolean is Interoperable in WebIdl of "boolean") =
    Interoperable[Boolean, WebIdl, "boolean"]()

  // WebIDL's `DOMString`, `USVString` and `ByteString` all canonicalise to `string` in the dialect.
  given string: (Text is Interoperable in WebIdl of "string") =
    Interoperable[Text, WebIdl, "string"]()

  // A WebIDL `sequence<T>` corresponds to a Scala `List` of the element type.
  given sequence: [element, topic]
  =>  ( element is Interoperable in WebIdl of topic )
  =>  ( List[element] is Interoperable in WebIdl of ("sequence" over topic) ) =
    Interoperable[List[element], WebIdl, ("sequence" over topic)]()

  // A WebIDL `FrozenArray<T>` likewise corresponds to a Scala `List`.
  given frozenArray: [element, topic]
  =>  ( element is Interoperable in WebIdl of topic )
  =>  ( List[element] is Interoperable in WebIdl of ("FrozenArray" over topic) ) =
    Interoperable[List[element], WebIdl, ("FrozenArray" over topic)]()

  // A WebIDL `record<K, V>` corresponds to a Scala `Map`.
  given record: [key, value, keyTopic, valueTopic]
  =>  ( key is Interoperable in WebIdl of keyTopic,
        value is Interoperable in WebIdl of valueTopic )
  =>  ( Map[key, value] is Interoperable in WebIdl of ("record" over (keyTopic, valueTopic)) ) =
    Interoperable[Map[key, value], WebIdl, ("record" over (keyTopic, valueTopic))]()

  // WebIDL `null` (produced by reading a nullable `T?` as `T | null`) is the absent `Optional`.
  given nullable: (Unset.type is Interoperable in WebIdl of "null") =
    Interoperable[Unset.type, WebIdl, "null"]()

  // A WebIDL nullable `T?` (read as `T | null`) corresponds to a Scala `Optional`. The `Mandatable`
  // constraint identifies the mandatory type `inner`, so the instance applies only to genuine
  // optionals and never competes with `inner`'s instance.
  given optional: [inner <: value, value >: Unset.type: Mandatable to inner, topic]
  =>  ( inner is Interoperable in WebIdl of topic )
  =>  ( value is Interoperable in WebIdl of (topic | "null") ) =
    Interoperable[value, WebIdl, (topic | "null")]()

trait WebIdl extends Ecosystem:
  type Grammar = WebIdlDialect.type
  type Emission = "xenophile.JsInvoke"
