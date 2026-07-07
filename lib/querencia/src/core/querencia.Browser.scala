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
package querencia

import anticipation.*
import gossamer.*
import hellenism.*
import honeycomb.*
import hypotenuse.*
import prepositional.*
import vacuous.*
import xenophile.*

// The `Browser` ecosystem: the web platform's foreign types, read from a WebIDL description of the
// DOM (`/querencia/dom.idl`) with xenophile's `WebIdlDialect`. `Browser` is a distinct ecosystem
// from the general `WebIdl` one so it owns the DOM's `Interface` (and so its `Foreign` values may
// render to JavaScript and serve as Honeycomb event handlers) without monopolising `WebIdl`.
//
// Everything that must be summonable lives here in `object Browser`: because `Browser` appears in
// each summoned type (`… in Browser`, `Foreign … from Browser`), this companion is in implicit
// scope at every use site, so no imports are needed to navigate the DOM or to render a handler.
object Browser:
  // `Interoperable` markers (mirroring `xenophile.WebIdl`, scoped to `Browser`) for the WebIDL
  // primitives, so Scala literal arguments convert to `Foreign` values of the right foreign type.
  given s8: (S8 is Interoperable in Browser of "s8") = Interoperable[S8, Browser, "s8"]()
  given s16: (S16 is Interoperable in Browser of "s16") = Interoperable[S16, Browser, "s16"]()
  given s32: (S32 is Interoperable in Browser of "s32") = Interoperable[S32, Browser, "s32"]()
  given s64: (S64 is Interoperable in Browser of "s64") = Interoperable[S64, Browser, "s64"]()
  given u8: (U8 is Interoperable in Browser of "u8") = Interoperable[U8, Browser, "u8"]()
  given u16: (U16 is Interoperable in Browser of "u16") = Interoperable[U16, Browser, "u16"]()
  given u32: (U32 is Interoperable in Browser of "u32") = Interoperable[U32, Browser, "u32"]()
  given u64: (U64 is Interoperable in Browser of "u64") = Interoperable[U64, Browser, "u64"]()
  given f32: (F32 is Interoperable in Browser of "f32") = Interoperable[F32, Browser, "f32"]()
  given f64: (F64 is Interoperable in Browser of "f64") = Interoperable[F64, Browser, "f64"]()

  given boolean: (Boolean is Interoperable in Browser of "boolean") =
    Interoperable[Boolean, Browser, "boolean"]()

  // WebIDL's `DOMString`/`USVString`/`ByteString` all canonicalise to `string` in the dialect.
  given string: (Text is Interoperable in Browser of "string") =
    Interoperable[Text, Browser, "string"]()

  // A WebIDL `sequence<T>` corresponds to a Scala `List` of the element type.
  given sequence: [element, topic]
  =>  ( element is Interoperable in Browser of topic )
  =>  ( List[element] is Interoperable in Browser of ("sequence" over topic) ) =
    Interoperable[List[element], Browser, ("sequence" over topic)]()

  // WebIDL `null` (produced by reading a nullable `T?` as `T | null`) is the absent `Optional`.
  given nullable: (Unset.type is Interoperable in Browser of "null") =
    Interoperable[Unset.type, Browser, "null"]()

  // A WebIDL nullable `T?` (read as `T | null`) corresponds to a Scala `Optional`. The `Mandatable`
  // constraint identifies the mandatory type `inner`, so the instance applies only to genuine
  // optionals and never competes with `inner`'s instance.
  given optional: [inner <: value, value >: Unset.type: Mandatable to inner, topic]
  =>  ( inner is Interoperable in Browser of topic )
  =>  ( value is Interoperable in Browser of (topic | "null") ) =
    Interoperable[value, Browser, (topic | "null")]()

  // The DOM definitions: the bundled WebIDL on the classpath. Summoned by the navigation macro.
  given interface: (Interface in Browser at "/querencia/dom.idl") =
    Interface[Browser](cp"/querencia/dom.idl")

  // The Honeycomb bridge: a foreign DOM expression is a valid scripting attribute value (e.g. for
  // `onclick`), rendered to JavaScript when the page is shown. Scoped to `Browser`, so a `Foreign`
  // from another ecosystem (TypeScript, WIT) cannot be used as an event handler.
  given scripting: ((Foreign from Browser) is Attributive to Whatwg.Scripting) =
    (key, value) => (key, Javascript.serialize(value.expr))

  // The browser globals, as foreign values rooted at their JavaScript identifiers (`document`,
  // `window` — lowercase, distinct from the WebIDL interface names `Document`/`Window`, which is
  // why they are constructed directly rather than via `Foreign["Document", Browser]`, whose
  // reference is the type name).
  val document: Foreign of "Document" from Browser =
    Foreign.make(Foreign.Expression.Reference(t"document"))
    . asInstanceOf[Foreign of "Document" from Browser]

  val window: Foreign of "Window" from Browser =
    Foreign.make(Foreign.Expression.Reference(t"window"))
    . asInstanceOf[Foreign of "Window" from Browser]

trait Browser extends Ecosystem:
  type Grammar = WebIdlDialect.type
