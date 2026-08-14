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
import fulminate.*
import gossamer.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import vacuous.*

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
  // The conversion is a pure function (`->`): the anonymous codec instance retains it, and a
  // capturing lambda would make the codec itself a capability, which `Encodable`'s pure self
  // type (rightly) forbids.
  private def enc[self, carrier0](lambda: self -> carrier0)
  :   (self is Encodable in Wasm) { type Carrier = carrier0 } =

    new Encodable:
      type Self = self
      type Form = Wasm
      type Carrier = carrier0
      def encoded(value: self): Wasm = Wasm(lambda(value))

  private def dec[self, carrier0](lambda: carrier0 -> self)
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

  // The carrier is `Array[Byte]^{}` (which `Data` already is): its erasure is the same `byte[]`
  // the WIT bridge passes, and unlike a mutable `Array[Byte]` carrier it is pure under capture
  // checking, so the codec instances stay pure fields of this object.
  given dataEncodable: ((Data is Encodable in Wasm) { type Carrier = Array[Byte]^{} }) =
    enc[Data, Array[Byte]^{}](identity(_))

  given dataDecodable: ((Data is Decodable in Wasm) { type Carrier = Array[Byte]^{} }) =
    dec[Data, Array[Byte]^{}](identity(_))

  // WitError → Wasm.Error
  object Error:
    // The failing case's lower-kebab-case name (as a `Wasm.Case` would spell it), recovered from the
    // error value's class.
    private def nameOf(value: Any): Text =
      val simple = value.getClass.getSimpleName.nn.tt
      val stripped = if simple.ends(t"$$") then simple.skip(1, Rtl) else simple
      stripped.uncamel.kebab

  // The `err` arm of a WIT `result<…>`, raised by `invoke`'s decoder. The error value (e.g. a case
  // of `wasi:filesystem`'s `error-code`) is held untyped, so this module never names its
  // (Wasm-only) class; `name` recovers which case it is — the case's lower-kebab-case name, as a
  // `Wasm.Case` would spell it — so callers can translate failures into their own error vocabulary.
  // Raised from generated code, where no `Diagnostics` can be summoned, so it supplies its own.
  case class Error(value: Any)
  extends fulminate.Error(m"the WIT import returned the error ${Error.nameOf(value)}")
    ( using errorDiagnostics.emptyDiagnostics ):

    def name: Text = Error.nameOf(value)

  // WitCase → Wasm.Case
  // A payload-less case of a WIT `variant` (or `enum`), named by its lower-kebab-case Scala-side
  // name (e.g. `get` or `dns-timeout`), for passing as an argument to a WIT function — such as the
  // `method` taken by `wasi:http`'s `outgoing-request.set-method`. The phantom `Topic` records the
  // variant type, so the value converts (via the `Interoperable` instance below) into an argument of
  // that foreign type; `invoke` selects the corresponding facade case object at runtime.
  object Case:
    def apply[topic <: Label](name: Text): Case of topic =
      new Case(name).asInstanceOf[Case of topic]

    // The case's lower-kebab-case name, recovered from a facade case object's class (the same
    // spelling `apply` accepts, and the same derivation as `Wasm.Error.name`).
    def caseName(value: Any): Text =
      val simple = value.getClass.getSimpleName.nn.tt
      val stripped = if simple.ends(t"$$") then simple.skip(1, Rtl) else simple
      stripped.uncamel.kebab

    given interoperable: [topic <: Label]
    =>  ( (Case of topic) is Interoperable in Wit of topic ) =
      Interoperable()

  final class Case(val name: Text) extends Topical

  // WitHandle → Wasm.Handle
  // An opaque handle to a WIT resource — a stateful foreign value such as an output stream or file
  // descriptor — obtained by `invoke`ing a resource-returning WIT function. The underlying value (a
  // `@WitResourceImport` facade instance, only meaningful in Wasm-compiled code) is held untyped, so
  // this module never names it; the phantom `Topic` records the WIT resource type, so the handle can
  // be navigated like any other foreign value (via the `Interoperable` instance below) to invoke the
  // resource's methods, and eventually `dispose()`d.
  object Handle:
    given interoperable: [topic <: Label]
    =>  ( (Handle of topic) is Interoperable in Wit of topic ) =
      Interoperable()

  final class Handle(val value: Any) extends Topical

  // WitVariant → Wasm.Variant
  // A payload-carrying case of a WIT `variant`, for passing as an argument to a WIT function — such
  // as the `ip-socket-address` (an `ipv4`/`ipv6` case wrapping a socket-address record) taken by
  // `wasi:sockets`'s `start-connect`. The phantom `Topic` records the variant type and `Case` the
  // selected case (both lower-kebab-case names, given as literal type arguments), while `Payload`
  // preserves the argument's Scala type so `invoke` can encode it; the case must be a compile-time
  // literal because the payload type differs per case, so the facade case is built with no runtime
  // dispatch. `Wasm.Case` is the payload-less counterpart.
  //
  // Written `Variant["ip-socket-address", "ipv4"](payload)`: the topic and case are explicit type
  // arguments, the payload is inferred. At the downstream Wasm-link site `invoke` resolves the
  // variant's facade, selects the named case, and constructs it — building any nested record/tuple
  // payload from `payload` element-wise.
  object Variant:
    transparent inline def apply[topic <: Label, name <: Label]: Applier[topic, name] =
      Applier()

    // The topic and case are fixed by the type arguments above; this second application infers the
    // payload's Scala type (which a single explicit type-argument list could not do alongside them).
    // `invoke` reads the payload type from the `Variant`'s type argument and the topic and case
    // from its phantom `Topic`/`Case` members.
    class Applier[topic <: Label, name <: Label]():
      transparent inline def apply[payload](payload: payload)
      :   (Variant[payload] of topic) { type Case = name } =
        new Variant(payload).asInstanceOf[(Variant[payload] of topic) { type Case = name }]

    given interoperable: [topic <: Label, name <: Label, payload]
    =>  ((Variant[payload] of topic) { type Case = name } is Interoperable in Wit of topic) =
      Interoperable()

  final class Variant[payload](val payload: payload) extends Topical

  // TODO: `F32`/`F64` (need the `Float`/`Double`->`F32`/`F64` constructors) and a WIT `list<T>`
  // codec (crosses the boundary as an `Array` of the element carrier) are not yet provided.

// A value marshalled to the carrier type it crosses a WIT function boundary as.
final class Wasm(val value: Any)
