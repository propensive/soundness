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

import prepositional.*

// A payload-carrying case of a WIT `variant`, for passing as an argument to a WIT function — such
// as the `ip-socket-address` (an `ipv4`/`ipv6` case wrapping a socket-address record) taken by
// `wasi:sockets`'s `start-connect`. The phantom `Topic` records the variant type and `Case` the
// selected case (both lower-kebab-case names, given as literal type arguments), while `Payload`
// preserves the argument's Scala type so `invoke` can encode it; the case must be a compile-time
// literal because the payload type differs per case, so the facade case is built with no runtime
// dispatch. `WitCase` is the payload-less counterpart.
//
// Written `WitVariant["ip-socket-address", "ipv4"](payload)`: the topic and case are explicit type
// arguments, the payload is inferred. At the downstream Wasm-link site `invoke` resolves the
// variant's facade, selects the named case, and constructs it — building any nested record/tuple
// payload from `payload` element-wise.
object WitVariant:
  transparent inline def apply[topic <: Label, name <: Label]: Applier[topic, name] =
    Applier()

  // The topic and case are fixed by the type arguments above; this second application infers the
  // payload's Scala type (which a single explicit type-argument list could not do alongside them).
  // `invoke` reads the payload type from the `WitVariant`'s type argument and the topic and case
  // from its phantom `Topic`/`Case` members.
  class Applier[topic <: Label, name <: Label]():
    transparent inline def apply[payload](payload: payload)
    :   (WitVariant[payload] of topic) { type Case = name } =
      new WitVariant(payload).asInstanceOf[(WitVariant[payload] of topic) { type Case = name }]

  given interoperable: [topic <: Label, name <: Label, payload]
  =>  ((WitVariant[payload] of topic) { type Case = name } is Interoperable in Wit of topic) =
    Interoperable()

final class WitVariant[payload](val payload: payload) extends Topical
