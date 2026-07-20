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
package crossparse

import anticipation.*
import breviloquence.*
import contingency.*, strategies.throwUnsafely
import gossamer.*
import jacinta.*
import prepositional.*
import rudiments.*
import stratiform.*
import vacuous.*
import wisteria.*
import xylophone.*

// The discriminator key the JSON derivations read at this module's
// derivation sites.
import jacinta.discriminables.jsonByKindDiscriminable

// The shared structure decoded by every format: varied primitive types, a
// nested record, a sequence and a coproduct. Compiled one phase before the
// benchmarks, so `Json.Inlinable`'s expansion-time instance evaluation can
// load the types and resolve the companion `Inlinable` givens.
enum Payment:
  case Card(number: Text, expiry: Text, secure: Boolean)
  case Transfer(iban: Text, reference: Long)

object Payment:
  // An XML sum nested inside a product cannot use xylophone's label-based
  // default `Discriminable`: the product encoder relabels the variant
  // element with the *field's* name, which destroys the discriminator. The
  // variant rides in a `type` attribute instead — `<payment type="Card">` —
  // mirroring the discriminator key the JSON and YAML corpora carry. The
  // named `DiscriminantAttribute` shape also lets the inlined XML parser
  // dispatch on the attribute straight off the open tag.
  given xmlDiscriminable: Payment is Discriminable in Xml = Xml.DiscriminantAttribute(t"type")

  // The CBOR corpus carries the same `type` discriminator as a map entry;
  // the named `DiscriminantKey` shape lets the inlined CBOR parser dispatch
  // on a scan-ahead of that entry.
  given cborDiscriminable: Payment is Discriminable in Cbor = Cbor.DiscriminantKey(t"type")

// The per-format `Inlinable` instances arrive through `derives` clauses:
// each format's carrier (`Inlinable.ForJson`, `Inlinable.ForTel`, …) is the
// `Self`-typed typeclass, synthesized into the companion, where the staging
// summons resolve them live.
case class LineItem(sku: Text, description: Text, quantity: Int, price: Double, taxed: Boolean)
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf

case class Customer(id: Long, name: Text, email: Text, region: Text)
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf

case class Order
  ( reference: Text, customer: Customer, items: List[LineItem], payment: Payment,
    priority: Boolean, discount: Double )
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf

case class Orders(orders: List[Order])
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf

object Orders:
  // Direct parsing is opt-in per format; only the top type needs a nominal
  // instance — nested types resolve through each format's field fallback
  // chain.
  given jsonParsable: Orders is Json.Parsable = Json.Parsable.derived
  given telParsable: Orders is Tel.Parsable = Tel.Parsable.derived
  given xmlParsable: Orders is Xml.Parsable = Xml.Parsable.derived

// ── The document matrix ─────────────────────────────────────────────────
// Typed translations of the jacinta benchmark corpus (a servlet config, a
// menu fragment, user records, log entries, blockchain transactions, and
// two numeric arrays), adapted so every format can represent them: uniform
// record shapes (heterogeneous parameter objects become key/value lists),
// identifier-safe field names, no sums (BinTEL cannot decode a sum in field
// position), and wrapper records for the bare arrays (Protobuf and BinTEL
// need a message root). High-precision numerics travel as `Text`, since no
// common typed representation exists across the formats.

case class Param(key: Text, value: Text)
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel

case class Servlet(servletName: Text, servletClass: Text, params: List[Param])
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel

case class Mapping(servletName: Text, url: Text)
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel

case class Taglib(uri: Text, location: Text)
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel

case class WebApp(servlets: List[Servlet], mappings: List[Mapping], taglib: Taglib)
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel

case class Config(webApp: WebApp)
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel

case class MenuItem(value: Text, onclick: Text)
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel

case class Popup(menuitems: List[MenuItem])
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel

case class Menu(id: Text, value: Text, popup: Popup)
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel

case class MenuDoc(menu: Menu)
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel

case class User(id: Int, username: Text, email: Text, active: Boolean, role: Text)
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel

case class Users(users: List[User])
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel

case class LogEntry
  ( timestamp: Long, level: Text, service: Text, requestId: Text, userId: Int, message: Text )
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel

case class Logs(logs: List[LogEntry])
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel

case class Transaction
  ( from: Text, to: Text, valueWei: Text, valueEth: Text, gasPriceWei: Text, gasUsed: Int,
    blockNumber: Long, nonce: Int, temperatureDelta: Double )
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel

case class Transactions(transactions: List[Transaction])
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel

case class Ints(values: List[Int])
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel

case class Decimals(values: List[Double])
derives jacinta.Inlinable.ForJson, stratiform.Inlinable.ForTel, xylophone.Inlinable.ForXml,
    breviloquence.Inlinable.ForCbor, locomotion.Inlinable.ForProtobuf,
    BintelInlinable.ForBintel
