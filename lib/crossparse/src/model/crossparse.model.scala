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
package crossparse

import anticipation.*
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

case class LineItem(sku: Text, description: Text, quantity: Int, price: Double, taxed: Boolean)

// The per-format `Inlinable` givens are qualified: each format's staged
// component declares its own `Inlinable`, and this module imports all three
// packages.
object LineItem:
  given jsonInlinable: (LineItem is jacinta.Inlinable) = jacinta.Inlinable.derived
  given telInlinable: (LineItem is stratiform.Inlinable) = stratiform.Inlinable.derived
  given xmlInlinable: (LineItem is xylophone.Inlinable) = xylophone.Inlinable.derived

case class Customer(id: Long, name: Text, email: Text, region: Text)

object Customer:
  given jsonInlinable: (Customer is jacinta.Inlinable) = jacinta.Inlinable.derived
  given telInlinable: (Customer is stratiform.Inlinable) = stratiform.Inlinable.derived
  given xmlInlinable: (Customer is xylophone.Inlinable) = xylophone.Inlinable.derived

case class Order
  ( reference: Text, customer: Customer, items: List[LineItem], payment: Payment,
    priority: Boolean, discount: Double )

object Order:
  given jsonInlinable: (Order is jacinta.Inlinable) = jacinta.Inlinable.derived
  given telInlinable: (Order is stratiform.Inlinable) = stratiform.Inlinable.derived
  given xmlInlinable: (Order is xylophone.Inlinable) = xylophone.Inlinable.derived

case class Orders(orders: List[Order])

object Orders:
  // Direct parsing is opt-in per format; only the top type needs a nominal
  // instance — nested types resolve through each format's field fallback
  // chain.
  given jsonParsable: Orders is Json.Parsable = Json.Parsable.derived
  given telParsable: Orders is Tel.Parsable = Tel.Parsable.derived
  given xmlParsable: Orders is Xml.Parsable = Xml.Parsable.derived

  given jsonInlinable: (Orders is jacinta.Inlinable) = jacinta.Inlinable.derived
  given telInlinable: (Orders is stratiform.Inlinable) = stratiform.Inlinable.derived
  given xmlInlinable: (Orders is xylophone.Inlinable) = xylophone.Inlinable.derived
