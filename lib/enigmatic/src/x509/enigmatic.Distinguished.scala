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
package enigmatic

import scala.caps


import anticipation.*
import prepositional.*
import rudiments.*
import vacuous.*

// A distinguished name: the X.501 structure that names a certificate's subject and issuer. Only
// the attributes that appear in practice are modelled; each is optional, and absent ones are
// simply not encoded.
object Distinguished:
  // The attribute type object identifiers, all under the X.500 attribute arc 2.5.4 except the
  // email address, which is a PKCS#9 attribute.
  private val CommonName: List[Int] = List(2, 5, 4, 3)
  private val Country: List[Int] = List(2, 5, 4, 6)
  private val Locality: List[Int] = List(2, 5, 4, 7)
  private val State: List[Int] = List(2, 5, 4, 8)
  private val Organization: List[Int] = List(2, 5, 4, 10)
  private val OrganizationalUnit: List[Int] = List(2, 5, 4, 11)
  private val Email: List[Int] = List(1, 2, 840, 113549, 1, 9, 1)

  // A `Name` is a `SEQUENCE OF RelativeDistinguishedName`, each a `SET OF AttributeTypeAndValue`.
  // Multi-valued RDNs are legal but almost unheard of, so each attribute becomes its own
  // single-member set. The order is the conventional one, most general first, which is also the
  // order `openssl x509` prints.
  given encodable: Distinguished is Encodable in Der = sequence(_).in[Der]

  private[enigmatic] def sequence(name: Distinguished): Asn1 =
    val attributes =
      List
        ( name.country.let(attribute(Country, _, printable = true)),
          name.state.let(attribute(State, _)),
          name.locality.let(attribute(Locality, _)),
          name.organization.let(attribute(Organization, _)),
          name.organizationalUnit.let(attribute(OrganizationalUnit, _)),
          name.commonName.let(attribute(CommonName, _)),
          name.email.let(attribute(Email, _, ia5 = true)) )

    Asn1.Sequence(attributes.sweep { case attribute: Asn1 => attribute })

  // `countryName` is a `PrintableString` by definition, and `emailAddress` an `IA5String`;
  // everything else is a `UTF8String`, which is what every modern profile prefers.
  private def attribute
    ( identifier: List[Int],
      value:      Text,
      printable:  Boolean = false,
      ia5:        Boolean = false )
  :   Asn1 =

    val text =
      if printable then Asn1.PrintableString(value)
      else if ia5 then Asn1.Ia5String(value)
      else Asn1.Utf8String(value)

    Asn1.Set(List(Asn1.Sequence(List(Asn1.ObjectId(identifier), text))))

case class Distinguished
  ( commonName:         Optional[Text] = Unset,
    organization:       Optional[Text] = Unset,
    organizationalUnit: Optional[Text] = Unset,
    locality:           Optional[Text] = Unset,
    state:              Optional[Text] = Unset,
    country:            Optional[Text] = Unset,
    email:              Optional[Text] = Unset )
