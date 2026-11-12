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
package stratiform

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import larceny.*
import prepositional.*
import probably.*
import rudiments.*
import vacuous.*

import strategies.throwUnsafely
import Tel.given

// NB: `dynamicAccess.dynamicTel` is deliberately *not* imported here — verified
// `Tel of T` navigation must work without it.

case class Person(name: Text, age: Int) derives CanEqual
case class Duel(challenger: Person, opponent: Person) derives CanEqual
case class Troupe(name: Text, players: List[Person]) derives CanEqual
case class TextAge(name: Text, age: Text) derives CanEqual
case class Nicked(name: Text, nick: Optional[Text]) derives CanEqual
case class Config(name: Text, prefs: Map[Text, Int]) derives CanEqual

// A type whose TEL codec routes through its `… in Text` codec (a scalar atom),
// although structurally it would derive a `Struct`. Used to check the fused TEL
// schema is coherent with the codec (a `Scalar`), not the derived structure.
enum Tint derives CanEqual:
  case Pale, Deep

object Tint:
  given encodable: Tint is Encodable in Text = _.toString.tt.lower

object VerifyTests extends Suite("tel-verify", m"Stratiform verify tests"):
  def keywords(struct: Tels.Struct): List[Text] =
    struct.members.to[List].sweep:
      case field: Tels.Field => field.keyword
    . to[List]

  def run(): Unit =
    suite(m"Runtime verification"):
      test(m"A conformant value verifies and still decodes"):
        Person(t"Percy", 30).encode.verify[Person].as[Person]
      . assert(_ == Person(t"Percy", 30))

      test(m"A conformant value verifies successfully"):
        safely(Person(t"Hal", 4).encode.verify[Person]).present
      . assert(_ == true)

      test(m"A wrong-typed field fails to verify"):
        safely(TextAge(t"Hal", t"old").encode.verify[Person]).absent
      . assert(_ == true)

    suite(m"Typed navigation (no enabler import)"):
      test(m"Access a verified field"):
        Person(t"Percy", 30).encode.verify[Person].name.as[Text]
      . assert(_ == t"Percy")

      test(m"Access a verified Int field"):
        Person(t"Percy", 30).encode.verify[Person].age.as[Int]
      . assert(_ == 30)

      test(m"Access a nested verified field"):
        Duel(Person(t"Hamlet", 30), Person(t"Laertes", 25))
          .encode.verify[Duel].opponent.name.as[Text]
      . assert(_ == t"Laertes")

      test(m"Index into a verified collection field"):
        Troupe(t"Athens", List(Person(t"Quince", 1), Person(t"Bottom", 2)))
          .encode.verify[Troupe].players(1).name.as[Text]
      . assert(_ == t"Bottom")

    suite(m"Collection round-trip"):
      test(m"A list field round-trips through TEL"):
        Troupe(t"Athens", List(Person(t"Quince", 1), Person(t"Bottom", 2))).encode.as[Troupe]
      . assert(_ == Troupe(t"Athens", List(Person(t"Quince", 1), Person(t"Bottom", 2))))

      test(m"A map field round-trips through TEL"):
        Config(t"c", Map(t"a" -> 1, t"b" -> 2)).encode.as[Config]
      . assert(_ == Config(t"c", Map(t"a" -> 1, t"b" -> 2)))

    suite(m"Schema derivation"):
      test(m"A product derives a Struct with kebab-cased field keywords"):
        keywords(Tels.tels[Person](t"person").document)
      . assert(_ == List(t"name", t"age"))

      test(m"A required field has implicit polarity, as a hand-written field does"):

          Tels.tels[Person](t"person").document.members.to[List].sweep:
            case field: Tels.Field if field.keyword == t"name" => field.required
          . to[List]
      . assert(_ == List(Tels.Polarity.Implicit))

      test(m"An Optional field loosens to Loose polarity"):

          Tels.tels[Nicked](t"nicked").document.members.to[List].sweep:
            case field: Tels.Field if field.keyword == t"nick" => field.required
          . to[List]
      . assert(_ == List(Tels.Polarity.Loose))

      test(m"A collection field is repeatable, referencing the element's record"):
          val schema = Tels.tels[Troupe](t"troupe")

          schema.document.members.to[List].sweep:
            case field: Tels.Field if field.keyword == t"players" =>
              field.repeatable -> field.fieldType
          . sweep:
              case (repeatable, Tels.Reference(name)) =>
                repeatable -> schema.records.seek(_.name == name).let: record =>
                  keywords(Tels.Struct(record.members, record.validators))
                . or(List())
          . to[List]
      . assert(_ == List(Tels.Polarity.Loose -> List(t"name", t"age")))

      test(m"A map field's type is a Struct of repeatable `entries` of key/value"):

          Tels.tels[Config](t"config").document.members.to[List].sweep:
            case field: Tels.Field if field.keyword == t"prefs" => field.fieldType
          . sweep:
              case struct: Tels.Struct => struct.members.to[List].sweep:
                case field: Tels.Field if field.keyword == t"entries" => field.fieldType
          . flat.sweep:
              case struct: Tels.Struct => keywords(struct)
          . to[List]
      . assert(_ == List(List(t"key", t"value")))

    suite(m"Schema coherence (the carried shape tracks the codec)"):
      test(m"A Text-branch encoder's fused TEL schema is a Scalar, matching the codec"):
        telSchematics.encodable[Tint].schema()
      . assert:
          case _: Tels.Scalar => true
          case _              => false

    suite(m"Encodable & Schematic fusion"):
      val person = Person(t"Percy", 30)

      test(m"A fused encoder encodes (and round-trips) as Tel"):
        telSchematics.encodable[Person].encoded(person).as[Person]
      . assert(_ == person)

      test(m"A fused encoder yields a schema"):
        telSchematics.encodable[Person].schema()
      . assert:
          case _: Tels.Struct => true
          case _              => false

      test(m"A fused decoder decodes from Tel"):
        telSchematics.decodable[Person].decoded(person.encode)
      . assert(_ == person)

      test(m"A fused decoder yields a schema"):
        telSchematics.decodable[Person].schema()
      . assert:
          case _: Tels.Struct => true
          case _              => false

    suite(m"Compile-time schema checks"):
      test(m"An unknown field is rejected"):
        demilitarize:
          Person(t"Percy", 30).encode.verify[Person].nope
        . head.let(_.message)
      . assert(_.lay(false)(_.contains("has no field")))

      test(m"Indexing a non-collection field is rejected"):
        demilitarize:
          Person(t"Percy", 30).encode.verify[Person].name(0)
        . head.let(_.message)
      . assert(_.lay(false)(_.contains("not an indexable collection")))

      test(m"Field access on a scalar position is rejected"):
        demilitarize:
          Person(t"Percy", 30).encode.verify[Person].name.deeper
        . head.let(_.message)
      . assert(_.lay(false)(_.contains("has no field")))

      test(m"Plain (unverified) field access requires the enabler"):
        demilitarize:
          Person(t"Percy", 30).encode.name
        . head.let(_.message)
      . assert(_.lay(false)(_.contains("dynamicAccess.dynamicTel")))
