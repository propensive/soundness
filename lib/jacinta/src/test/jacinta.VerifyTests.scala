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
package jacinta

import soundness.*

import proscenium.compat.*

import charEncoders.utf8Encoder
import strategies.throwUnsafely

// NB: `dynamicJsonAccess.enabled` is deliberately *not* imported here — verified
// `Json of T` navigation must work without it.

case class Employee(name: Text, age: Int, email: Text) derives CanEqual
case class Workplace(street: Text, city: Text) derives CanEqual
case class Posting(employee: Employee, workplace: Workplace) derives CanEqual
case class Squad(lead: Text, members: List[Employee]) derives CanEqual

// A type whose JSON codec routes through its `… in Text` codec (it encodes as a
// string), even though structurally it would derive an object/`oneOf` schema.
// Used to demonstrate that the *fused* schema is coherent with the codec — a
// string — rather than the independently-derived object that the old fusion
// (which summoned `Schematic` separately) would have paired with it.
enum Hue derives CanEqual:
  case Crimson, Viridian

object Hue:
  given encodable: Hue is Encodable in Text = _.toString.tt.lower

object VerifyTests extends Suite(m"Jacinta verify tests"):
  def run(): Unit =
    suite(m"Encodable & Schematic fusion"):
      val person = Employee(t"Alice", 30, t"a@b.c")

      test(m"A fused encoder encodes (and round-trips) as Json"):
        jsonSchematics.encodable[Employee].encoded(person).as[Employee]
      . assert(_ == person)

      test(m"A fused encoder yields a schema"):
        jsonSchematics.encodable[Employee].schema()
      . assert:
          case _: JsonSchema.Object => true
          case _                    => false

      test(m"A fused decoder decodes from Json"):
        val json = t"""{"name": "Alice", "age": 30, "email": "a@b.c"}""".read[Json]
        jsonSchematics.decodable[Employee].decoded(json)
      . assert(_ == person)

      test(m"A fused decoder yields a schema"):
        jsonSchematics.decodable[Employee].schema()
      . assert:
          case _: JsonSchema.Object => true
          case _                    => false

      test(m"The encoder-only instance still resolves"):
        infer[Employee is Encodable in Json].encoded(person).as[Employee]
      . assert(_ == person)

      test(m"The schema-only instance still resolves"):
        infer[Employee is Schematic over JsonSchema].schema()
      . assert:
          case _: JsonSchema.Object => true
          case _                    => false

      test(m"A fused instance can be promoted to a local given"):
        given (Employee is Decodable & Schematic in Json over JsonSchema) =
          jsonSchematics.decodable[Employee]
        summon[Employee is Decodable & Schematic in Json over JsonSchema].schema()
      . assert:
          case _: JsonSchema.Object => true
          case _                    => false

    suite(m"Schema coherence (the carried shape tracks the codec)"):
      test(m"A Text-branch encoder's fused schema is a String, matching the codec"):
        jsonSchematics.encodable[Hue].schema()
      . assert:
          case _: JsonSchema.String => true
          case _                    => false

      test(m"The standalone Schematic still derives the structural Object schema"):
        infer[Hue is Schematic over JsonSchema].schema()
      . assert:
          case _: JsonSchema.Object => true
          case _                    => false

    suite(m"Runtime verification"):
      test(m"A conformant object verifies and still decodes"):
        val json = t"""{"name": "Alice", "age": 30, "email": "a@b.c"}""".read[Json]
        json.verify[Employee].as[Employee]
      . assert(_ == Employee(t"Alice", 30, t"a@b.c"))

      test(m"A nonconformant object fails to verify"):
        val json = t"""{"name": "Bob"}""".read[Json]
        safely(json.verify[Employee]).absent
      . assert(_ == true)

      test(m"A conformant object verifies successfully"):
        val json = t"""{"name": "Bob", "age": 4, "email": "b@c.d"}""".read[Json]
        safely(json.verify[Employee]).present
      . assert(_ == true)

      test(m"A wrong-typed field fails to verify"):
        val json = t"""{"name": "Bob", "age": "old", "email": "b@c.d"}""".read[Json]
        safely(json.verify[Employee]).absent
      . assert(_ == true)

      test(m"A wrong-typed nested array element fails to verify"):
        val json = t"""{"lead": "Z", "members": [{"name": "A", "age": "x", "email": "a@a"}]}"""
                     .read[Json]
        safely(json.verify[Squad]).absent
      . assert(_ == true)

    suite(m"Typed navigation (no enabler import)"):
      test(m"Access a verified field"):
        val json = t"""{"name": "Alice", "age": 30, "email": "a@b.c"}""".read[Json]
        json.verify[Employee].name.as[Text]
      . assert(_ == t"Alice")

      test(m"Access a verified Int field"):
        val json = t"""{"name": "Alice", "age": 30, "email": "a@b.c"}""".read[Json]
        json.verify[Employee].age.as[Int]
      . assert(_ == 30)

      test(m"Access a nested verified field"):
        val json = t"""{"employee": {"name": "Bob", "age": 2, "email": "b@c.d"},
                        "workplace": {"street": "Main", "city": "Town"}}""".read[Json]
        json.verify[Posting].workplace.city.as[Text]
      . assert(_ == t"Town")

      test(m"Index into a verified array field"):
        val json = t"""{"lead": "Z",
                        "members": [{"name": "A", "age": 1, "email": "a@a"},
                                    {"name": "B", "age": 2, "email": "b@b"}]}""".read[Json]
        json.verify[Squad].members(1).name.as[Text]
      . assert(_ == t"B")

    suite(m"Compile-time schema checks"):
      test(m"An unknown field is rejected"):
        demilitarize:
          t"""{"name": "Alice", "age": 30, "email": "a@b.c"}""".read[Json].verify[Employee].nope
        . head.message
      . assert(_.contains("has no field"))

      test(m"Indexing a non-array field is rejected"):
        demilitarize:
          t"""{"name": "Alice", "age": 30, "email": "a@b.c"}""".read[Json].verify[Employee].name(0)
        . head.message
      . assert(_.contains("not an indexable array"))

      test(m"Field access on a scalar position is rejected"):
        demilitarize:
          t"""{"name": "Alice", "age": 30, "email": "a@b.c"}""".read[Json].verify[Employee].name.deeper
        . head.message
      . assert(_.contains("has no field"))

      test(m"Plain (unverified) field access requires the enabler"):
        demilitarize:
          t"""{"name": "Alice"}""".read[Json].name
        . head.message
      . assert(_.contains("dynamicJsonAccess.enabled"))
