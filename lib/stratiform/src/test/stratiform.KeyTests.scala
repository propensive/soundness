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
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package stratiform

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import charEncoders.utf8Encoder
import denominative.dysasymptotics.linearSize

// Key fields (spec §20): the `key` flag on `Field`, its schema validity
// constraints (E219–E221, §20.1), the monotone-OR layer merge (§20.3), the
// load-bearing atom-phase ordering (`key` before `default` in the TELS
// Field record, §20.5), and instance-level key uniqueness (E314, §21.6).
object KeyTests extends Suite(m"Stratiform key field tests"):

  private def schemaOf(text: Text): Tels = Tels.Reconstructor.fromTel(text.read[Tel])

  // The schema-validity code raised by composing and checking `text`, or 0.
  private def schemaCode(text: Text): Int =
    try
      Tels.Validation.validate(schemaOf(text))
      0
    catch case error: Tel.Error => error.reason.number

  case class Collected(codes: List[Int] = Nil)(using Diagnostics)
  extends Error(m"${codes.size} collected codes"):
    def +(code: Int): Collected = Collected(codes :+ code)

  // Codes accrued type-assigning `document` under the composed `schema`.
  private def assignCodes(schemaText: Text, document: Text): List[Int] =
    val schema = Tels.Validation.validate(schemaOf(schemaText))
    val tel = document.read[Tel]

    validate[Tel.Focus](Collected()):
      case error: Tel.Error => accrual + error.reason.number
    . protect:
        Tel.Type.assign(tel, schema)
        ()
    . codes

  // A schema whose repeatable `contact` member and `cat`/`dog` Select
  // variants are all keyed records, with `owner`/`admin` non-repeatable.
  private val menagerie: Text =
    Text("""|tel 1.0
        |
        |name menagerie
        |
        |record Contact
        |  field name Identifier key
        |  field email String optional repeatable
        |
        |record Cat
        |  field name Identifier key
        |
        |record Dog
        |  field name Identifier key
        |
        |select Pet
        |  variant cat Cat
        |  variant dog Dog
        |
        |document
        |  field owner Contact
        |  field admin Contact optional
        |  field contact Contact optional repeatable
        |  select Pet optional repeatable
        |""".stripMargin)

  def run(): Unit =
    suite(m"TELS axiom ordering"):
      test(m"Field record members put key at index 6, before default"):
        val field = Tels.Axiom.tels.records.readable.find(_.name == t"Field").get
        field.members.readable.toList.map:
          case f: Tels.Field => f.keyword.s
          case _             => "?"
      . assert(_ == List
          ( "keyword", "type", "optional", "required", "repeatable", "irrepeatable", "key",
            "default", "description" ))

      // §21.8 inserted `pattern` at index 2, shifting `encoding` to 3. The
      // `SemanticReconstructor` reads these positions by index, so this order
      // is load-bearing, not decorative.
      test(m"Scalar record members put pattern at index 2, before encoding"):
        val scalar = Tels.Axiom.tels.records.readable.find(_.name == t"Scalar").get
        scalar.members.readable.toList.map:
          case f: Tels.Field => f.keyword.s
          case _             => "?"
      . assert(_ == List("name", "validate", "pattern", "encoding", "description"))

    suite(m"Field declaration atom phase (§20.5)"):
      test(m"a trailing key atom sets the flag, not the default"):
        val schema = schemaOf(Text("""|tel 1.0
                                  |name t
                                  |record User
                                  |  field username Identifier key
                                  |document
                                  |  field user User
                                  |""".stripMargin))
        val field = schema.records.readable.find(_.name == t"User").get.members.readable.head
        field.absolve match
          case f: Tels.Field => (f.key, f.default.absent)
      . assert(_ == (true, true))

      test(m"the first non-flag atom after the type is the default"):
        val schema = schemaOf(Text("""|tel 1.0
                                  |name t
                                  |document
                                  |  field country String England
                                  |""".stripMargin))
        schema.document.members.readable.head.absolve match
          case f: Tels.Field => (f.key, f.default)
      . assert(_ == (false, t"England"))

      test(m"key then default: both are read, in flags-before-default order"):
        val schema = schemaOf(Text("""|tel 1.0
                                  |name t
                                  |document
                                  |  field id Identifier key fallback
                                  |""".stripMargin))
        schema.document.members.readable.head.absolve match
          case f: Tels.Field => (f.key, f.default)
      . assert(_ == (true, t"fallback"))

      test(m"key as a compound child also sets the flag"):
        val schema = schemaOf(Text("""|tel 1.0
                                  |name t
                                  |document
                                  |  field username Identifier
                                  |    key
                                  |""".stripMargin))
        schema.document.members.readable.head.absolve match
          case f: Tels.Field => f.key
      . assert(_ == true)

    suite(m"Layer merge (§20.3)"):
      test(m"a layer may mark a base field as key (monotone OR)"):
        val composed = Tels.Validation.validate:
          schemaOf(Text("""|tel 1.0
                       |name t
                       |record User
                       |  field username Identifier
                       |document
                       |  field user User optional repeatable
                       |layer keyed
                       |  record User
                       |    field username Identifier key
                       |""".stripMargin))
        composed.records.readable.find(_.name == t"User").get.members.readable.head.absolve match
          case f: Tels.Field => f.key
      . assert(_ == true)

      test(m"restating an existing key is benign, not E221"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |record User
                       |  field username Identifier key
                       |document
                       |  field user User optional repeatable
                       |layer restate
                       |  record User
                       |    field username Identifier key
                       |""".stripMargin))
      . assert(_ == 0)

      test(m"a layer keying a base-optional field must also declare required"):
        // The composed member is required, so E220 is not raised (§20.3).
        schemaCode(Text("""|tel 1.0
                       |name t
                       |record User
                       |  field username Identifier optional
                       |document
                       |  field user User optional repeatable
                       |layer keyed
                       |  record User
                       |    field username Identifier required key
                       |""".stripMargin))
      . assert(_ == 0)

    suite(m"Schema validity (E219–E221)"):
      test(m"E219: key on a field whose type is not a Scalar"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |record Inner
                       |  field label String
                       |record Outer
                       |  field inner Inner key
                       |document
                       |  field outer Outer optional repeatable
                       |""".stripMargin))
      . assert(_ == 219)

      test(m"E220: key on an optional field"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |record Item
                       |  field label Identifier optional key
                       |document
                       |  field item Item optional repeatable
                       |""".stripMargin))
      . assert(_ == 220)

      test(m"E220: key on a repeatable field"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |record Item
                       |  field label Identifier repeatable key
                       |document
                       |  field item Item optional repeatable
                       |""".stripMargin))
      . assert(_ == 220)

      test(m"E220: a layer keying a still-optional base field"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |record User
                       |  field username Identifier optional
                       |document
                       |  field user User optional repeatable
                       |layer keyed
                       |  record User
                       |    field username Identifier key
                       |""".stripMargin))
      . assert(_ == 220)

      test(m"E221: two key fields in one record"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |record Pair
                       |  field first Identifier key
                       |  field second Identifier key
                       |document
                       |  field pair Pair optional repeatable
                       |""".stripMargin))
      . assert(_ == 221)

      test(m"E221: a layer keying a second field of a keyed record"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |record Pair
                       |  field first Identifier key
                       |  field second Identifier
                       |document
                       |  field pair Pair optional repeatable
                       |layer second
                       |  record Pair
                       |    field second Identifier key
                       |""".stripMargin))
      . assert(_ == 221)

    suite(m"Key uniqueness (E314, §21.6)"):
      test(m"duplicate key values among repeatable children raise E314"):
        assignCodes(menagerie, Text("""|tel 1.0
                                   |
                                   |owner amy
                                   |contact bea
                                   |contact bea
                                   |""".stripMargin))
      . assert(_ == List(314))

      test(m"distinct key values are clean"):
        assignCodes(menagerie, Text("""|tel 1.0
                                   |
                                   |owner amy
                                   |contact bea
                                   |contact chu
                                   |""".stripMargin))
      . assert(_ == List())

      test(m"a compound-child key value participates in uniqueness"):
        assignCodes(menagerie, Text("""|tel 1.0
                                   |
                                   |owner amy
                                   |contact bea
                                   |contact
                                   |  name bea
                                   |""".stripMargin))
      . assert(_ == List(314))

      test(m"non-repeatable members are exempt: owner and admin may collide"):
        assignCodes(menagerie, Text("""|tel 1.0
                                   |
                                   |owner amy
                                   |admin amy
                                   |contact amy
                                   |""".stripMargin))
      . assert(_ == List())

      test(m"key values are unique across keywords of the same parent"):
        assignCodes(menagerie, Text("""|tel 1.0
                                   |
                                   |owner amy
                                   |cat felix
                                   |dog felix
                                   |""".stripMargin))
      . assert(_ == List(314))

      test(m"same-variant duplicates raise E314"):
        assignCodes(menagerie, Text("""|tel 1.0
                                   |
                                   |owner amy
                                   |cat felix
                                   |dog rex
                                   |cat felix
                                   |""".stripMargin))
      . assert(_ == List(314))

      test(m"default-supplied key values collide"):
        val schema = Text("""|tel 1.0
                         |name t
                         |record Item
                         |  field label Identifier key anonymous
                         |  field note String optional
                         |document
                         |  field item Item optional repeatable
                         |""".stripMargin)

        assignCodes(schema, Text("""|tel 1.0
                                |
                                |item
                                |  note first
                                |item
                                |  note second
                                |""".stripMargin))
      . assert(_ == List(314))

      test(m"an explicit key never collides with a differing default"):
        val schema = Text("""|tel 1.0
                         |name t
                         |record Item
                         |  field label Identifier key anonymous
                         |  field note String optional
                         |document
                         |  field item Item optional repeatable
                         |""".stripMargin)

        assignCodes(schema, Text("""|tel 1.0
                                |
                                |item named
                                |item
                                |  note second
                                |""".stripMargin))
      . assert(_ == List())
