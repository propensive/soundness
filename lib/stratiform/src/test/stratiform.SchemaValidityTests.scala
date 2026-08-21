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

// The schema-validity checks of §20.1 beyond keys and encodings: E207
// (sigil validity), E202/E216 base-side select constraints, and the
// layer `exclude` operation (§20.3) with E211 (no such variant) and
// E212 (emptying a SelectDefinition referenced by a required SelectRef).
object SchemaValidityTests extends Suite(m"Stratiform schema validity tests"):

  private def schemaOf(text: Text): Tels = Tels.Reconstructor.fromTel(text.read[Tel])

  // The schema-validity code raised by composing and checking `schema`, or 0.
  private def codeOf(schema: Tels): Int =
    try
      Tels.Validation.validate(schema)
      0
    catch case error: Tel.Error => error.reason.number

  private def schemaCode(text: Text): Int = codeOf(schemaOf(text))

  // A base schema with a three-variant Select, referenced by `pet`.
  private def menagerie(required: Boolean, layerBody: Text): Text =
    val polarity = if required then t"" else t" optional"
    Text(s"""|tel 1.0
             |
             |name menagerie
             |
             |record Cat
             |  field name Identifier
             |
             |record Dog
             |  field name Identifier
             |
             |record Fox
             |  field name Identifier
             |
             |select Pet
             |  variant cat Cat
             |  variant dog Dog
             |  variant fox Fox
             |
             |document
             |  select Pet$polarity repeatable
             |${layerBody.s}""".stripMargin)

  def run(): Unit =
    suite(m"Sigil validity (E207)"):
      test(m"a parenthetical sigil raises E207"):
        codeOf(schemaOf(t"tel 1.0\nname t\ndocument\n  field a String\n").copy(sigil = '('))
      . assert(_ == 207)

      test(m"a letter sigil raises E207"):
        codeOf(schemaOf(t"tel 1.0\nname t\ndocument\n  field a String\n").copy(sigil = 'x'))
      . assert(_ == 207)

      test(m"a symbolic sigil is accepted"):
        codeOf(schemaOf(t"tel 1.0\nname t\ndocument\n  field a String\n").copy(sigil = '%'))
      . assert(_ == 0)

    suite(m"Base-side select constraints"):
      test(m"E202: a base SelectDefinition with no variants"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |select Empty
                       |  validate something
                       |document
                       |  field a String
                       |""".stripMargin))
      . assert(_ == 202)

      test(m"E216: exclude in a base SelectDefinition body"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |record Cat
                       |  field name Identifier
                       |select Pet
                       |  variant cat Cat
                       |  exclude cat
                       |document
                       |  select Pet optional
                       |""".stripMargin))
      . assert(_ == 216)

    suite(m"Layer excludes (§20.3, E211/E212)"):
      test(m"an excluded variant is removed from the composed select"):
        val composed = Tels.Validation.validate:
          schemaOf(menagerie(required = false, Text("""|layer no-foxes
                                                   |  select Pet
                                                   |    exclude fox
                                                   |""".stripMargin)))
        composed.selects.readable.find(_.name == t"Pet").get.variants.readable.map(_.keyword.s).toList
      . assert(_ == List("cat", "dog"))

      test(m"a document using an excluded variant fails with E306"):
        val composed = Tels.Validation.validate:
          schemaOf(menagerie(required = false, Text("""|layer no-foxes
                                                   |  select Pet
                                                   |    exclude fox
                                                   |""".stripMargin)))
        capture[Tel.Error](Tel.Type.assign(t"tel 1.0\n\nfox robin\n".read[Tel], composed))
        . reason.number
      . assert(_ == 306)

      test(m"E211: exclude names a variant absent from the base"):
        schemaCode(menagerie(required = false, Text("""|layer no-wolves
                                                   |  select Pet
                                                   |    exclude wolf
                                                   |""".stripMargin)))
      . assert(_ == 211)

      test(m"E211: exclude in a layer-introduced fresh select"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |record Cat
                       |  field name Identifier
                       |document
                       |  field a String
                       |layer fresh
                       |  select Pet
                       |    variant cat Cat
                       |    exclude cat
                       |""".stripMargin))
      . assert(_ == 211)

      test(m"E212: excludes emptying a required select"):
        schemaCode(menagerie(required = true, Text("""|layer none
                                                  |  select Pet
                                                  |    exclude cat
                                                  |    exclude dog
                                                  |    exclude fox
                                                  |""".stripMargin)))
      . assert(_ == 212)

      test(m"emptying an optional select is permitted"):
        schemaCode(menagerie(required = false, Text("""|layer none
                                                   |  select Pet
                                                   |    exclude cat
                                                   |    exclude dog
                                                   |    exclude fox
                                                   |""".stripMargin)))
      . assert(_ == 0)

      test(m"E212: a required SelectRef inside a record also counts"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |record Cat
                       |  field name Identifier
                       |record Owner
                       |  field name Identifier
                       |  select Pet
                       |select Pet
                       |  variant cat Cat
                       |document
                       |  field owner Owner optional repeatable
                       |layer none
                       |  select Pet
                       |    exclude cat
                       |""".stripMargin))
      . assert(_ == 212)

      test(m"excludes survive a BinTEL round-trip of the schema document"):
        // The SelectChild `exclude` variant (§20.5) must be carried by the
        // semantic reconstruction, or a layer loses its excludes when a
        // schema travels as BinTEL.
        val doc = menagerie(required = false, Text("""|layer no-foxes
                                                  |  select Pet
                                                  |    exclude fox
                                                  |""".stripMargin)).read[Tel]
        val body = doc.bintel(Tels.Axiom.tels)
        val element = Bintel.decode(body, Tels.Axiom.tels)
        val composed = Tels.Validation.validate(Tels.SemanticReconstructor.fromElement(element))
        composed.selects.readable.find(_.name == t"Pet").get.variants.readable.map(_.keyword.s).toList
      . assert(_ == List("cat", "dog"))
