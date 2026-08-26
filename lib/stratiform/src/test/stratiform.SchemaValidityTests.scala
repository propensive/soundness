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
// (sigil validity), E202/E216 base-side select constraints, E206 layer
// field-type merges, E209/E217 reference resolution, and the layer
// `exclude` operation (§20.3) with E211 (no such variant) and E212
// (emptying a SelectDefinition referenced by a required SelectRef).
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

      test(m"the layer-selection marker '+' as sigil raises E207"):
        codeOf(schemaOf(t"tel 1.0\nname t\ndocument\n  field a String\n").copy(sigil = '+'))
      . assert(_ == 207)

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

    suite(m"Layer field-type merges (§20.3, E206)"):
      test(m"E206: a layer restates a field with a different type"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |document
                       |  field foo String
                       |layer extension
                       |  overlay
                       |    field foo Identifier
                       |""".stripMargin))
      . assert(_ == 206)

      test(m"restating a field with the same type is permitted"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |document
                       |  field foo String optional
                       |layer extension
                       |  overlay
                       |    field foo String
                       |""".stripMargin))
      . assert(_ == 0)

      test(m"restating a field referencing the same record is permitted"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |record Cat
                       |  field name Identifier
                       |document
                       |  field pet Cat optional
                       |layer extension
                       |  overlay
                       |    field pet Cat
                       |""".stripMargin))
      . assert(_ == 0)

      test(m"E206: a record-body field restated with a different type"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |record Cat
                       |  field name Identifier
                       |document
                       |  field pet Cat optional
                       |layer extension
                       |  record Cat
                       |    field name String
                       |""".stripMargin))
      . assert(_ == 206)

    suite(m"Reference resolution (E209/E217)"):
      test(m"E209: a field references an undefined TypeName"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |document
                       |  field foo Missing
                       |""".stripMargin))
      . assert(_ == 209)

      test(m"E217: a field references a SelectDefinition"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |record Cat
                       |  field name Identifier
                       |select Pet
                       |  variant cat Cat
                       |document
                       |  field foo Pet optional
                       |""".stripMargin))
      . assert(_ == 217)

      test(m"E209: a SelectRef references an undefined name"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |document
                       |  select Missing optional
                       |""".stripMargin))
      . assert(_ == 209)

      test(m"E217: a SelectRef references a record"):
        schemaCode(Text("""|tel 1.0
                       |name t
                       |record Cat
                       |  field name Identifier
                       |document
                       |  select Cat optional
                       |""".stripMargin))
      . assert(_ == 217)

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

    suite(m"Pattern constraints (§21.8)"):
      // A base scalar constrained by `patterns`, optionally refined by a layer.
      def coded(base: Text, layer: Text): Text =
        Text(s"""|tel 1.0
                 |
                 |name coded
                 |
                 |scalar Code
                 |${base.s}
                 |document
                 |  field code Code
                 |${layer.s}""".stripMargin)

      def pattern(regex: Text): Text = Text(s"  pattern ${regex.s}\n")

      test(m"a scalar constrained by a pattern alone is valid"):
        schemaCode(coded(pattern(t"[A-Z]{2}-[0-9]{4}"), t""))
      . assert(_ == 0)

      test(m"a scalar with neither validate nor pattern raises E224"):
        schemaCode(t"tel 1.0\n\nname u\n\nscalar Loose\n\ndocument\n  field x Loose\n")
      . assert(_ == 224)

      test(m"an unparseable pattern raises E222"):
        schemaCode(coded(pattern(t"[unclosed"), t""))
      . assert(_ == 222)

      test(m"a pattern using a construct outside RE2 raises E222"):
        schemaCode(coded(pattern(t"(?=foo)bar"), t""))
      . assert(_ == 222)

      test(m"E222 is reported before E223"):
        // The layer widens *and* the base pattern is invalid; the invalid
        // pattern must win, since containment cannot be asked about it.
        schemaCode(coded(pattern(t"[unclosed"), t"layer l\n  scalar Code\n    pattern .*\n"))
      . assert(_ == 222)

      test(m"a layer narrowing a pattern is accepted"):
        schemaCode(coded(pattern(t"[A-Z]{2}-[0-9]{4}"),
            t"layer regional\n  scalar Code\n    pattern (EU|UK)-[0-9]{4}\n"))
      . assert(_ == 0)

      test(m"a layer widening a pattern raises E223"):
        schemaCode(coded(pattern(t"[A-Z]{2}-[0-9]{4}"),
            t"layer regional\n  scalar Code\n    pattern (EU|USA)-[0-9]{4}\n"))
      . assert(_ == 223)

      test(m"a layer with no pattern lines inherits the base's"):
        val schema = Tels.Validation.validate(schemaOf(coded(pattern(t"[A-Z]{2}-[0-9]{4}"),
            t"layer regional\n  scalar Code\n    encoding hex\n")))

        schema.scalars.readable.find(_.name == t"Code").get.patterns.readable.map(_.s).toList
      . assert(_ == List("[A-Z]{2}-[0-9]{4}"))

      test(m"restating an identical list needs no containment decision"):
        // A word boundary is `Unverifiable`, so this only passes if the
        // textual-identity short-circuit fires before any decision is made.
        schemaCode(coded(pattern(t"\\bfoo\\b"),
            t"layer l\n  scalar Code\n    pattern \\bfoo\\b\n"))
      . assert(_ == 0)

      test(m"an undecidable replacement fails closed as E223"):
        schemaCode(coded(pattern(t"[a-z]+"), t"layer l\n  scalar Code\n    pattern \\bfoo\\b\n"))
      . assert(_ == 223)

      // Neither replacement pattern alone is contained in the base, but their
      // intersection is: the n-ary containment decision is what accepts this.
      test(m"a multi-pattern replacement is decided as an intersection"):
        schemaCode(coded(pattern(t"[A-Z]{2}-[0-9]{4}"),
            t"layer l\n  scalar Code\n    pattern [A-Z]{2}-[0-9]{4}|[A-Z]{2}-[0-9]{2}\n" +
            t"    pattern [A-Z]{2}-[0-9]{4}|[A-Z]{2}-[0-9]{6}\n"))
      . assert(_ == 0)

      test(m"patterns survive a BinTEL round-trip of the schema document"):
        val doc = coded(pattern(t"[A-Z]{2}-[0-9]{4}"), t"").read[Tel]
        val element = Bintel.decode(doc.bintel(Tels.Axiom.tels), Tels.Axiom.tels)
        val composed = Tels.Validation.validate(Tels.SemanticReconstructor.fromElement(element))
        composed.scalars.readable.find(_.name == t"Code").get.patterns.readable.map(_.s).toList
      . assert(_ == List("[A-Z]{2}-[0-9]{4}"))

    suite(m"Pattern value checks (§21.8)"):
      val schema = Tels.Validation.validate(schemaOf
         (t"tel 1.0\n\nname coded\n\nscalar Code\n  pattern [0-9]+\n\ndocument\n  field code Code\n"))

      def valueCode(text: Text): Int =
        try
          Tel.Type.assign(text.read[Tel], schema, Tel.Validator.Registry.builtins)
          0
        catch case error: Tel.Error => error.reason.number

      test(m"a value matching the pattern is accepted")(valueCode(t"code 1234\n"))
      . assert(_ == 0)

      test(m"a value failing the pattern raises E315")(valueCode(t"code 12a4\n"))
      . assert(_ == 315)

      // §21.8 matches the *entire* value text, as if `\\A(?:p)\\z`, so a
      // partial match is a failure rather than a success.
      test(m"a leading partial match still raises E315")(valueCode(t"code 12x\n"))
      . assert(_ == 315)

      test(m"a trailing partial match still raises E315")(valueCode(t"code x12\n"))
      . assert(_ == 315)

      test(m"multiple patterns AND-conjoin"):
        val both = Tels.Validation.validate(schemaOf(t"tel 1.0\n\nname coded\n\nscalar Code\n" +
            t"  pattern [0-9]+\n  pattern ..\n\ndocument\n  field code Code\n"))

        def code(text: Text): Int =
          try
            Tel.Type.assign(text.read[Tel], both, Tel.Validator.Registry.builtins)
            0
          catch case error: Tel.Error => error.reason.number

        (code(t"code 12\n"), code(t"code 123\n"))
      . assert(_ == (0, 315))
