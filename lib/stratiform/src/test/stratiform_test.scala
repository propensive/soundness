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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import scala.language.unsafeNulls

import anticipation.*
import contingency.*
import fulminate.*
import probably.*
import rudiments.*
import vacuous.*

import strategies.throwUnsafely
import errorDiagnostics.stackTraces
import Tel.given

object Tests extends Suite(m"Stratiform Tests"):
  case class Person(name: Text, age: Int) derives CanEqual


  def run(): Unit =
    suite(m"Positive corpus"):
      CorpusLoader.positive.each: testcase =>
        test(m"parses ${testcase.stem}"):
          val parsed = Tel.parse(testcase.source)
          TelCheckTree.of(parsed)
        . assert(_ == CheckFormat.parse(testcase.check).tree)

    suite(m"Round-trip print → parse"):
      CorpusLoader.positive.each: testcase =>
        test(m"round-trip ${testcase.stem}"):
          val first = Tel.parse(testcase.source)
          val printed = Tel.show(first)
          val reparsed = Tel.parse(IArray.from(printed.s.getBytes("UTF-8")))
          TelCheckTree.of(reparsed)
        . assert(_ == TelCheckTree.of(Tel.parse(testcase.source)))

    suite(m"Encode/decode primitives"):
      test(m"Text round-trip"):
        Text("hello").encode.as[Text]
      . assert(_ == Text("hello"))

      test(m"Int round-trip"):
        42.encode.as[Int]
      . assert(_ == 42)

      test(m"Boolean round-trip"):
        true.encode.as[Boolean]
      . assert(identity)

      test(m"Long round-trip"):
        1234567890123L.encode.as[Long]
      . assert(_ == 1234567890123L)

    suite(m"Wisteria derivation"):
      test(m"case class round-trip"):
        Tests.Person(Text("Alice"), 30).encode.as[Tests.Person]
      . assert(_ == Tests.Person(Text("Alice"), 30))

    suite(m"tel\"…\" interpolator"):
      test(m"simple literal"):
        val parsed = tel"hello"
        parsed.childCompounds.headOption.map(_.keyword).getOrElse(Text(""))
      . assert(_ == Text("hello"))

      test(m"keyword with atom and hole"):
        val alice = Text("Alice")
        val parsed = tel"name $alice"
        parsed.childCompounds.headOption.map(c =>
          (c.keyword, c.atoms.collect { case Tel.Atom.Inline(t, _) => t }.headOption.getOrElse(Text(""))))
          .getOrElse((Text(""), Text("")))
      . assert(_ == (Text("name"), Text("Alice")))

      test(m"multi-line tel literal parses"):
        val parsed = tel"""parent
  child
"""
        parsed.childCompounds.headOption.map(_.keyword).getOrElse(Text(""))
      . assert(_ == Text("parent"))

    suite(m"tel\"…\" extractor"):
      test(m"literal pattern matches"):
        val input = tel"hello"
        input match
          case tel"hello" => true
          case _          => false
      . assert(identity)

      test(m"literal pattern non-match"):
        val input = tel"hello"
        input match
          case tel"goodbye" => true
          case _            => false
      . assert(!_)

      test(m"single capture binds atom text"):
        val input = tel"name Alice"
        input match
          case tel"name $name" => name.primaryAtom
          case _               => Text("")
      . assert(_ == Text("Alice"))

    suite(m"Schema axiom"):
      test(m"tel-schema axiom has the documented name"):
        TelSchemaAxiom.telSchema.name
      . assert(_ == Text("tel-schema"))

      test(m"axiom declares the Field record"):
        TelSchemaAxiom.telSchema.records.exists(_.name == Text("Field"))
      . assert(identity)

      test(m"axiom declares the four built-in scalars"):
        TelSchemaAxiom.telSchema.scalars.map(_.name).toSet
      . assert: scalars =>
          scalars == Set(Text("Identifier"), Text("TypeName"), Text("Sigil"), Text("String"))

    suite(m"Type assignment"):
      // A small hand-built schema for a `person` document with required
      // name (Scalar String) and optional age (Scalar Identifier).
      val personSchema = TelSchema(
        name     = Text("person"),
        document = TelSchema.Struct(
          members = IArray(
            TelSchema.Field
             ( TelSchema.Polarity.Implicit, TelSchema.Polarity.Implicit,
               Text("name"), TelSchema.Scalar(IArray(Text("string"))), Unset ),
            TelSchema.Field
             ( TelSchema.Polarity.Loose, TelSchema.Polarity.Implicit,
               Text("age"), TelSchema.Scalar(IArray(Text("identifier"))), Unset )),
          validators = IArray.empty),
        layers   = IArray.empty,
        sigil    = Unset,
        records  = IArray.empty,
        scalars  = IArray.empty,
        selects  = IArray.empty)

      test(m"assigns Value for present scalar field"):
        val doc = Tel.parseDocument(IArray.from("name Alice\nage 30\n".getBytes("UTF-8")))
        val root = TelTypeAssignment.assign(doc, personSchema)
        root match
          case TelElement.Node(_, _, children) =>
            children.collect:
              case TelElement.Value(_, _, t) => t
            .toList

          case _ => Nil
      . assert(_ == List(Text("Alice"), Text("30")))

      test(m"raises E307 when required scalar field is missing"):
        val doc = Tel.parseDocument(IArray.from("age 30\n".getBytes("UTF-8")))
        capture[TelError](TelTypeAssignment.assign(doc, personSchema)).reason
      . assert(_ == TelError.Reason.RequiredMemberAbsent)

    suite(m"Dynamic access"):
      import dynamicTelAccess.enabled

      test(m"select-dynamic on encoded case class"):
        val doc = Tests.Person(Text("Alice"), 30).encode
        doc.name.as[Text]
      . assert(_ == Text("Alice"))

      test(m"camelCase → kebab-case keyword lookup"):
        case class CamelCase(firstName: Text, lastName: Text) derives CanEqual
        val cc = CamelCase(Text("Alice"), Text("Anderson")).encode
        cc.firstName.as[Text]
      . assert(_ == Text("Alice"))

    suite(m"Negative corpus (E1xx parsing)"):
      CorpusLoader.negative.each: testcase =>
        CorpusLoader.expectedCode(testcase.stem).let: code =>
          // Phase 1 covers E1xx parsing errors only. E2xx (schema validity)
          // and E3xx (validation) require the schema component shipped in
          // phase 3.
          if code < 200 then
            test(m"raises E$code on ${testcase.stem}"):
              capture[TelError](Tel.parse(testcase.source)).reason.number
            . assert(_ == code)
