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

import java.lang as jl

import adversaria.name
import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gastronomy.*
import gossamer.*
import hieroglyph.*
import panopticon.*
import prepositional.*
import probably.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

import zephyrine.lineation.linefeedByte

import strategies.throwUnsafely
import errorDiagnostics.stackTraces
import charEncoders.utf8
import charDecoders.utf8
import Tel.given

object Tests extends Suite(m"Stratiform Tests"):
  case class Person(name: Text, age: Int) derives CanEqual
  case class Renamed(@name[Tel](t"full_name") fullName: Text, @name(t"yob") year: Int)
  derives CanEqual
  case class PersonAge(name: Text, age: Int) derives CanEqual


  def run(): Unit =
    suite(m"Positive corpus"):
      CorpusLoader.positive.each: testcase =>
        test(m"parses ${testcase.stem}"):
          val parsed = testcase.source.read[Tel]
          TelCheckTree.of(parsed)
        . assert(_ == CheckFormat.parse(testcase.check).tree)

    suite(m"Round-trip print → parse"):
      CorpusLoader.positive.each: testcase =>
        test(m"round-trip ${testcase.stem}"):
          val first = testcase.source.read[Tel]
          val printed = Tel.show(first)
          val reparsed = printed.s.tt.read[Tel]
          TelCheckTree.of(reparsed)
        . assert(_ == TelCheckTree.of(testcase.source.read[Tel]))

    suite(m"Streaming parser — positive corpus"):
      CorpusLoader.positive.each: testcase =>
        test(m"streaming parses ${testcase.stem}"):
          val cursor = Cursor[Data](testcase.source)
          val doc = TelParser.parse(cursor)
          TelCheckTree.of(Tel.make(doc))
        . assert(_ == CheckFormat.parse(testcase.check).tree)

    suite(m"Streaming parser — parity with TelParser"):
      CorpusLoader.positive.each: testcase =>
        test(m"streaming matches TelParser on ${testcase.stem}"):
          val a = TelCheckTree.of(testcase.source.read[Tel])
          val b = TelCheckTree.of(Tel.make(
            TelParser.parse(Cursor[Data](testcase.source))))
          a == b
        . assert(identity)

    suite(m"Streaming parser — round-trip"):
      CorpusLoader.positive.each: testcase =>
        test(m"streaming round-trip ${testcase.stem}"):
          val first = TelParser.parse(Cursor[Data](testcase.source))
          val printed = Tel.show(Tel.make(first))
          val bytes: Data = summon[CharEncoder].encoded(printed)
          val reparsed = TelParser.parse(Cursor[Data](bytes))
          TelCheckTree.of(Tel.make(reparsed)) == TelCheckTree.of(Tel.make(first))
        . assert(identity)

    suite(m"Streaming parser — negative corpus (E1xx)"):
      CorpusLoader.negative.each: testcase =>
        val codes = CorpusLoader.expectedCodes(testcase)
        if !codes.nil && codes.all(_ < 200) then
          test(m"streaming raises an expected E1xx error on ${testcase.stem}"):
            codes.contains:
              capture[TelError](TelParser.parse(Cursor[Data](testcase.source)))
              .reason.number
          . assert(_ == true)

    suite(m"Streaming parser — chunk-boundary fuzz"):
      def chunkedCursor(data: Data, n: Int): Cursor[Data] =
        val it = new Iterator[Data]:
          var p: Int = 0
          def hasNext: Boolean = p < data.length
          def next(): Data =
            val end = (p + n).min(data.length)
            val out: Data = data.slice(p, end)
            p = end
            out
        Cursor[Data](it)

      CorpusLoader.positive.each: testcase =>
        test(m"all chunk sizes parse identically on ${testcase.stem}"):
          val baseline = TelCheckTree.of(Tel.make(
            TelParser.parse(Cursor[Data](testcase.source))))
          val sizes = List(1, 7, 64, 1024, testcase.source.length.max(1))
          sizes.all: n =>
            val tree = TelCheckTree.of(Tel.make(
              TelParser.parse(chunkedCursor(testcase.source, n))))
            tree == baseline
        . assert(identity)

    suite(m"Encode/decode primitives"):
      test(m"Text round-trip"):
        t"hello".encode.as[Text]
      . assert(_ == t"hello")

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
        Tests.Person(t"Alice", 30).encode.as[Tests.Person]
      . assert(_ == Tests.Person(t"Alice", 30))

      test(m"@name[Tel] keyword is used verbatim (overriding camel→kebab)"):
        t"full_name Ann\nyob 1984\n".read[Tel].as[Tests.Renamed]
      . assert(_ == Tests.Renamed(t"Ann", 1984))

      test(m"@name renames round-trip"):
        Tests.Renamed(t"Ann", 1984).encode.as[Tests.Renamed]
      . assert(_ == Tests.Renamed(t"Ann", 1984))

    suite(m"`over Tel` decoder shorthand"):
      test(m"`read[T over Tel]` resolves a value directly from text"):
        t"name Alice\nage 30\n".read[Tests.Person over Tel]
      . assert(_ == Tests.Person(t"Alice", 30))

    suite(m"tel\"…\" interpolator"):
      test(m"simple literal"):
        val parsed = tel"hello"
        parsed.childCompounds.headOption.map(_.keyword).getOrElse(t"")
      . assert(_ == t"hello")

      test(m"keyword with atom and hole"):
        val alice = t"Alice"
        val parsed = tel"name $alice"
        parsed.childCompounds.headOption.map(c =>
          (c.keyword, c.atoms.collect { case Tel.Atom.Inline(t, _) => t }.headOption.getOrElse(t"")))
          .getOrElse((t"", t""))
      . assert(_ == (t"name", t"Alice"))

      test(m"multi-line tel literal parses"):
        val parsed = tel"""parent
  child
"""
        parsed.childCompounds.headOption.map(_.keyword).getOrElse(t"")
      . assert(_ == t"parent")

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
          case _               => t""
      . assert(_ == t"Alice")

      test(m"two captures across separate atoms"):
        val input = tel"contact Alice alice@example.com"
        input match
          case tel"contact $name $email" => (name.primaryAtom, email.primaryAtom)
          case _                          => (t"", t"")
      . assert(_ == (t"Alice", t"alice@example.com"))

      test(m"multiple captures within a single atom — split on hyphen"):
        val input = tel"item foo-bar"
        input match
          case tel"item $prefix-$suffix" => (prefix.primaryAtom, suffix.primaryAtom)
          case _                          => (t"", t"")
      . assert(_ == (t"foo", t"bar"))

      test(m"three captures within a single atom — split on dots"):
        val input = tel"version 1.2.3"
        input match
          case tel"version $major.$minor.$patch" =>
            (major.primaryAtom, minor.primaryAtom, patch.primaryAtom)
          case _ => (t"", t"", t"")
      . assert(_ == (t"1", t"2", t"3"))

      test(m"multi-marker non-match falls through"):
        val input = tel"item foo"  // no hyphen, no second capture site
        input match
          case tel"item $prefix-$suffix" => true
          case _                          => false
      . assert(!_)

    suite(m"tel-schema self-consistency"):
      // Phase-3 partial: parse the canonical tel-schema.tel and verify
      // it produces a valid presentation AST. Full self-consistency
      // (type-assign against the axiom and reconstruct a Tels
      // value equal to Tels.Axiom.tels) is the phase-3 merge
      // blocker — it requires the axiom's Definition shapes to match
      // the canonical document's vocabulary verbatim, including the
      // `Body` record indirection and the `Member` / `SelectChild`
      // top-level Selects.
      test(m"canonical tel-schema.tel parses without error"):
        val stream = getClass.getResourceAsStream("/stratiform/corpus/tel-schema.tel").nn
        val bytes  =
          val arr = stream.readAllBytes().nn
          stream.close()
          IArray.unsafeFromArray(arr)

        bytes.read[Tel].childCompounds.length
      . assert(_ > 0)

      test(m"canonical tel-schema.tel type-assigns against the axiom"):
        // §20.5 self-consistency: the canonical document must type-assign
        // cleanly under the hand-encoded axiom.
        val stream = getClass.getResourceAsStream("/stratiform/corpus/tel-schema.tel").nn
        val bytes  =
          val arr = stream.readAllBytes().nn
          stream.close()
          IArray.unsafeFromArray(arr)

        val doc = bytes.read[Tel]
        try
          Tel.Type.assign(doc, Tels.Axiom.tels)
          "ok"
        catch case e: TelError => s"failed-with-${e.reason}"
      . assert(_ == "ok")

      test(m"canonical tel-schema.tel reconstructs structurally equal to the axiom"):
        // The strongest §20.5 property: reconstruct a Tels from the
        // canonical document and assert it is structurally identical to
        // the hand-encoded axiom.
        val stream = getClass.getResourceAsStream("/stratiform/corpus/tel-schema.tel").nn
        val bytes  =
          val arr = stream.readAllBytes().nn
          stream.close()
          IArray.unsafeFromArray(arr)

        val doc = bytes.read[Tel]
        val reconstructed = Tels.Reconstructor.fromTel(doc)
        Tels.Reconstructor.equivalent(reconstructed, Tels.Axiom.tels)
      . assert(identity)

    suite(m"Schema axiom"):
      test(m"tel-schema axiom has the documented name"):
        Tels.Axiom.tels.name
      . assert(_ == t"tel-schema")

      test(m"axiom declares the Field record"):
        Tels.Axiom.tels.records.exists(_.name == t"Field")
      . assert(identity)

      test(m"axiom declares the four built-in scalars"):
        Tels.Axiom.tels.scalars.map(_.name).to[Set]
      . assert: scalars =>
          scalars == Set(t"Identifier", t"TypeName", t"Sigil", t"String")

    suite(m"E107 schema-aware recovery (§19.5)"):
      // A schema where a root-level `parent` field references a
      // `Parent` record, and the `Parent` record contains a field
      // `child`. The keyword `child` is admissible inside Parent but
      // NOT at the document root, so an odd-indented `child` line
      // following a `parent` line must be recovered to the deeper
      // candidate.
      val recoverSchema = Tels(
        name     = t"recover",
        document = Tels.Struct(
          members = IArray(
            Tels.Field
             ( Tels.Polarity.Implicit, Tels.Polarity.Loose,
               t"parent",
               Tels.Reference(t"Parent"),
               Unset )),
          validators = IArray.empty),
        layers   = IArray.empty,
        sigil    = Unset,
        records  = IArray(Tels.RecordDefinition(
          t"Parent",
          IArray(Tels.Field
                 ( Tels.Polarity.Loose, Tels.Polarity.Loose,
                   t"child", Tels.Scalar(IArray(t"string")), Unset )),
          IArray.empty)),
        scalars  = IArray.empty,
        selects  = IArray.empty)

      test(m"picks deeper when only deeper is valid"):
        // `child` at one space of indent (=odd) is invalid as a root
        // sibling but valid as a child of `parent`; the parser
        // recovers to the deeper interpretation, and the printer
        // re-emits at the canonical two-space indent.
        val src = summon[CharEncoder].encoded(t"parent\n child Alice\n")
        val tel = Tel.parse(src, recoverSchema)
        Tel.show(tel.document.vouch)
      . assert(_ == t"parent\n  child Alice\n")

      test(m"prefers shallower on tie"):
        // A `thing` keyword admissible at both depths via a
        // self-referential record; shallower must win.
        val tieSchema = Tels(
          name     = t"tie",
          document = Tels.Struct(
            members = IArray(Tels.Field
                            ( Tels.Polarity.Loose, Tels.Polarity.Loose,
                              t"thing", Tels.Reference(t"Thing"), Unset )),
            validators = IArray.empty),
          layers   = IArray.empty,
          sigil    = Unset,
          records  = IArray(Tels.RecordDefinition(
            t"Thing",
            IArray(Tels.Field
                   ( Tels.Polarity.Loose, Tels.Polarity.Loose,
                     t"thing", Tels.Reference(t"Thing"), Unset )),
            IArray.empty)),
          scalars  = IArray.empty,
          selects  = IArray.empty)

        // Open one level (`thing`), then an odd-indented `thing`.
        // Both depths admit `thing`; shallower wins per the
        // tie-breaker.
        val src = summon[CharEncoder].encoded(t"thing\n thing\n")
        val tel = Tel.parse(src, tieSchema)
        // The output's child compound is the shallower interpretation
        // (sibling at root) — its keyword is "thing".
        tel.childCompounds.length
      . assert(_ == 2)

      test(m"without schema, original shallower-wins still raises E107"):
        // The schema-independent parse path still aborts on odd indent.
        capture[TelError](t"parent\n child Alice\n".read[Tel]).reason
      . assert(_ == TelError.Reason.OddIndentation)

    suite(m"Error line/column positions"):
      // Parse-time errors carry an `Optional[TelError.Position]` so
      // callers can point at the offending line in the source. Validation
      // (post-parse) errors leave `position` Unset because they apply to
      // AST nodes rather than source bytes.

      test(m"BOM error is at line 1, column 1"):
        capture[TelError](t"﻿tel 1.0\n".read[Tel]).position
      . assert(_ == TelError.Position(1, 1))

      test(m"OddIndentation error reports the offending line"):
        capture[TelError](t"parent\n child Alice\n".read[Tel]).position.let(_.line)
      . assert(_ == 2)

      test(m"BadVersion error reports the pragma line"):
        capture[TelError](t"tel notaversion\n".read[Tel]).position.let(_.line)
      . assert(_ == 1)

      test(m"PragmaNotFirst error reports the misplaced pragma's line"):
        capture[TelError](t"foo bar\ntel 1.0\nbaz\n".read[Tel]).position.let(_.line)
      . assert(_ == 2)

      test(m"TrailingSpaces error reports the offending line"):
        capture[TelError](t"good\nbad   \n".read[Tel]).position.let(_.line)
      . assert(_ == 2)

      test(m"Validation error (Type.assign) leaves position Unset"):
        val schema = Tels(
          name     = t"person",
          document = Tels.Struct(
            members = IArray(
              Tels.Field
               ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
                 t"name", Tels.Scalar(IArray(t"string")), Unset )),
            validators = IArray.empty),
          layers   = IArray.empty,
          sigil    = Unset,
          records  = IArray.empty,
          scalars  = IArray.empty,
          selects  = IArray.empty)
        val doc = t"age 30\n".read[Tel]
        capture[TelError](Tel.Type.assign(doc, schema)).position
      . assert(_ == Unset)

    suite(m"Type assignment"):
      // A small hand-built schema for a `person` document with required
      // name (Scalar String) and optional age (Scalar Identifier).
      val personSchema = Tels(
        name     = t"person",
        document = Tels.Struct(
          members = IArray(
            Tels.Field
             ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
               t"name", Tels.Scalar(IArray(t"string")), Unset ),
            Tels.Field
             ( Tels.Polarity.Loose, Tels.Polarity.Implicit,
               t"age", Tels.Scalar(IArray(t"identifier")), Unset )),
          validators = IArray.empty),
        layers   = IArray.empty,
        sigil    = Unset,
        records  = IArray.empty,
        scalars  = IArray.empty,
        selects  = IArray.empty)

      test(m"assigns Value for present scalar field"):
        val doc = t"name Alice\nage 30\n".read[Tel]
        val root = Tel.Type.assign(doc, personSchema)
        root match
          case Tel.Element.Node(_, _, children) =>
            children.collect:
              case Tel.Element.Value(_, _, t) => t
            .to[List]

          case _ => Nil
      . assert(_ == List(t"Alice", t"30"))

      test(m"raises E307 when required scalar field is missing"):
        val doc = t"age 30\n".read[Tel]
        capture[TelError](Tel.Type.assign(doc, personSchema)).reason
      . assert(_ == TelError.Reason.RequiredMemberAbsent)

      // A schema with a Status SelectRef whose variants are all Flag,
      // exercising sum-type handling.
      val statusSchema = Tels(
        name     = t"status",
        document = Tels.Struct(
          members = IArray(Tels.SelectRef
           ( required   = Tels.Polarity.Implicit,
             repeatable = Tels.Polarity.Implicit,
             reference  = t"Status" )),
          validators = IArray.empty),
        layers   = IArray.empty,
        sigil    = Unset,
        records  = IArray.empty,
        scalars  = IArray.empty,
        selects  = IArray(Tels.SelectDefinition(
          name     = t"Status",
          variants = IArray(
            Tels.Variant(t"active",   Tels.Flag),
            Tels.Variant(t"archived", Tels.Flag)),
          validators = IArray.empty)))

      test(m"SelectRef variant matches compound child"):
        val doc = t"active\n".read[Tel]
        val root = Tel.Type.assign(doc, statusSchema)
        root match
          case Tel.Element.Node(_, _, children) => children.length
          case _                               => -1
      . assert(_ == 1)

      test(m"unknown SelectRef variant raises E306"):
        val doc = t"unknown\n".read[Tel]
        capture[TelError](Tel.Type.assign(doc, statusSchema)).reason
      . assert(_ == TelError.Reason.UnknownKeyword)

    suite(m"Validators"):
      val reg = Tel.Validator.Registry.builtins

      test(m"string validator accepts any text"):
        reg(Tel.Validator.Request.Scalar(t"string", t"anything"))
      . assert(_ == Tel.Validator.Response.Valid)

      test(m"identifier accepts kebab-case"):
        reg(Tel.Validator.Request.Scalar(t"identifier", t"first-name"))
      . assert(_ == Tel.Validator.Response.Valid)

      test(m"identifier rejects leading hyphen"):
        reg(Tel.Validator.Request.Scalar(t"identifier", t"-leading")) match
          case Tel.Validator.Response.Invalid(_) => true
          case _                                => false
      . assert(identity)

      test(m"type-name accepts PascalCase"):
        reg(Tel.Validator.Request.Scalar(t"type-name", t"PhoneNumber"))
      . assert(_ == Tel.Validator.Response.Valid)

      test(m"type-name rejects leading lowercase"):
        reg(Tel.Validator.Request.Scalar(t"type-name", t"phoneNumber")) match
          case Tel.Validator.Response.Invalid(_) => true
          case _                                => false
      . assert(identity)

      test(m"sigil accepts a permitted symbol"):
        reg(Tel.Validator.Request.Scalar(t"sigil", t"#"))
      . assert(_ == Tel.Validator.Response.Valid)

      test(m"sigil rejects letters"):
        reg(Tel.Validator.Request.Scalar(t"sigil", t"a")) match
          case Tel.Validator.Response.Invalid(_) => true
          case _                                => false
      . assert(identity)

      test(m"type assignment with identifier validator rejects bad identifier"):
        val schemaWithValidator = Tels(
          name     = t"ident",
          document = Tels.Struct(
            members = IArray(Tels.Field
             ( Tels.Polarity.Implicit, Tels.Polarity.Implicit, t"name",
               Tels.Scalar(IArray(t"identifier")), Unset )),
            validators = IArray.empty),
          layers  = IArray.empty,
          sigil   = Unset,
          records = IArray.empty,
          scalars = IArray.empty,
          selects = IArray.empty)

        val doc = t"name -bad\n".read[Tel]
        capture[TelError]:
          Tel.Type.assign(doc, schemaWithValidator, Tel.Validator.Registry.builtins)
        .reason
      . assert(_ == TelError.Reason.ValidatorRejected)

    suite(m"Layer composition"):
      test(m"a layer adding a field extends the document Struct"):
        val base = Tels(
          name     = t"base",
          document = Tels.Struct(
            members = IArray(Tels.Field
             ( Tels.Polarity.Implicit, Tels.Polarity.Implicit, t"name",
               Tels.Scalar(IArray(t"string")), Unset )),
            validators = IArray.empty),
          layers = IArray(Tels.Layer(
            name     = t"extra",
            overlay  = Tels.Struct(
              members = IArray(Tels.Field
               ( Tels.Polarity.Loose, Tels.Polarity.Implicit, t"email",
                 Tels.Scalar(IArray(t"string")), Unset )),
              validators = IArray.empty),
            records = IArray.empty, scalars = IArray.empty, selects = IArray.empty)),
          sigil    = Unset,
          records  = IArray.empty,
          scalars  = IArray.empty,
          selects  = IArray.empty)

        val composed = Tels.Layers.compose(base)
        composed.document.members.length
      . assert(_ == 2)

      test(m"plain as[Person] decodes a conforming document"):
        val tel = t"name Alice\nage 30\n".read[Tel]
        tel.as[Tests.PersonAge]
      . assert(_ == Tests.PersonAge(t"Alice", 30))

      test(m"asValidated validates and decodes a conforming document"):
        val schema = Tels(
          name     = t"person",
          document = Tels.Struct(
            members = IArray(
              Tels.Field
               ( Tels.Polarity.Implicit, Tels.Polarity.Implicit, t"name",
                 Tels.Scalar(IArray(t"string")), Unset ),
              Tels.Field
               ( Tels.Polarity.Implicit, Tels.Polarity.Implicit, t"age",
                 Tels.Scalar(IArray(t"string")), Unset )),
            validators = IArray.empty),
          layers  = IArray.empty,
          sigil   = Unset,
          records = IArray.empty,
          scalars = IArray.empty,
          selects = IArray.empty)

        given Tels = schema
        import Tels.Decoder.asValidated
        val tel = t"name Alice\nage 30\n".read[Tel]
        tel.asValidated[Tests.PersonAge]
      . assert(_ == Tests.PersonAge(t"Alice", 30))

      test(m"duplicate layer name raises E205"):
        val layer = Tels.Layer
         ( name    = t"dup",
           overlay = Tels.Struct(IArray.empty, IArray.empty),
           records = IArray.empty, scalars = IArray.empty, selects = IArray.empty )

        val base = Tels(
          name = t"base",
          document = Tels.Struct(IArray.empty, IArray.empty),
          layers = IArray(layer, layer),
          sigil = Unset,
          records = IArray.empty, scalars = IArray.empty, selects = IArray.empty)

        capture[TelError](Tels.Layers.compose(base)).reason
      . assert(_ == TelError.Reason.DuplicateLayerName)

    suite(m"Dynamic access"):
      import dynamicTelAccess.enabled

      test(m"select-dynamic on encoded case class"):
        val doc = Tests.Person(t"Alice", 30).encode
        doc.name.as[Text]
      . assert(_ == t"Alice")

      test(m"camelCase → kebab-case keyword lookup"):
        case class CamelCase(firstName: Text, lastName: Text) derives CanEqual
        val cc = CamelCase(t"Alice", t"Anderson").encode
        cc.firstName.as[Text]
      . assert(_ == t"Alice")

    suite(m"Mutation primitives"):
      def doc(source: String): Tel = source.tt.read[Tel]

      test(m"UpdateAtom rewrites the targeted inline atom"):
        val tel    = doc("name Alice\n")
        val ptr    = Tel.Pointer.of(t"name")
        val result = Mutation(tel, Mutation.Op.UpdateAtom(ptr, 0, t"Bob"))
        Tel.show(result.document.vouch)
      . assert(_ == t"name Bob\n")

      test(m"AttachRemark adds a remark to the targeted compound"):
        val tel    = doc("name Alice\n")
        val ptr    = Tel.Pointer.of(t"name")
        val result = Mutation(tel, Mutation.Op.AttachRemark(ptr, t"primary contact"))
        Tel.show(result.document.vouch)
      . assert(_ == t"name Alice  # primary contact\n")

      test(m"RemoveRemark drops a previously attached remark"):
        val tel    = doc("name Alice  # noted\n")
        val ptr    = Tel.Pointer.of(t"name")
        val result = Mutation(tel, Mutation.Op.RemoveRemark(ptr))
        Tel.show(result.document.vouch)
      . assert(_ == t"name Alice\n")

      test(m"Insert appends a child compound to the parent"):
        val tel    = doc("contact\n  name Alice\n")
        val newCompound = Tel.Compound
                          (t"email",
                           IArray(Tel.Atom.Inline(t"alice@example.com", 1)),
                           Unset, IArray.empty)
        val ptr    = Tel.Pointer.of(t"contact")
        val result = Mutation(tel, Mutation.Op.Insert(ptr, newCompound))
        Tel.show(result.document.vouch)
      . assert(_ == t"contact\n  name Alice\n  email alice@example.com\n")

      test(m"Delete removes the addressed compound"):
        val tel    = doc("name Alice\nemail alice@example.com\n")
        val ptr    = Tel.Pointer.of(t"email")
        val result = Mutation(tel, Mutation.Op.Delete(ptr))
        Tel.show(result.document.vouch)
      . assert(_ == t"name Alice\n")

      test(m"InsertBefore places a new sibling before the target"):
        val tel    = doc("b two\n")
        val a      = Tel.Compound
                      (t"a", IArray(Tel.Atom.Inline(t"one", 1)), Unset, IArray.empty)
        val ptr    = Tel.Pointer.of(t"b")
        val result = Mutation(tel, Mutation.Op.InsertBefore(ptr, a))
        Tel.show(result.document.vouch)
      . assert(_ == t"a one\nb two\n")

      test(m"InsertAfter places a new sibling after the target"):
        val tel    = doc("a one\n")
        val b      = Tel.Compound
                      (t"b", IArray(Tel.Atom.Inline(t"two", 1)), Unset, IArray.empty)
        val ptr    = Tel.Pointer.of(t"a")
        val result = Mutation(tel, Mutation.Op.InsertAfter(ptr, b))
        Tel.show(result.document.vouch)
      . assert(_ == t"a one\nb two\n")

      test(m"Replace swaps a compound for a new one"):
        val tel    = doc("name Alice\n")
        val replacement = Tel.Compound
                           (t"name", IArray(Tel.Atom.Inline(t"Charlie", 1)),
                            Unset, IArray.empty)
        val ptr    = Tel.Pointer.of(t"name")
        val result = Mutation(tel, Mutation.Op.Replace(ptr, replacement))
        Tel.show(result.document.vouch)
      . assert(_ == t"name Charlie\n")

      test(m"SetFlag attaches a flag-typed child compound"):
        val tel    = doc("opt\n")
        val ptr    = Tel.Pointer.of(t"opt")
        val result = Mutation(tel, Mutation.Op.SetFlag(ptr, t"enabled"))
        Tel.show(result.document.vouch)
      . assert(_ == t"opt\n  enabled\n")

      test(m"UnsetFlag removes a previously set flag"):
        val tel    = doc("opt\n  enabled\n")
        val ptr    = Tel.Pointer.of(t"opt")
        val result = Mutation(tel, Mutation.Op.UnsetFlag(ptr, t"enabled"))
        Tel.show(result.document.vouch)
      . assert(_ == t"opt\n")

      test(m"sequenced ops apply in order"):
        val tel    = doc("name Alice\n")
        val ptr    = Tel.Pointer.of(t"name")
        val ops    = Seq
                      ( Mutation.Op.UpdateAtom(ptr, 0, t"Bob"),
                        Mutation.Op.AttachRemark(ptr, t"note") )
        val result = Mutation(tel, ops)
        Tel.show(result.document.vouch)
      . assert(_ == t"name Bob  # note\n")

      test(m"pointer with no match raises PointerNotFound"):
        val tel = doc("name Alice\n")
        val ptr = Tel.Pointer.of(t"missing")
        capture[MutationError](Mutation(tel, Mutation.Op.Delete(ptr))).reason
      . assert(_ == MutationError.Reason.PointerNotFound)

      test(m"ReorderWithinGroup moves a same-keyword sibling"):
        val tel = doc("item a\nitem b\nitem c\n")
        val op  = Mutation.Op.ReorderWithinGroup(Tel.Pointer.Empty, t"item", 0, 2)
        Tel.show(Mutation(tel, op).document.vouch)
      . assert(_ == t"item b\nitem c\nitem a\n")

      test(m"ReorderWithinGroup with same old and new is a no-op"):
        val tel = doc("item a\nitem b\n")
        val op  = Mutation.Op.ReorderWithinGroup(Tel.Pointer.Empty, t"item", 1, 1)
        Tel.show(Mutation(tel, op).document.vouch)
      . assert(_ == t"item a\nitem b\n")

      test(m"ReorderWithinGroup with out-of-range index raises"):
        val tel = doc("item a\nitem b\n")
        val op  = Mutation.Op.ReorderWithinGroup(Tel.Pointer.Empty, t"item", 0, 5)
        capture[MutationError](Mutation(tel, op)).reason
      . assert(_ == MutationError.Reason.PointerNotFound)

      test(m"ReorderGroups swaps contiguous member groups"):
        val tel = doc("name Alice\nname Bob\nage 30\nage 31\n")
        val op  = Mutation.Op.ReorderGroups(Tel.Pointer.Empty, t"name", t"age")
        Tel.show(Mutation(tel, op).document.vouch)
      . assert(_ == t"age 30\nage 31\nname Alice\nname Bob\n")

      test(m"ReorderGroups raises when a group is missing"):
        val tel = doc("name Alice\n")
        val op  = Mutation.Op.ReorderGroups(Tel.Pointer.Empty, t"name", t"age")
        capture[MutationError](Mutation(tel, op)).reason
      . assert(_ == MutationError.Reason.PointerNotFound)

      test(m"Construct picks inline atoms for simple values"):
        val c = Mutation.construct(t"name", t"Alice")
        c.atoms.head match
          case Tel.Atom.Inline(text, _) => text
          case _                        => t""
      . assert(_ == t"Alice")

      test(m"Construct picks a source atom for multi-line values"):
        val c = Mutation.construct(t"note", t"first line\nsecond line\n")
        c.atoms.head match
          case _: Tel.Atom.Source  => "source"
          case _: Tel.Atom.Inline  => "inline"
          case _: Tel.Atom.Literal => "literal"
      . assert(_ == "source")

      test(m"Construct falls back to literal for blank-line payloads"):
        val c = Mutation.construct(t"note", t"first\n\nsecond\n")
        c.atoms.head match
          case _: Tel.Atom.Literal => "literal"
          case _: Tel.Atom.Source  => "source"
          case _: Tel.Atom.Inline  => "inline"
      . assert(_ == "literal")

      test(m"Construct's inline atom uses one preceding space"):
        val c = Mutation.construct(t"name", t"Alice")
        c.atoms.head match
          case Tel.Atom.Inline(_, sp) => sp
          case _                       => -1
      . assert(_ == 1)

    suite(m"Tel.fields repeated-keyword accessor"):
      test(m"fields returns all matching children in order"):
        val tel = t"item 1\nitem 2\nitem 3\n".read[Tel]
        tel.fields(t"item").map(_.primaryAtom).to[List]
      . assert(_ == List(t"1", t"2", t"3"))

      test(m"fields returns empty array when none match"):
        val tel = t"other 1\n".read[Tel]
        tel.fields(t"item").length
      . assert(_ == 0)

    suite(m".read[Tel] from Text"):
      test(m"reading a Text value gives a Tel"):
        val tel = t"name Alice\n".read[Tel]
        tel.childCompounds.headOption.map(_.keyword).getOrElse(t"")
      . assert(_ == t"name")

    suite(m".load[Tel] returns Document[Tel] with metadata"):
      test(m"prologue-free document has empty metadata"):
        val doc = t"name Alice\n".load[Tel]
        (doc.metadata.interpreterDirective.absent, doc.metadata.pragma.absent)
      . assert(_ == (true, true))

      test(m"pragma is captured in Document metadata"):
        val doc = t"tel 1.0\nname Alice\n".load[Tel]
        doc.metadata.pragma.let(_.version).or((0, 0))
      . assert(_ == (1, 0))

      test(m"Document[Tel].root parses the content"):
        val doc = t"name Alice\n".load[Tel]
        doc.root.childCompounds.headOption.map(_.keyword).getOrElse(t"")
      . assert(_ == t"name")

    suite(m"Integration: parse → mutate → print → reparse"):
      def doc(source: String): Tel = source.tt.read[Tel]

      test(m"editing through the lens preserves surrounding formatting"):
        import dynamicTelAccess.enabled
        val original = doc("# header\nname Alice\nemail a@example.com\n")
        val lens = summon["email" is Lens from Tel onto Tel]
        val updated = lens.modify(original)(_ => Tel.scalar(t"b@example.com"))
        Tel.show(updated.document.vouch)
      . assert(_ == t"# header\nname Alice\nemail b@example.com\n")

      test(m"a multi-step Edit log round-trips through the printer"):
        val original = doc("name Alice\n")
        val edited =
          original.edited
            ( Edit.at(Tel.Pointer.of(t"name")).update(t"Bob")
           ++ Edit.at(Tel.Pointer.Empty)
                  .insert(Edit.compound(t"email", t"b@example.com")) )

        val printed   = Tel.show(edited.document.vouch)
        val reparsed  = printed.s.tt.read[Tel]
        Tel.show(reparsed.document.vouch)
      . assert(_ == t"name Bob\nemail b@example.com\n")

    suite(m"Tel.modify and Lens given"):
      import dynamicTelAccess.enabled
      def doc(source: String): Tel = source.tt.read[Tel]

      test(m"modify replaces an existing field's compound"):
        val tel = doc("name Alice\n")
        val updated = tel.modify("name", Tel.scalar(t"Bob"))
        updated.selectDynamic("name").primaryAtom
      . assert(_ == t"Bob")

      test(m"modify appends when the field is absent"):
        val tel = doc("name Alice\n")
        val updated = tel.modify("email", Tel.scalar(t"a@b.c"))
        updated.selectDynamic("email").primaryAtom
      . assert(_ == t"a@b.c")

      test(m"Lens by field name reads the current value"):
        val tel = doc("name Alice\n")
        val lens = summon["name" is Lens from Tel onto Tel]
        lens(tel).primaryAtom
      . assert(_ == t"Alice")

      test(m"Lens.modify updates the field through the transform"):
        val tel = doc("name Alice\n")
        val lens = summon["name" is Lens from Tel onto Tel]
        val updated = lens.modify(tel)(_ => Tel.scalar(t"Carol"))
        updated.selectDynamic("name").primaryAtom
      . assert(_ == t"Carol")

    suite(m"Edit DSL"):
      def doc(source: String): Tel = source.tt.read[Tel]

      test(m"single-op edit changes one atom"):
        val tel  = doc("name Alice\n")
        val edit = Edit.at(Tel.Pointer.of(t"name")).update(t"Bob")
        Tel.show(tel.edited(edit).document.vouch)
      . assert(_ == t"name Bob\n")

      test(m"chained edits apply in order"):
        val tel = doc("name Alice\n")
        val edit = Edit.at(Tel.Pointer.of(t"name")).update(t"Bob")
                ++ Edit.at(Tel.Pointer.of(t"name")).attachRemark(t"note")

        Tel.show(tel.edited(edit).document.vouch)
      . assert(_ == t"name Bob  # note\n")

      test(m"Edit.compound helper builds an inline-atom compound"):
        val c = Edit.compound(t"email", t"a@b.c")
        c.keyword
      . assert(_ == t"email")

      test(m"inserting via Edit composes with deletion"):
        val tel  = doc("a 1\nb 2\n")
        val edit = Edit.at(Tel.Pointer.of(t"b")).delete
                ++ Edit.at(Tel.Pointer.of(t"a")).insertAfter(Edit.compound(t"c", t"3"))

        Tel.show(tel.edited(edit).document.vouch)
      . assert(_ == t"a 1\nc 3\n")

      test(m"noop edit returns the document unchanged"):
        val tel = doc("name Alice\n")
        Tel.show(tel.edited(Edit.noop).document.vouch)
      . assert(_ == t"name Alice\n")

    suite(m"Negative corpus (E1xx parsing)"):
      CorpusLoader.negative.each: testcase =>
        val codes = CorpusLoader.expectedCodes(testcase)
        // Phase 1 covers E1xx parsing errors only. E2xx (schema validity)
        // and E3xx (validation) require the schema component shipped in
        // phase 3. We use the .check file's reported error codes when
        // present; the captured error must be one of them, since fixture
        // filenames sometimes describe a scenario while the reference
        // parser surfaces a different code first (e.g. e118 → E117).
        if !codes.nil && codes.all(_ < 200) then
          test(m"raises an expected E1xx error on ${testcase.stem}"):
            codes.contains(capture[TelError](testcase.source.read[Tel]).reason.number)
          . assert(_ == true)

    suite(m"BASE-256 codec"):
      test(m"alphabet has 256 entries"):
        Base256.alphabet.length
      . assert(_ == 256)

      test(m"alphabet satisfies codepoint ≡ index (mod 256)"):
        (0 until 256).forall(i => Base256.alphabet(i).toInt % 256 == i)
      . assert(_ == true)

      test(m"alphabet entries are pairwise distinct"):
        Base256.alphabet.to[Set].size
      . assert(_ == 256)

      test(m"ASCII digits encode to themselves"):
        (0x30 to 0x39).forall(b => Base256.alphabet(b) == b.toChar)
      . assert(_ == true)

      test(m"ASCII uppercase letters encode to themselves"):
        (0x41 to 0x5A).forall(b => Base256.alphabet(b) == b.toChar)
      . assert(_ == true)

      test(m"ASCII lowercase letters encode to themselves"):
        (0x61 to 0x7A).forall(b => Base256.alphabet(b) == b.toChar)
      . assert(_ == true)

      test(m"round-trip all 256 byte values"):
        val data: Data = (0 to 255).map(_.toByte).toArray.asInstanceOf[IArray[Byte]]
        Base256.decode(Base256.encode(data)).to[List].scala
      . assert(_ == (0 to 255).map(_.toByte))

      test(m"empty bytes round-trip to empty text"):
        Base256.encode(IArray.empty[Byte])
      . assert(_ == t"")

      test(m"empty text round-trips to empty bytes"):
        Base256.decode(t"").length
      . assert(_ == 0)

      test(m"encoded length in characters equals input length in bytes"):
        val data: Data = (0 to 255).map(_.toByte).toArray.asInstanceOf[IArray[Byte]]
        Base256.encode(data).s.length
      . assert(_ == 256)

      test(m"permissive decode accepts non-alphabet chars by residue"):
        Base256.decode(t"A ").to[List].scala
      . assert(_ == Seq(0x41.toByte, 0x20.toByte))

      test(m"strict decode accepts the alphabet"):
        val data: Data = (0 to 255).map(_.toByte).toArray.asInstanceOf[IArray[Byte]]
        Base256.decodeStrict(Base256.encode(data)).to[List].scala
      . assert(_ == (0 to 255).map(_.toByte))

      test(m"strict decode rejects a non-alphabet char"):
        capture[Base256Error](Base256.decodeStrict(t"A B")).reason match
          case Base256Error.Reason.NotInAlphabet(pos, ch) => (pos, ch)
      . assert(_ == (1, ' '))

    suite(m"BinTEL §4 varint"):
      def hex(data: Data): String =
        val sb = new java.lang.StringBuilder
        var i = 0
        while i < data.length do
          sb.append(f"${data(i) & 0xff}%02X")
          if i + 1 < data.length then sb.append(' ')
          i += 1
        sb.toString

      val vectors: List[(Long, String)] = List(
        0L     -> "00",
        1L     -> "01",
        127L   -> "7F",
        128L   -> "80 01",
        255L   -> "FF 01",
        16383L -> "FF 7F",
        16384L -> "80 80 01"
      )

      vectors.each: (value, expected) =>
        test(m"encodes $value as $expected"):
          hex(Varint.encode(value))
        . assert(_ == expected)

        test(m"decodes $expected back to $value"):
          val parts = expected.split(" ").map(java.lang.Integer.parseInt(_, 16).toByte)
          Varint.decode(parts.asInstanceOf[IArray[Byte]], 0).value
        . assert(_ == value)

      test(m"round-trips every value in 0..1023"):
        (0L to 1023L).all: n =>
          Varint.decode(Varint.encode(n), 0).value == n
      . assert(identity)

      test(m"round-trips powers of two up to 2^62"):
        (0 to 62).map(_.toLong).all: i =>
          val n = 1L << i
          Varint.decode(Varint.encode(n), 0).value == n
      . assert(identity)

      test(m"decode returns next offset"):
        val data: Data = Array[Byte](0x80.toByte, 0x01, 0x42).asInstanceOf[IArray[Byte]]
        Varint.decode(data, 0).next
      . assert(_ == 2)

      test(m"decode raises on truncated continuation"):
        val data: Data = Array[Byte](0x80.toByte).asInstanceOf[IArray[Byte]]
        capture[VarintError](Varint.decode(data, 0)).reason
      . assert(_ == VarintError.Reason.Truncated)

      test(m"encode rejects negative input"):
        try
          Varint.encode(-1L)
          false
        catch case _: IllegalArgumentException => true
      . assert(identity)

    val nameSchema = Tels(
      name     = t"contact",
      document = Tels.Struct(
        members = IArray(Tels.Field
         ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
           t"name", Tels.Scalar(IArray(t"string")), Unset )),
        validators = IArray.empty),
      layers   = IArray.empty,
      sigil    = Unset,
      records  = IArray.empty,
      scalars  = IArray.empty,
      selects  = IArray.empty)

    def hex(data: Data): String =
      val sb = new java.lang.StringBuilder
      var i = 0
      while i < data.length do
        sb.append(f"${data(i) & 0xff}%02X")
        if i + 1 < data.length then sb.append(' ')
        i += 1
      sb.toString

    def hexBytes(s: String): Seq[Byte] =
      val arr = new Array[Byte](s.length / 2)
      var i = 0
      while i < arr.length do
        arr(i) = jl.Integer.parseInt(s.substring(i * 2, i * 2 + 2), 16).toByte
        i += 1
      IArray.unsafeFromArray(arr).to[List].scala

    suite(m"BinTEL §7 node encoder"):

      test(m"empty struct encodes as a single 00 child-count"):
        val root = Tel.Element.Node(Unset, nameSchema.document, IArray.empty)
        hex(root.bintel(nameSchema))
      . assert(_ == "00")

      test(m"single scalar child via tel.bintel(schema)"):
        hex(t"name Alice\n".read[Tel].bintel(nameSchema))
      . assert(_ == "01 00 05 41 6C 69 63 65")

      test(m"empty scalar value encodes as zero-length"):
        val scalar = Tels.Scalar(IArray.empty)
        val value = Tel.Element.Value(0, scalar, t"")
        val root = Tel.Element.Node(Unset, nameSchema.document, IArray(value))
        hex(root.bintel(nameSchema))
      . assert(_ == "01 00 00")

      test(m"UTF-8 byte length is encoded, not character count"):
        // "café" = 0x63 0x61 0x66 0xC3 0xA9 = 5 bytes, 4 chars
        val scalar = Tels.Scalar(IArray.empty)
        val value = Tel.Element.Value(0, scalar, t"café")
        val root = Tel.Element.Node(Unset, nameSchema.document, IArray(value))
        hex(root.bintel(nameSchema))
      . assert(_ == "01 00 05 63 61 66 C3 A9")

      test(m"flag node encodes as just its keyword index"):
        val flagNode = Tel.Element.Node(0, Tels.Flag, IArray.empty)
        val flagSchema = Tels(
          name     = t"feature",
          document = Tels.Struct(
            members = IArray(Tels.Field
             ( Tels.Polarity.Loose, Tels.Polarity.Implicit,
               t"enabled", Tels.Flag, Unset )),
            validators = IArray.empty),
          layers   = IArray.empty,
          sigil    = Unset,
          records  = IArray.empty,
          scalars  = IArray.empty,
          selects  = IArray.empty)
        val root = Tel.Element.Node(Unset, flagSchema.document, IArray(flagNode))
        hex(root.bintel(nameSchema))
      . assert(_ == "01 00")

      test(m"nested struct emits kidx + count + children recursively"):
        val innerScalar = Tels.Scalar(IArray.empty)
        val innerStruct = Tels.Struct(
          members = IArray(Tels.Field
           ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
             t"host", innerScalar, Unset )),
          validators = IArray.empty)
        val outerStruct = Tels.Struct(
          members = IArray(Tels.Field
           ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
             t"config", innerStruct, Unset )),
          validators = IArray.empty)

        val configNode = Tel.Element.Node(
          0, innerStruct,
          IArray(Tel.Element.Value(0, innerScalar, t"example.com")))

        val root = Tel.Element.Node(Unset, outerStruct, IArray(configNode))
        hex(root.bintel(nameSchema))
      . assert(_ == "01 00 01 00 0B 65 78 61 6D 70 6C 65 2E 63 6F 6D")

      test(m"large keyword index uses multi-byte varint"):
        val scalar = Tels.Scalar(IArray.empty)
        val value = Tel.Element.Value(128, scalar, t"x")
        val root = Tel.Element.Node(Unset, nameSchema.document, IArray(value))
        hex(root.bintel(nameSchema))
      . assert(_ == "01 80 01 01 78")

      test(m"§7.2 canonical order — children reordered by member index"):
        // Build a root whose children appear in reverse member order in
        // source. The encoder must emit them in member order so that
        // independent member groups produce identical bytes regardless
        // of source ordering.
        val scalar = Tels.Scalar(IArray.empty)
        val struct = Tels.Struct(
          members = IArray(
            Tels.Field(Tels.Polarity.Implicit, Tels.Polarity.Implicit,
                       t"first",  scalar, Unset),
            Tels.Field(Tels.Polarity.Implicit, Tels.Polarity.Implicit,
                       t"second", scalar, Unset)),
          validators = IArray.empty)
        val children = IArray(
          Tel.Element.Value(1, scalar, t"B"),
          Tel.Element.Value(0, scalar, t"A"))
        val root = Tel.Element.Node(Unset, struct, children)
        hex(root.bintel(nameSchema))
      . assert(_ == "02 00 01 41 01 01 42")

      test(m"§7.2 canonical order is stable within a member"):
        // Two values at the same member index must stay in source order.
        val scalar = Tels.Scalar(IArray.empty)
        val struct = Tels.Struct(
          members = IArray(Tels.Field(Tels.Polarity.Implicit, Tels.Polarity.Loose,
                                       t"item", scalar, Unset)),
          validators = IArray.empty)
        val children = IArray(
          Tel.Element.Value(0, scalar, t"first"),
          Tel.Element.Value(0, scalar, t"second"))
        val root = Tel.Element.Node(Unset, struct, children)
        // 2 children, then two Value(0, len, text)s.
        hex(root.bintel(nameSchema))
      . assert(_ == "02 00 05 66 69 72 73 74 00 06 73 65 63 6F 6E 64")

    suite(m"BinTEL §7.8 decoder"):
      test(m"empty struct round-trips"):
        val root = Tel.Element.Node(Unset, nameSchema.document, IArray.empty)
        val bytes = root.bintel(nameSchema)
        val decoded = Bintel.decode(bytes, nameSchema)
        decoded match
          case Tel.Element.Node(_, _, c) => c.length
          case _                          => -1
      . assert(_ == 0)

      test(m"single scalar value round-trips"):
        val original = t"name Alice\n".read[Tel]
        val bytes = original.bintel(nameSchema)
        val decoded = Bintel.decode(bytes, nameSchema)
        decoded match
          case Tel.Element.Node(_, _, children) =>
            children.to[List].collect:
              case Tel.Element.Value(_, _, t) => t
          case _ => Nil
      . assert(_ == List(t"Alice"))

      test(m"empty scalar value round-trips"):
        val scalar = Tels.Scalar(IArray.empty)
        val root = Tel.Element.Node
                    (Unset, nameSchema.document, IArray(Tel.Element.Value(0, scalar, t"")))
        val bytes = root.bintel(nameSchema)
        val decoded = Bintel.decode(bytes, nameSchema)
        decoded match
          case Tel.Element.Node(_, _, children) =>
            children.to[List].collect:
              case Tel.Element.Value(_, _, t) => t
          case _ => Nil
      . assert(_ == List(t""))

      test(m"UTF-8 multi-byte scalar round-trips"):
        val scalar = Tels.Scalar(IArray.empty)
        val root = Tel.Element.Node
                    (Unset, nameSchema.document, IArray(Tel.Element.Value(0, scalar, t"café")))
        val bytes = root.bintel(nameSchema)
        val decoded = Bintel.decode(bytes, nameSchema)
        decoded match
          case Tel.Element.Node(_, _, children) =>
            children.to[List].collect:
              case Tel.Element.Value(_, _, t) => t
          case _ => Nil
      . assert(_ == List(t"café"))

      test(m"flag element round-trips"):
        val flagSchema = Tels(
          name     = t"feature",
          document = Tels.Struct(
            members = IArray(Tels.Field
             ( Tels.Polarity.Loose, Tels.Polarity.Implicit,
               t"enabled", Tels.Flag, Unset )),
            validators = IArray.empty),
          layers   = IArray.empty,
          sigil    = Unset,
          records  = IArray.empty,
          scalars  = IArray.empty,
          selects  = IArray.empty)
        val root = Tel.Element.Node
                    (Unset, flagSchema.document, IArray(Tel.Element.Node(0, Tels.Flag, IArray.empty)))
        val bytes = root.bintel(nameSchema)
        val decoded = Bintel.decode(bytes, flagSchema)
        decoded match
          case Tel.Element.Node(_, _, IArray(Tel.Element.Node(_, Tels.Flag, _))) => true
          case _                                                                  => false
      . assert(identity)

      test(m"nested struct round-trips"):
        val innerScalar = Tels.Scalar(IArray.empty)
        val innerStruct = Tels.Struct(
          members = IArray(Tels.Field
           ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
             t"host", innerScalar, Unset )),
          validators = IArray.empty)
        val outerStruct = Tels.Struct(
          members = IArray(Tels.Field
           ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
             t"config", innerStruct, Unset )),
          validators = IArray.empty)
        val outerSchema = Tels(
          name = t"app", document = outerStruct, layers = IArray.empty,
          sigil = Unset, records = IArray.empty, scalars = IArray.empty,
          selects = IArray.empty)

        val configNode = Tel.Element.Node(
          0, innerStruct,
          IArray(Tel.Element.Value(0, innerScalar, t"example.com")))
        val root = Tel.Element.Node(Unset, outerStruct, IArray(configNode))

        val bytes = root.bintel(nameSchema)
        val decoded = Bintel.decode(bytes, outerSchema)
        decoded match
          case Tel.Element.Node(_, _, IArray(Tel.Element.Node(_, _, inner))) =>
            inner.to[List].collect:
              case Tel.Element.Value(_, _, t) => t
          case _ => Nil
      . assert(_ == List(t"example.com"))

      test(m"trailing bytes after document root raise BintelError"):
        val original = t"name Alice\n".read[Tel]
        val bytes = original.bintel(nameSchema)
        val padded = (bytes.to[List] :+ 0xff.toByte).scala.toArray.asInstanceOf[IArray[Byte]]
        capture[BintelError](Bintel.decode(padded, nameSchema)).reason
      . assert(_ == BintelError.Reason.TrailingBytes)

      test(m"truncated input raises BintelError"):
        val original = t"name Alice\n".read[Tel]
        val bytes = original.bintel(nameSchema)
        val truncated = bytes.slice(0, bytes.length - 1)
        // Either UnexpectedEoi or ValueTruncated depending on where the
        // truncation lands; both are valid framing errors.
        val reason = capture[BintelError](Bintel.decode(truncated, nameSchema)).reason
        reason == BintelError.Reason.UnexpectedEoi ||
          reason == BintelError.Reason.ValueTruncated
      . assert(identity)

      test(m"out-of-range keyword index raises BintelError"):
        // Manually craft a body with one child whose keyword index is
        // out of range for the schema. nameSchema has 1 flat-keyword
        // entry (index 0); we use index 5.
        val bytes: Data =
          Array[Byte](
            0x01,                   // child-count 1
            0x05,                   // keyword index 5 (out of range)
            0x00                    // scalar length 0
          ).asInstanceOf[IArray[Byte]]
        capture[BintelError](Bintel.decode(bytes, nameSchema)).reason
      . assert(_ == BintelError.Reason.BadKeywordIndex)

    suite(m"BinTEL §6 file framing"):
      val sig32: Data = Array.fill[Byte](32)(0x55.toByte).asInstanceOf[IArray[Byte]]
      val sig34: Data = Array.fill[Byte](34)(0xAA.toByte).asInstanceOf[IArray[Byte]]

      test(m"magic number bytes are B2 C4 B5 BB"):
        hex(Bintel.magic)
      . assert(_ == "B2 C4 B5 BB")

      test(m"frame prepends magic, signature-length varint, signature"):
        val body: Data = Array[Byte](0x01, 0x02).asInstanceOf[IArray[Byte]]
        val framed = Bintel.frame(body, sig32)
        // magic (4) + sigLen varint (1: 0x20) + signature (32) + body (2) = 39
        framed.length
      . assert(_ == 39)

      test(m"frame writes signature length immediately after magic"):
        val body: Data = Array[Byte](0x01).asInstanceOf[IArray[Byte]]
        val framed = Bintel.frame(body, sig32)
        framed.slice(0, 5).to[List].scala
      . assert(_ == Seq(0xB2.toByte, 0xC4.toByte, 0xB5.toByte, 0xBB.toByte, 0x20.toByte))

      test(m"frame rejects too-short signature"):
        val tooShort: Data = Array.fill[Byte](1)(0).asInstanceOf[IArray[Byte]]
        val body: Data     = IArray.empty[Byte]
        capture[BintelError](Bintel.frame(body, tooShort)).reason
      . assert(_ == BintelError.Reason.BadSignatureLength)

      test(m"frame rejects signature with reserved hash-size index"):
        // XOR-fold ⇒ 0xA0, naming reserved s = 10
        val bad: Data = Array[Byte](0xA0.toByte, 0, 0, 0, 0).asInstanceOf[IArray[Byte]]
        val body: Data = IArray.empty[Byte]
        capture[BintelError](Bintel.frame(body, bad)).reason
      . assert(_ == BintelError.Reason.BadSignatureLength)

      test(m"unframe recovers signature and body"):
        val body: Data = Array[Byte](0x01, 0x02, 0x03).asInstanceOf[IArray[Byte]]
        val framed = Bintel.frame(body, sig32)
        val Bintel.Framed(sig, recovered) = Bintel.unframe(framed)
        (sig.to[List].scala, recovered.to[List].scala)
      . assert(_ == (sig32.to[List].scala, Seq[Byte](0x01, 0x02, 0x03)))

      test(m"unframe rejects bad magic"):
        val bytes: Data = Array.fill[Byte](40)(0).asInstanceOf[IArray[Byte]]
        capture[BintelError](Bintel.unframe(bytes)).reason
      . assert(_ == BintelError.Reason.BadMagic)

      test(m"unframe rejects truncated input"):
        val bytes: Data =
          Array[Byte](0xB2.toByte, 0xC4.toByte, 0xB5.toByte, 0xBB.toByte, 0x20.toByte)
            .asInstanceOf[IArray[Byte]]
        capture[BintelError](Bintel.unframe(bytes)).reason
      . assert(_ == BintelError.Reason.UnexpectedEoi)

      test(m"larger signatures of permitted lengths are accepted"):
        val body: Data = Array[Byte](0x01).asInstanceOf[IArray[Byte]]
        val framed = Bintel.frame(body, sig34)
        Bintel.unframe(framed).signature.length
      . assert(_ == 34)

      test(m"frame ↔ unframe round-trip for non-trivial body"):
        val original: Data = (0 to 99).map(_.toByte).toArray.asInstanceOf[IArray[Byte]]
        val framed = Bintel.frame(original, sig32)
        val recovered = Bintel.unframe(framed).body.to[List].scala
        recovered == original.to[List].scala
      . assert(_ == true)

      test(m"tel.bintelDocument produces a file beginning with magic"):
        val bytes = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        bytes.slice(0, 4).to[List].scala
      . assert(_ == Seq(0xB2.toByte, 0xC4.toByte, 0xB5.toByte, 0xBB.toByte))

      test(m"decodeDocument round-trips through frame + decode"):
        val bytes = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        val doc = Bintel.decodeDocument(bytes, nameSchema)
        doc.root match
          case Tel.Element.Node(_, _, children) =>
            children.to[List].collect:
              case Tel.Element.Value(_, _, t) => t
          case _ => Nil
      . assert(_ == List(t"Alice"))

    suite(m"BinTEL §9 textual encoding"):
      val sig32: Data = Array.fill[Byte](32)(0x55.toByte).asInstanceOf[IArray[Byte]]

      test(m"text begins with βτελ (the four BASE-256 chars for the magic bytes)"):
        val bytes = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        Bintel.text(bytes).s.substring(0, 4)
      . assert(_ == "βτελ")

      test(m"text/fromText round-trip"):
        val source = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        val text = Bintel.text(source)
        val recovered = Bintel.fromText(text)
        recovered.to[List].scala == source.to[List].scala
      . assert(_ == true)

    suite(m"BinTEL §8.2 schema signature"):
      // BinTEL-pinned Cadence(initial = 4, regular = 2, hashSize = 32).
      def synthetic(seed: Int): Data =
        val arr = new Array[Byte](32)
        var i = 0
        while i < 32 do
          arr(i) = ((seed * 31 + i * 17) & 0xff).toByte
          i += 1
        arr.asInstanceOf[IArray[Byte]]

      val h0 = synthetic(1)
      val h1 = synthetic(2)
      val h2 = synthetic(3)

      test(m"single-component signature length is 33 (32 + 1 cadence byte)"):
        SchemaSignature.encode(List(h0)).length
      . assert(_ == 33)

      test(m"single-component signature begins with the component hash"):
        SchemaSignature.encode(List(h0)).slice(0, 32).to[List].scala == h0.to[List].scala
      . assert(_ == true)

      test(m"two-component signature length is 37 (32 + 4 + 1)"):
        SchemaSignature.encode(List(h0, h1)).length
      . assert(_ == 37)

      test(m"three-component signature length is 39 (32 + 4 + 2 + 1)"):
        SchemaSignature.encode(List(h0, h1, h2)).length
      . assert(_ == 39)

      test(m"empty hash list raises BadSignatureLength"):
        capture[BintelError](SchemaSignature.encode(Nil)).reason
      . assert(_ == BintelError.Reason.BadSignatureLength)

      test(m"wrong-size hash raises BadSignatureLength"):
        val bad: Data = Array.fill[Byte](16)(0).asInstanceOf[IArray[Byte]]
        capture[BintelError](SchemaSignature.encode(List(bad))).reason
      . assert(_ == BintelError.Reason.BadSignatureLength)

      test(m"single-component signature decodes back to the hash"):
        val sig = SchemaSignature.encode(List(h0))
        val recovered = SchemaSignature.decode(sig, List(h0).scala)
        recovered.map(_.to[List].scala) == List(h0.to[List].scala)
      . assert(_ == true)

      test(m"two-component signature round-trips through encode/decode"):
        val sig = SchemaSignature.encode(List(h0, h1))
        val recovered = SchemaSignature.decode(sig, List(h0, h1, h2).scala)
        recovered.map(_.to[List].scala) == List(h0.to[List].scala, h1.to[List].scala)
      . assert(_ == true)

      test(m"three-component signature round-trips through encode/decode"):
        val sig = SchemaSignature.encode(List(h0, h1, h2))
        val recovered = SchemaSignature.decode(sig, List(h0, h1, h2).scala)
        recovered.map(_.to[List].scala) == List(h0.to[List].scala, h1.to[List].scala, h2.to[List].scala)
      . assert(_ == true)

      test(m"decode with reserved hash-size index raises BadSignatureLength"):
        // XOR-fold ⇒ 0xA0, naming reserved s = 10
        val bad: Data = Array[Byte](0xA0.toByte, 0, 0, 0, 0).asInstanceOf[IArray[Byte]]
        capture[BintelError](SchemaSignature.decode(bad, List(h0).scala)).reason
      . assert(_ == BintelError.Reason.BadSignatureLength)

      test(m"decode raises BadSignature when library is missing components"):
        val sig = SchemaSignature.encode(List(h0, h1))
        capture[BintelError](SchemaSignature.decode(sig, List(h2).scala)).reason
      . assert(_ == BintelError.Reason.BadSignature)

    suite(m"BinTEL §8.1 schema signature from document"):
      test(m"single-component signature for a no-layer schema is 33 bytes"):
        val stream = getClass.getResourceAsStream("/stratiform/corpus/tel-schema.tel").nn
        val source =
          val arr = stream.readAllBytes().nn
          stream.close()
          IArray.unsafeFromArray(arr)

        val sig = SchemaSignature.fromDocument(source.read[Tel], Tels.Axiom.tels)
        sig.length
      . assert(_ == 33)

      test(m"no-layer schema signature begins with the 32-byte BLAKE3 value hash"):
        val stream = getClass.getResourceAsStream("/stratiform/corpus/tel-schema.tel").nn
        val source =
          val arr = stream.readAllBytes().nn
          stream.close()
          IArray.unsafeFromArray(arr)

        val sig = SchemaSignature.fromDocument(source.read[Tel], Tels.Axiom.tels)
        val bintel = Tel.Type.assign(source.read[Tel], Tels.Axiom.tels).bintel(Tels.Axiom.tels)
        val hash = Blake3.hashOf(bintel, 32)
        sig.slice(0, 32).to[List].scala == hash.to[List].scala
      . assert(_ == true)

      test(m"schema with a single layer produces a 37-byte signature"):
        val src = """tel 1.0
                    |
                    |name basic
                    |
                    |record Item
                    |  field key Identifier
                    |
                    |document
                    |  field key Identifier
                    |
                    |layer ext
                    |  scalar Number identifier
                    |""".stripMargin.tt
        val sig = SchemaSignature.fromDocument(src.read[Tel], Tels.Axiom.tels)
        sig.length
      . assert(_ == 37)

      test(m"two-layer schema produces a 39-byte signature"):
        val src = """tel 1.0
                    |
                    |name multi
                    |
                    |record Item
                    |  field key Identifier
                    |
                    |document
                    |  field key Identifier
                    |
                    |layer ext1
                    |  scalar Number identifier
                    |
                    |layer ext2
                    |  scalar Symbol identifier
                    |""".stripMargin.tt
        val sig = SchemaSignature.fromDocument(src.read[Tel], Tels.Axiom.tels)
        sig.length
      . assert(_ == 39)

    suite(m"BinTEL §3 value hash"):
      test(m"valueHash is deterministic"):
        val tel = t"name Alice\n".read[Tel]
        val a = tel.valueHash(nameSchema).data.to[List].scala
        val b = tel.valueHash(nameSchema).data.to[List].scala
        a == b
      . assert(_ == true)

      test(m"valueHash differs when value differs"):
        val a = t"name Alice\n".read[Tel].valueHash(nameSchema).data.to[List].scala
        val b = t"name Bob\n".read[Tel].valueHash(nameSchema).data.to[List].scala
        a == b
      . assert(_ == false)

      test(m"valueHash output is 32 bytes"):
        t"name Alice\n".read[Tel].valueHash(nameSchema).data.length
      . assert(_ == 32)

      test(m"§3 canonical tel-schema.tel value hash is deterministic"):
        val stream = getClass.getResourceAsStream("/stratiform/corpus/tel-schema.tel").nn
        val source =
          val arr = stream.readAllBytes().nn
          stream.close()
          IArray.unsafeFromArray(arr)

        val a = Tel.Type.assign(source.read[Tel], Tels.Axiom.tels).valueHash(Tels.Axiom.tels).data.to[List].scala
        val b = Tel.Type.assign(source.read[Tel], Tels.Axiom.tels).valueHash(Tels.Axiom.tels).data.to[List].scala
        (a.length, a == b)
      . assert(_ == (32, true))

      test(m"§3 — canonical tel-schema.tel encodes byte-for-byte against reference"):
        val telStream = getClass.getResourceAsStream("/stratiform/corpus/tel-schema.tel").nn
        val telBytes  =
          val arr = telStream.readAllBytes().nn
          telStream.close()
          IArray.unsafeFromArray(arr)

        val refStream = getClass.getResourceAsStream("/stratiform/corpus/tel-schema.bintel.hex").nn
        val refHex    =
          val arr = refStream.readAllBytes().nn
          refStream.close()
          String(arr, "UTF-8").trim

        val refBytes = hexBytes(refHex)
        val element  = Tel.Type.assign(telBytes.read[Tel], Tels.Axiom.tels)
        element.bintel(Tels.Axiom.tels).to[List].scala == refBytes
      . assert(_ == true)

      test(m"§3 — tel-schema.tel matches the normative BLAKE3-256 value hash"):
        // The single vector to which §3 of BinTEL and §20.5 of the TEL
        // Specification are both pinned.
        val telStream = getClass.getResourceAsStream("/stratiform/corpus/tel-schema.tel").nn
        val telBytes  =
          val arr = telStream.readAllBytes().nn
          telStream.close()
          IArray.unsafeFromArray(arr)

        val digest = Tel.Type.assign(telBytes.read[Tel], Tels.Axiom.tels).valueHash(Tels.Axiom.tels)
        digest.data.to[List].scala.map(b => f"${b & 0xff}%02x").mkString
      . assert(_ == "626dd8958809da354a2f8bd9f7dac1cfda7f549ecbe047eb0d8c0a17c278d517")

    suite(m"BinTEL §6.2 self-contained mode"):
      val schemaDoc = """name greeting
                        |
                        |document
                        |  field name Identifier
                        |""".stripMargin.tt
      val dataDoc = t"name Alice\n"

      def selfContained(): Data =
        dataDoc.read[Tel].bintelSelfContained(schemaDoc.read[Tel])

      test(m"self-contained document begins with the B2 C4 B5 BC magic"):
        selfContained().slice(0, 4).to[List].scala
      . assert(_ == Seq[Byte](0xb2.toByte, 0xc4.toByte, 0xb5.toByte, 0xbc.toByte))

      test(m"self-contained text form begins with βτεμ"):
        Bintel.text(selfContained()).s.substring(0, 4)
      . assert(_ == "βτεμ")

      test(m"round-trips: decode recovers the single document-root child"):
        Bintel.decodeDocumentSelfContained(selfContained()).root match
          case Tel.Element.Node(_, _, children) => children.length
          case _                                => -1
      . assert(_ == 1)

      test(m"value hash is mode-independent (external == self-contained)"):
        val schema = Tels.Layers.compose(Tels.Reconstructor.fromTel(schemaDoc.read[Tel]))
        val external = dataDoc.read[Tel].bintel(schema)
        val recovered = Bintel.decodeDocumentSelfContained(selfContained()).root.bintel(schema)
        recovered.to[List].scala == external.to[List].scala
      . assert(_ == true)

      test(m"signature not matching the embedded schema raises B11"):
        val axiom      = Tels.Axiom.tels
        val sd         = schemaDoc.read[Tel]
        val schemaBody = sd.bintel(axiom)
        val schema     = Tels.Layers.compose(Tels.Reconstructor.fromTel(sd))
        val docBody    = dataDoc.read[Tel].bintel(schema)
        // A valid-length but wrong signature: flip the first body byte and
        // the trailing cadence byte so the XOR-fold length check still passes.
        val wrong = SchemaSignature.fromDocument(sd, axiom).asInstanceOf[Array[Byte]].clone()
        wrong(0) = (wrong(0) ^ 0x01).toByte
        wrong(wrong.length - 1) = (wrong(wrong.length - 1) ^ 0x01).toByte
        val bytes = Bintel.frameSelfContained(wrong.asInstanceOf[IArray[Byte]], schemaBody, docBody)
        capture[BintelError](Bintel.decodeDocumentSelfContained(bytes)).reason
      . assert(_ == BintelError.Reason.EmbeddedSignatureMismatch)

      test(m"undecodable embedded schema raises B12"):
        val axiom   = Tels.Axiom.tels
        val sd      = schemaDoc.read[Tel]
        val schema  = Tels.Layers.compose(Tels.Reconstructor.fromTel(sd))
        val docBody = dataDoc.read[Tel].bintel(schema)
        val sig     = SchemaSignature.fromDocument(sd, axiom)
        val garbage: Data = IArray[Byte](0x7f, 0x7f, 0x7f, 0x7f)
        val bytes = Bintel.frameSelfContained(sig, garbage, docBody)
        capture[BintelError](Bintel.decodeDocumentSelfContained(bytes)).reason
      . assert(_ == BintelError.Reason.EmbeddedSchemaUndecodable)

    RecordsTests()
