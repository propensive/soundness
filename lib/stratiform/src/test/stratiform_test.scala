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
import spectacular.*
import turbulence.*
import vacuous.*
import zephyrine.*

import zephyrine.lineation.linefeedByte

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import charEncoders.utf8Encoder
import charDecoders.utf8Decoder
import Tel.given

object Tests extends Suite(m"Stratiform Tests"):
  case class Person(name: Text, age: Int) derives CanEqual
  case class Renamed(@name[Tel](t"full_name") fullName: Text, @name(t"yob") year: Int)
  derives CanEqual
  case class PersonAge(name: Text, age: Int) derives CanEqual
  case class Team(name: Text, members: List[Person]) derives CanEqual

  enum Shape2 derives CanEqual:
    case Circle(radius: Int)
    case Rectangle(width: Int, height: Int)
    case Dot


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
          val printed = first.show
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
          val printed = Tel.make(first).show
          val bytes: Data = summon[CharEncoder].encoded(printed)
          val reparsed = TelParser.parse(Cursor[Data](bytes))
          TelCheckTree.of(Tel.make(reparsed)) == TelCheckTree.of(Tel.make(first))
        . assert(identity)

    suite(m"Streaming parser — negative corpus (E1xx)"):
      CorpusLoader.negative.each: testcase =>
        val codes = CorpusLoader.expectedCodes(testcase)
        if codes.nonEmpty && codes.forall(_ < 200) then
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
          sizes.forall: n =>
            val tree = TelCheckTree.of(Tel.make(
              TelParser.parse(chunkedCursor(testcase.source, n))))
            tree == baseline
        . assert(identity)

    suite(m"Document streams (§6.1)"):
      // Each `.check` fixture holds a `=== document N ===` sequence; the parsed
      // documents must match it document-for-document. Fixtures whose `.check`
      // carries an `errors:` section exercise per-document error *recovery* in
      // streaming mode (the reference still yields a recovered document and
      // reports its errors). Stratiform's `Stream[Tel]`/`List[Tel]` model is
      // deliberately fail-fast (see "a malformed document in a stream raises"
      // below) and carries no per-document error list, so those fixtures are
      // skipped pending the in-progress upstream error-isolation work.
      CorpusLoader.streaming.filterNot(_.check.s.contains("errors:")).each: testcase =>
        test(m"read[List[Tel]] parses ${testcase.stem}"):
          testcase.source.read[List[Tel]].map(TelCheckTree.of)
        . assert(_ == CheckFormat.parseStream(testcase.check).map(_.tree))

        // `LazyList[Tel]` is proscenium's `Stream[Tel]`; this file doesn't
        // import the predef alias, so spell it out.
        test(m"read[Stream[Tel]] parses ${testcase.stem}"):
          testcase.source.read[LazyList[Tel]].map(TelCheckTree.of).to(List)
        . assert(_ == CheckFormat.parseStream(testcase.check).map(_.tree))

      test(m"two documents yield a list of two"):
        CorpusLoader.caseByStem(t"stream", t"two-documents").source.read[List[Tel]].length
      . assert(_ == 2)

      test(m"a trailing separator yields no empty trailing document"):
        CorpusLoader.caseByStem(t"stream", t"trailing-separator").source.read[List[Tel]].length
      . assert(_ == 1)

      test(m"two consecutive separators yield an empty document between them"):
        CorpusLoader.caseByStem(t"stream", t"empty-between").source.read[List[Tel]].length
      . assert(_ == 3)

      test(m"a malformed document in a stream raises (fail-fast)"):
        // The second document has an odd indentation (E107); reading the whole
        // list eagerly surfaces it.
        capture[TelError](t"a 1\n##\nparent\n   bad".read[List[Tel]]).reason.number
      . assert(_ == 107)

      test(m"read[Stream[Tel]] is lazy past a malformed later document"):
        val source = t"first ok\n##\nparent\n   bad"
        TelCheckTree.of(source.read[LazyList[Tel]].head)
      . assert(_ == TelCheckTree.of(t"first ok".read[Tel]))

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

      test(m"a List field encodes as one repeated compound per element"):
        val team = Tests.Team(t"Reds", List(Tests.Person(t"Alice", 30), Tests.Person(t"Bob", 25)))
        team.encode.childCompounds.filter(_.keyword == t"members").length
      . assert(_ == 2)

      test(m"an empty List field encodes as no compounds"):
        Tests.Team(t"Reds", Nil).encode.childCompounds.filter(_.keyword == t"members").length
      . assert(_ == 0)

      test(m"a sum encodes its variant as a child keyed by the variant name"):
        val shape: Tests.Shape2 = Tests.Shape2.Circle(7)
        shape.encode.childCompounds.head.keyword
      . assert(_ == t"circle")

      test(m"a single-field sum variant round-trips"):
        val shape: Tests.Shape2 = Tests.Shape2.Circle(7)
        shape.encode.as[Tests.Shape2]
      . assert(_ == Tests.Shape2.Circle(7))

      test(m"a multi-field sum variant round-trips"):
        val shape: Tests.Shape2 = Tests.Shape2.Rectangle(3, 4)
        shape.encode.as[Tests.Shape2]
      . assert(_ == Tests.Shape2.Rectangle(3, 4))

      test(m"a fieldless sum variant round-trips"):
        val shape: Tests.Shape2 = Tests.Shape2.Dot
        shape.encode.as[Tests.Shape2]
      . assert(_ == Tests.Shape2.Dot)

    suite(m"sum-type schema derivation"):
      test(m"a sum derives a select with one variant per case"):
        Tels.tels[Tests.Shape2](t"shape").selects.flatMap(_.variants).map(_.keyword).to(List)
      . assert(_ == List(t"circle", t"rectangle", t"dot"))

      test(m"each variant's fields are derived into its struct"):
        val select = Tels.tels[Tests.Shape2](t"shape").selects.head
        select.variants.find(_.keyword == t"rectangle").get.variantType match
          case struct: Tels.Struct => struct.members.length
          case _                   => -1
      . assert(_ == 2)

      test(m"the document root references the select"):
        Tels.tels[Tests.Shape2](t"shape").document.members.map:
          case ref: Tels.SelectRef => ref.reference
          case _                   => t""
        . to(List)
      . assert(_ == List(t"Shape2"))

    suite(m"`in Tel` decoder shorthand"):
      test(m"`read[T in Tel]` resolves a value directly from text"):
        t"name Alice\nage 30\n".read[Tests.Person in Tel]
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
          IArray.from(arr)

        bytes.read[Tel].childCompounds.length
      . assert(_ > 0)

      test(m"canonical tel-schema.tel type-assigns against the axiom"):
        // §20.5 self-consistency: the canonical document must type-assign
        // cleanly under the hand-encoded axiom.
        val stream = getClass.getResourceAsStream("/stratiform/corpus/tel-schema.tel").nn
        val bytes  =
          val arr = stream.readAllBytes().nn
          stream.close()
          IArray.from(arr)

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
          IArray.from(arr)

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
        Tels.Axiom.tels.scalars.map(_.name).toSet
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
        tel.document.vouch.show
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
            .toList

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

    suite(m"Schema default-field"):
      // Like `personSchema`, but the required `name` field carries a default,
      // so a document omitting it is filled with the default rather than
      // raising `RequiredMemberAbsent`.
      val defaultingSchema = Tels(
        name     = t"person",
        document = Tels.Struct(
          members = IArray(
            Tels.Field
             ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
               t"name", Tels.Scalar(IArray(t"string")), t"Anonymous" ),
            Tels.Field
             ( Tels.Polarity.Loose, Tels.Polarity.Implicit,
               t"age", Tels.Scalar(IArray(t"identifier")), Unset )),
          validators = IArray.empty),
        layers   = IArray.empty,
        sigil    = Unset,
        records  = IArray.empty,
        scalars  = IArray.empty,
        selects  = IArray.empty)

      test(m"applies the schema default when the field is omitted"):
        val doc = t"age 30\n".read[Tel]
        Tel.Type.assign(doc, defaultingSchema) match
          case Tel.Element.Node(_, _, children) =>
            children.collect:
              case Tel.Element.Value(_, _, t) => t
            .to(Set)

          case _ => Set()
      . assert(_ == Set(t"Anonymous", t"30"))

      test(m"an explicit value overrides the schema default"):
        val doc = t"name Alice\nage 30\n".read[Tel]
        Tel.Type.assign(doc, defaultingSchema) match
          case Tel.Element.Node(_, _, children) =>
            children.collect:
              case Tel.Element.Value(_, _, t) => t
            .toList

          case _ => Nil
      . assert(_ == List(t"Alice", t"30"))

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
        result.document.vouch.show
      . assert(_ == t"name Bob\n")

      test(m"AttachRemark adds a remark to the targeted compound"):
        val tel    = doc("name Alice\n")
        val ptr    = Tel.Pointer.of(t"name")
        val result = Mutation(tel, Mutation.Op.AttachRemark(ptr, t"primary contact"))
        result.document.vouch.show
      . assert(_ == t"name Alice  # primary contact\n")

      test(m"RemoveRemark drops a previously attached remark"):
        val tel    = doc("name Alice  # noted\n")
        val ptr    = Tel.Pointer.of(t"name")
        val result = Mutation(tel, Mutation.Op.RemoveRemark(ptr))
        result.document.vouch.show
      . assert(_ == t"name Alice\n")

      test(m"Insert appends a child compound to the parent"):
        val tel    = doc("contact\n  name Alice\n")
        val newCompound = Tel.Compound
                          (t"email",
                           IArray(Tel.Atom.Inline(t"alice@example.com", 1)),
                           Unset, IArray.empty)
        val ptr    = Tel.Pointer.of(t"contact")
        val result = Mutation(tel, Mutation.Op.Insert(ptr, newCompound))
        result.document.vouch.show
      . assert(_ == t"contact\n  name Alice\n  email alice@example.com\n")

      test(m"Delete removes the addressed compound"):
        val tel    = doc("name Alice\nemail alice@example.com\n")
        val ptr    = Tel.Pointer.of(t"email")
        val result = Mutation(tel, Mutation.Op.Delete(ptr))
        result.document.vouch.show
      . assert(_ == t"name Alice\n")

      test(m"InsertBefore places a new sibling before the target"):
        val tel    = doc("b two\n")
        val a      = Tel.Compound
                      (t"a", IArray(Tel.Atom.Inline(t"one", 1)), Unset, IArray.empty)
        val ptr    = Tel.Pointer.of(t"b")
        val result = Mutation(tel, Mutation.Op.InsertBefore(ptr, a))
        result.document.vouch.show
      . assert(_ == t"a one\nb two\n")

      test(m"InsertAfter places a new sibling after the target"):
        val tel    = doc("a one\n")
        val b      = Tel.Compound
                      (t"b", IArray(Tel.Atom.Inline(t"two", 1)), Unset, IArray.empty)
        val ptr    = Tel.Pointer.of(t"a")
        val result = Mutation(tel, Mutation.Op.InsertAfter(ptr, b))
        result.document.vouch.show
      . assert(_ == t"a one\nb two\n")

      test(m"Replace swaps a compound for a new one"):
        val tel    = doc("name Alice\n")
        val replacement = Tel.Compound
                           (t"name", IArray(Tel.Atom.Inline(t"Charlie", 1)),
                            Unset, IArray.empty)
        val ptr    = Tel.Pointer.of(t"name")
        val result = Mutation(tel, Mutation.Op.Replace(ptr, replacement))
        result.document.vouch.show
      . assert(_ == t"name Charlie\n")

      test(m"SetFlag attaches a flag-typed child compound"):
        val tel    = doc("opt\n")
        val ptr    = Tel.Pointer.of(t"opt")
        val result = Mutation(tel, Mutation.Op.SetFlag(ptr, t"enabled"))
        result.document.vouch.show
      . assert(_ == t"opt\n  enabled\n")

      test(m"UnsetFlag removes a previously set flag"):
        val tel    = doc("opt\n  enabled\n")
        val ptr    = Tel.Pointer.of(t"opt")
        val result = Mutation(tel, Mutation.Op.UnsetFlag(ptr, t"enabled"))
        result.document.vouch.show
      . assert(_ == t"opt\n")

      test(m"sequenced ops apply in order"):
        val tel    = doc("name Alice\n")
        val ptr    = Tel.Pointer.of(t"name")
        val ops    = Seq
                      ( Mutation.Op.UpdateAtom(ptr, 0, t"Bob"),
                        Mutation.Op.AttachRemark(ptr, t"note") )
        val result = Mutation(tel, ops)
        result.document.vouch.show
      . assert(_ == t"name Bob  # note\n")

      test(m"pointer with no match raises PointerNotFound"):
        val tel = doc("name Alice\n")
        val ptr = Tel.Pointer.of(t"missing")
        capture[MutationError](Mutation(tel, Mutation.Op.Delete(ptr))).reason
      . assert(_ == MutationError.Reason.PointerNotFound)

      test(m"ReorderWithinGroup moves a same-keyword sibling"):
        val tel = doc("item a\nitem b\nitem c\n")
        val op  = Mutation.Op.ReorderWithinGroup(Tel.Pointer.Empty, t"item", 0, 2)
        Mutation(tel, op).document.vouch.show
      . assert(_ == t"item b\nitem c\nitem a\n")

      test(m"ReorderWithinGroup with same old and new is a no-op"):
        val tel = doc("item a\nitem b\n")
        val op  = Mutation.Op.ReorderWithinGroup(Tel.Pointer.Empty, t"item", 1, 1)
        Mutation(tel, op).document.vouch.show
      . assert(_ == t"item a\nitem b\n")

      test(m"ReorderWithinGroup with out-of-range index raises"):
        val tel = doc("item a\nitem b\n")
        val op  = Mutation.Op.ReorderWithinGroup(Tel.Pointer.Empty, t"item", 0, 5)
        capture[MutationError](Mutation(tel, op)).reason
      . assert(_ == MutationError.Reason.PointerNotFound)

      test(m"ReorderGroups swaps contiguous member groups"):
        val tel = doc("name Alice\nname Bob\nage 30\nage 31\n")
        val op  = Mutation.Op.ReorderGroups(Tel.Pointer.Empty, t"name", t"age")
        Mutation(tel, op).document.vouch.show
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
        val c = Mutation.construct(t"note", t"first line\nsecond line")
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

      test(m"Construct escalates a trailing-LF value to a literal atom (§22.2)"):
        // A trailing LF is unrepresentable as a source atom (Convention A,
        // §14), so the value must be carried by a literal atom.
        val c = Mutation.construct(t"note", t"single line\n")
        c.atoms.head match
          case _: Tel.Atom.Literal => "literal"
          case _: Tel.Atom.Source  => "source"
          case _: Tel.Atom.Inline  => "inline"
      . assert(_ == "literal")

      test(m"Construct's inline atom uses two preceding spaces for a spaced value"):
        // §22.3: a value containing a space is emitted with a hard-space
        // separator so its soft spaces survive as content (§10.3).
        val c = Mutation.construct(t"name", t"Jon Pretty")
        c.atoms.head match
          case Tel.Atom.Inline(_, sp) => sp
          case _                       => -1
      . assert(_ == 2)

      test(m"Construct keeps an internal space-then-sigil value inline (§22.2)"):
        // The remark risk (§11.2) is only a *leading* sigil-then-space; an
        // internal `<space><sigil>` is content in hard-space mode.
        val c = Mutation.construct(t"note", t"see #3")
        c.atoms.head match
          case _: Tel.Atom.Inline  => "inline"
          case _: Tel.Atom.Source  => "source"
          case _: Tel.Atom.Literal => "literal"
      . assert(_ == "inline")

      test(m"Construct escalates a leading sigil-then-space value off inline (§22.2)"):
        val c = Mutation.construct(t"note", t"# heading")
        c.atoms.head match
          case _: Tel.Atom.Inline  => "inline"
          case _                    => "escalated"
      . assert(_ == "escalated")

      test(m"Construct emits no atom for an empty value (§22.3)"):
        Mutation.construct(t"flag", t"").atoms.length
      . assert(_ == 0)

      test(m"UpdateAtom escalates an inline atom to a literal for a trailing-LF value"):
        val tel    = doc("note text\n")
        val ptr    = Tel.Pointer.of(t"note")
        val result = Mutation(tel, Mutation.Op.UpdateAtom(ptr, 0, t"line\n"))
        result.childCompounds.head.atoms.head match
          case _: Tel.Atom.Literal => "literal"
          case _: Tel.Atom.Source  => "source"
          case _: Tel.Atom.Inline  => "inline"
      . assert(_ == "literal")

      test(m"UpdateAtom escalates an inline atom to a source atom for a multi-line value"):
        val tel    = doc("note text\n")
        val ptr    = Tel.Pointer.of(t"note")
        val result = Mutation(tel, Mutation.Op.UpdateAtom(ptr, 0, t"line one\nline two"))
        result.childCompounds.head.atoms.head match
          case _: Tel.Atom.Source  => "source"
          case _: Tel.Atom.Inline  => "inline"
          case _: Tel.Atom.Literal => "literal"
      . assert(_ == "source")

      test(m"UpdateAtom never downgrades a literal atom to inline (§22.3)"):
        // A literal atom updated to an inline-safe value stays literal.
        val tel    = doc("note\n      ===\nnow literal\n===\n")
        val ptr    = Tel.Pointer.of(t"note")
        val result = Mutation(tel, Mutation.Op.UpdateAtom(ptr, 0, t"now simple"))
        result.childCompounds.head.atoms.head match
          case _: Tel.Atom.Literal => "literal"
          case _: Tel.Atom.Source  => "source"
          case _: Tel.Atom.Inline  => "inline"
      . assert(_ == "literal")

    suite(m"Tel.fields repeated-keyword accessor"):
      test(m"fields returns all matching children in order"):
        val tel = t"item 1\nitem 2\nitem 3\n".read[Tel]
        tel.fields(t"item").map(_.primaryAtom).toList
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
        updated.document.vouch.show
      . assert(_ == t"# header\nname Alice\nemail b@example.com\n")

      test(m"a multi-step Revision log round-trips through the printer"):
        val original = doc("name Alice\n")
        val edited =
          original.edited
            ( Revision.at(Tel.Pointer.of(t"name")).update(t"Bob")
           ++ Revision.at(Tel.Pointer.Empty)
                  .insert(Revision.compound(t"email", t"b@example.com")) )

        val printed   = edited.document.vouch.show
        val reparsed  = printed.s.tt.read[Tel]
        reparsed.document.vouch.show
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

    suite(m"Optics: positional child traversal"):
      import dynamicTelAccess.enabled
      def doc(source: String): Tel = source.tt.read[Tel]
      def contacts: Tel = doc("contacts\n  contact alice\n  contact bob\n")

      test(m"ordinal optic replaces the n-th child compound"):
        contacts.lens(_.contacts(Sec) = Tel.scalar(t"carol")).contacts(1).primaryAtom
      . assert(_ == t"carol")

      test(m"ordinal optic leaves siblings unchanged"):
        contacts.lens(_.contacts(Sec) = Tel.scalar(t"carol")).contacts(0).primaryAtom
      . assert(_ == t"alice")

      test(m"ordinal optic preserves the child's keyword"):
        contacts.lens(_.contacts(Sec) = Tel.scalar(t"carol")).applyDynamic("contacts")(1).keyword
      . assert(_ == t"contact")

      test(m"each optic transforms every child compound"):
        val updated = contacts.lens(_.contacts(Each) = Tel.scalar(t"x"))
        (updated.contacts(0).primaryAtom, updated.contacts(1).primaryAtom)
      . assert(_ == (t"x", t"x"))

      test(m"an out-of-range ordinal is a no-op"):
        contacts.lens(_.contacts(Quat) = Tel.scalar(t"none")).contacts(1).primaryAtom
      . assert(_ == t"bob")

      test(m"editing through an ordinal optic preserves surrounding formatting"):
        val original = doc("# header\ncontacts\n  contact alice\n  contact bob\n")
        val updated = original.lens(_.contacts(Sec) = Tel.scalar(t"carol"))
        updated.document.vouch.show
      . assert(_ == t"# header\ncontacts\n  contact alice\n  contact carol\n")

    suite(m"Revision DSL"):
      def doc(source: String): Tel = source.tt.read[Tel]

      test(m"single-op edit changes one atom"):
        val tel  = doc("name Alice\n")
        val edit = Revision.at(Tel.Pointer.of(t"name")).update(t"Bob")
        tel.edited(edit).document.vouch.show
      . assert(_ == t"name Bob\n")

      test(m"chained edits apply in order"):
        val tel = doc("name Alice\n")
        val edit = Revision.at(Tel.Pointer.of(t"name")).update(t"Bob")
                ++ Revision.at(Tel.Pointer.of(t"name")).attachRemark(t"note")

        tel.edited(edit).document.vouch.show
      . assert(_ == t"name Bob  # note\n")

      test(m"Revision.compound helper builds an inline-atom compound"):
        val c = Revision.compound(t"email", t"a@b.c")
        c.keyword
      . assert(_ == t"email")

      test(m"inserting via Revision composes with deletion"):
        val tel  = doc("a 1\nb 2\n")
        val edit = Revision.at(Tel.Pointer.of(t"b")).delete
                ++ Revision.at(Tel.Pointer.of(t"a")).insertAfter(Revision.compound(t"c", t"3"))

        tel.edited(edit).document.vouch.show
      . assert(_ == t"a 1\nc 3\n")

      test(m"noop edit returns the document unchanged"):
        val tel = doc("name Alice\n")
        tel.edited(Revision.noop).document.vouch.show
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
        if codes.nonEmpty && codes.forall(_ < 200) then
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
        Base256.alphabet.toSet.size
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
        Base256.decode(Base256.encode(data)).toSeq
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
        Base256.decode(t"A ").toSeq
      . assert(_ == Seq(0x41.toByte, 0x20.toByte))

      test(m"strict decode accepts the alphabet"):
        val data: Data = (0 to 255).map(_.toByte).toArray.asInstanceOf[IArray[Byte]]
        Base256.decodeStrict(Base256.encode(data)).toSeq
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

      vectors.foreach: (value, expected) =>
        test(m"encodes $value as $expected"):
          hex(Varint.encode(value))
        . assert(_ == expected)

        test(m"decodes $expected back to $value"):
          val parts = expected.split(" ").map(java.lang.Integer.parseInt(_, 16).toByte)
          Varint.decode(parts.asInstanceOf[IArray[Byte]], 0).value
        . assert(_ == value)

      test(m"round-trips every value in 0..1023"):
        (0L to 1023L).forall: n =>
          Varint.decode(Varint.encode(n), 0).value == n
      . assert(identity)

      test(m"round-trips powers of two up to 2^62"):
        (0 to 62).map(_.toLong).forall: i =>
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
      arr.toSeq

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
            children.toList.collect:
              case Tel.Element.Value(_, _, t) => t
          case _ => Nil
      . assert(_ == List(t"Alice"))

      test(m"a value derives its own schema and round-trips through BinTEL"):
        val shape: Tests.Shape2 = Tests.Shape2.Rectangle(3, 4)
        val schema = Tels.tels[Tests.Shape2](t"shape")

        def values(element: Tel.Element): List[Text] = element match
          case Tel.Element.Node(_, _, children) => children.to(List).flatMap(values)
          case Tel.Element.Value(_, _, text)    => List(text)

        values(Bintel.decode(shape.bintel, schema))
      . assert(_ == List(t"3", t"4"))

      test(m"a sum round-trips bytes-to-typed-value through bintel/read"):
        val shape: Tests.Shape2 = Tests.Shape2.Rectangle(3, 4)
        Bintel.read[Tests.Shape2](shape.bintel)
      . assert(_ == Tests.Shape2.Rectangle(3, 4))

      test(m"a fieldless variant round-trips bytes-to-typed-value"):
        val shape: Tests.Shape2 = Tests.Shape2.Dot
        Bintel.read[Tests.Shape2](shape.bintel)
      . assert(_ == Tests.Shape2.Dot)

      test(m"a product round-trips bytes-to-typed-value through bintel/read"):
        Bintel.read[Tests.Person](Tests.Person(t"Alice", 30).bintel)
      . assert(_ == Tests.Person(t"Alice", 30))

      test(m"empty scalar value round-trips"):
        val scalar = Tels.Scalar(IArray.empty)
        val root = Tel.Element.Node
                    (Unset, nameSchema.document, IArray(Tel.Element.Value(0, scalar, t"")))
        val bytes = root.bintel(nameSchema)
        val decoded = Bintel.decode(bytes, nameSchema)
        decoded match
          case Tel.Element.Node(_, _, children) =>
            children.toList.collect:
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
            children.toList.collect:
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
            inner.toList.collect:
              case Tel.Element.Value(_, _, t) => t
          case _ => Nil
      . assert(_ == List(t"example.com"))

      test(m"trailing bytes after document root raise BintelError"):
        val original = t"name Alice\n".read[Tel]
        val bytes = original.bintel(nameSchema)
        val padded = (bytes.toList :+ 0xff.toByte).toArray.asInstanceOf[IArray[Byte]]
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
        framed.slice(0, 5).toSeq
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
        (sig.toSeq, recovered.toSeq)
      . assert(_ == (sig32.toSeq, Seq[Byte](0x01, 0x02, 0x03)))

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
        val recovered = Bintel.unframe(framed).body.toSeq
        recovered == original.toSeq
      . assert(_ == true)

      test(m"tel.bintelDocument produces a file beginning with magic"):
        val bytes = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        bytes.slice(0, 4).toSeq
      . assert(_ == Seq(0xB2.toByte, 0xC4.toByte, 0xB5.toByte, 0xBB.toByte))

      test(m"decodeDocument round-trips through frame + decode"):
        val bytes = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        val doc = Bintel.decodeDocument(bytes, nameSchema)
        doc.root match
          case Tel.Element.Node(_, _, children) =>
            children.toList.collect:
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
        recovered.toSeq == source.toSeq
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
        SchemaSignature.encode(List(h0)).slice(0, 32).toSeq == h0.toSeq
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
        val recovered = SchemaSignature.decode(sig, List(h0))
        recovered.map(_.toSeq) == List(h0.toSeq)
      . assert(_ == true)

      test(m"two-component signature round-trips through encode/decode"):
        val sig = SchemaSignature.encode(List(h0, h1))
        val recovered = SchemaSignature.decode(sig, List(h0, h1, h2))
        recovered.map(_.toSeq) == List(h0.toSeq, h1.toSeq)
      . assert(_ == true)

      test(m"three-component signature round-trips through encode/decode"):
        val sig = SchemaSignature.encode(List(h0, h1, h2))
        val recovered = SchemaSignature.decode(sig, List(h0, h1, h2))
        recovered.map(_.toSeq) == List(h0.toSeq, h1.toSeq, h2.toSeq)
      . assert(_ == true)

      test(m"decode with reserved hash-size index raises BadSignatureLength"):
        // XOR-fold ⇒ 0xA0, naming reserved s = 10
        val bad: Data = Array[Byte](0xA0.toByte, 0, 0, 0, 0).asInstanceOf[IArray[Byte]]
        capture[BintelError](SchemaSignature.decode(bad, List(h0))).reason
      . assert(_ == BintelError.Reason.BadSignatureLength)

      test(m"decode raises BadSignature when library is missing components"):
        val sig = SchemaSignature.encode(List(h0, h1))
        capture[BintelError](SchemaSignature.decode(sig, List(h2))).reason
      . assert(_ == BintelError.Reason.BadSignature)

    suite(m"BinTEL §8.1 schema signature from document"):
      test(m"single-component signature for a no-layer schema is 33 bytes"):
        val stream = getClass.getResourceAsStream("/stratiform/corpus/tel-schema.tel").nn
        val source =
          val arr = stream.readAllBytes().nn
          stream.close()
          IArray.from(arr)

        val sig = SchemaSignature.fromDocument(source.read[Tel], Tels.Axiom.tels)
        sig.length
      . assert(_ == 33)

      test(m"no-layer schema signature begins with the 32-byte BLAKE3 value hash"):
        val stream = getClass.getResourceAsStream("/stratiform/corpus/tel-schema.tel").nn
        val source =
          val arr = stream.readAllBytes().nn
          stream.close()
          IArray.from(arr)

        val sig = SchemaSignature.fromDocument(source.read[Tel], Tels.Axiom.tels)
        val bintel = Tel.Type.assign(source.read[Tel], Tels.Axiom.tels).bintel(Tels.Axiom.tels)
        val hash = Blake3.hashOf(bintel, 32)
        sig.slice(0, 32).toSeq == hash.toSeq
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
        val a = tel.valueHash(nameSchema).data.toSeq
        val b = tel.valueHash(nameSchema).data.toSeq
        a == b
      . assert(_ == true)

      test(m"valueHash differs when value differs"):
        val a = t"name Alice\n".read[Tel].valueHash(nameSchema).data.toSeq
        val b = t"name Bob\n".read[Tel].valueHash(nameSchema).data.toSeq
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
          IArray.from(arr)

        val a = Tel.Type.assign(source.read[Tel], Tels.Axiom.tels).valueHash(Tels.Axiom.tels).data.toSeq
        val b = Tel.Type.assign(source.read[Tel], Tels.Axiom.tels).valueHash(Tels.Axiom.tels).data.toSeq
        (a.length, a == b)
      . assert(_ == (32, true))

      test(m"§3 — canonical tel-schema.tel encodes byte-for-byte against reference"):
        val telStream = getClass.getResourceAsStream("/stratiform/corpus/tel-schema.tel").nn
        val telBytes  =
          val arr = telStream.readAllBytes().nn
          telStream.close()
          IArray.from(arr)

        val refStream = getClass.getResourceAsStream("/stratiform/corpus/tel-schema.bintel.hex").nn
        val refHex    =
          val arr = refStream.readAllBytes().nn
          refStream.close()
          String(arr, "UTF-8").trim

        val refBytes = hexBytes(refHex)
        val element  = Tel.Type.assign(telBytes.read[Tel], Tels.Axiom.tels)
        element.bintel(Tels.Axiom.tels).toSeq == refBytes
      . assert(_ == true)

      test(m"§3 — tel-schema.tel matches the normative BLAKE3-256 value hash"):
        // The single vector to which §3 of BinTEL and §20.5 of the TEL
        // Specification are both pinned.
        val telStream = getClass.getResourceAsStream("/stratiform/corpus/tel-schema.tel").nn
        val telBytes  =
          val arr = telStream.readAllBytes().nn
          telStream.close()
          IArray.from(arr)

        val digest = Tel.Type.assign(telBytes.read[Tel], Tels.Axiom.tels).valueHash(Tels.Axiom.tels)
        digest.data.toSeq.map(b => f"${b & 0xff}%02x").mkString
      . assert(_ == "d4289b0fc6b7f666c9269a135d509ff3973bcea734fbe777b8f907045d3df8a9")

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
        selfContained().slice(0, 4).toSeq
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
        recovered.toSeq == external.toSeq
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
    VerifyTests()
