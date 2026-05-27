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

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import panopticon.*
import prepositional.*
import probably.*
import rudiments.*
import turbulence.*
import vacuous.*

import strategies.throwUnsafely
import errorDiagnostics.stackTraces
import charEncoders.utf8
import charDecoders.utf8
import Tel.given

object Tests extends Suite(m"Stratiform Tests"):
  case class Person(name: Text, age: Int) derives CanEqual
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
      // value equal to TelsAxiom.tels) is the phase-3 merge
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
          Tel.Type.assign(doc, TelsAxiom.tels)
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
        val reconstructed = TelsReconstructor.fromTel(doc)
        TelsReconstructor.equivalent(reconstructed, TelsAxiom.tels)
      . assert(identity)

    suite(m"Schema axiom"):
      test(m"tel-schema axiom has the documented name"):
        TelsAxiom.tels.name
      . assert(_ == t"tel-schema")

      test(m"axiom declares the Field record"):
        TelsAxiom.tels.records.exists(_.name == t"Field")
      . assert(identity)

      test(m"axiom declares the four built-in scalars"):
        TelsAxiom.tels.scalars.map(_.name).toSet
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

        val composed = TelsLayers.compose(base)
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
        import TelsDecoder.asValidated
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

        capture[TelError](TelsLayers.compose(base)).reason
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
        CorpusLoader.expectedCode(testcase.stem).let: code =>
          // Phase 1 covers E1xx parsing errors only. E2xx (schema validity)
          // and E3xx (validation) require the schema component shipped in
          // phase 3.
          if code < 200 then
            test(m"raises E$code on ${testcase.stem}"):
              capture[TelError](testcase.source.read[Tel]).reason.number
            . assert(_ == code)
