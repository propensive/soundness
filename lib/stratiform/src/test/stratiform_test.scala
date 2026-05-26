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
import panopticon.*
import prepositional.*
import probably.*
import rudiments.*
import vacuous.*

import strategies.throwUnsafely
import errorDiagnostics.stackTraces
import Tel.given

object Tests extends Suite(m"Stratiform Tests"):
  case class Person(name: Text, age: Int) derives CanEqual
  case class PersonAge(name: Text, age: Int) derives CanEqual


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

    suite(m"tel-schema self-consistency"):
      // Phase-3 partial: parse the canonical tel-schema.tel and verify
      // it produces a valid presentation AST. Full self-consistency
      // (type-assign against the axiom and reconstruct a TelSchema
      // value equal to TelSchemaAxiom.telSchema) is the phase-3 merge
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

        Tel.parse(bytes).childCompounds.length
      . assert(_ > 0)

      test(m"canonical tel-schema.tel type-assigns against the axiom"):
        // §20.5 self-consistency: the canonical document must type-assign
        // cleanly under the hand-encoded axiom.
        val stream = getClass.getResourceAsStream("/stratiform/corpus/tel-schema.tel").nn
        val bytes  =
          val arr = stream.readAllBytes().nn
          stream.close()
          IArray.from(arr)

        val doc = Tel.parseDocument(bytes)
        try
          TelTypeAssignment.assign(doc, TelSchemaAxiom.telSchema)
          "ok"
        catch case e: TelError => s"failed-with-${e.reason}"
      . assert(_ == "ok")

      test(m"canonical tel-schema.tel reconstructs structurally equal to the axiom"):
        // The strongest §20.5 property: reconstruct a TelSchema from the
        // canonical document and assert it is structurally identical to
        // the hand-encoded axiom.
        val stream = getClass.getResourceAsStream("/stratiform/corpus/tel-schema.tel").nn
        val bytes  =
          val arr = stream.readAllBytes().nn
          stream.close()
          IArray.from(arr)

        val doc = Tel.parseDocument(bytes)
        val reconstructed = TelSchemaReconstructor.fromDocument(doc)
        TelSchemaReconstructor.equivalent(reconstructed, TelSchemaAxiom.telSchema)
      . assert(identity)

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

      // A schema with a Status SelectRef whose variants are all Flag,
      // exercising sum-type handling.
      val statusSchema = TelSchema(
        name     = Text("status"),
        document = TelSchema.Struct(
          members = IArray(TelSchema.SelectRef
           ( required   = TelSchema.Polarity.Implicit,
             repeatable = TelSchema.Polarity.Implicit,
             reference  = Text("Status") )),
          validators = IArray.empty),
        layers   = IArray.empty,
        sigil    = Unset,
        records  = IArray.empty,
        scalars  = IArray.empty,
        selects  = IArray(TelSchema.SelectDefinition(
          name     = Text("Status"),
          variants = IArray(
            TelSchema.Variant(Text("active"),   TelSchema.Flag),
            TelSchema.Variant(Text("archived"), TelSchema.Flag)),
          validators = IArray.empty)))

      test(m"SelectRef variant matches compound child"):
        val doc = Tel.parseDocument(IArray.from("active\n".getBytes("UTF-8")))
        val root = TelTypeAssignment.assign(doc, statusSchema)
        root match
          case TelElement.Node(_, _, children) => children.length
          case _                               => -1
      . assert(_ == 1)

      test(m"unknown SelectRef variant raises E306"):
        val doc = Tel.parseDocument(IArray.from("unknown\n".getBytes("UTF-8")))
        capture[TelError](TelTypeAssignment.assign(doc, statusSchema)).reason
      . assert(_ == TelError.Reason.UnknownKeyword)

    suite(m"Validators"):
      val reg = TelValidator.Registry.builtins

      test(m"string validator accepts any text"):
        reg(TelValidator.Request.Scalar(Text("string"), Text("anything")))
      . assert(_ == TelValidator.Response.Valid)

      test(m"identifier accepts kebab-case"):
        reg(TelValidator.Request.Scalar(Text("identifier"), Text("first-name")))
      . assert(_ == TelValidator.Response.Valid)

      test(m"identifier rejects leading hyphen"):
        reg(TelValidator.Request.Scalar(Text("identifier"), Text("-leading"))) match
          case TelValidator.Response.Invalid(_) => true
          case _                                => false
      . assert(identity)

      test(m"type-name accepts PascalCase"):
        reg(TelValidator.Request.Scalar(Text("type-name"), Text("PhoneNumber")))
      . assert(_ == TelValidator.Response.Valid)

      test(m"type-name rejects leading lowercase"):
        reg(TelValidator.Request.Scalar(Text("type-name"), Text("phoneNumber"))) match
          case TelValidator.Response.Invalid(_) => true
          case _                                => false
      . assert(identity)

      test(m"sigil accepts a permitted symbol"):
        reg(TelValidator.Request.Scalar(Text("sigil"), Text("#")))
      . assert(_ == TelValidator.Response.Valid)

      test(m"sigil rejects letters"):
        reg(TelValidator.Request.Scalar(Text("sigil"), Text("a"))) match
          case TelValidator.Response.Invalid(_) => true
          case _                                => false
      . assert(identity)

      test(m"type assignment with identifier validator rejects bad identifier"):
        val schemaWithValidator = TelSchema(
          name     = Text("ident"),
          document = TelSchema.Struct(
            members = IArray(TelSchema.Field
             ( TelSchema.Polarity.Implicit, TelSchema.Polarity.Implicit, Text("name"),
               TelSchema.Scalar(IArray(Text("identifier"))), Unset )),
            validators = IArray.empty),
          layers  = IArray.empty,
          sigil   = Unset,
          records = IArray.empty,
          scalars = IArray.empty,
          selects = IArray.empty)

        val doc = Tel.parseDocument(IArray.from("name -bad\n".getBytes("UTF-8")))
        capture[TelError]:
          TelTypeAssignment.assign(doc, schemaWithValidator, TelValidator.Registry.builtins)
        .reason
      . assert(_ == TelError.Reason.ValidatorRejected)

    suite(m"Layer composition"):
      test(m"a layer adding a field extends the document Struct"):
        val base = TelSchema(
          name     = Text("base"),
          document = TelSchema.Struct(
            members = IArray(TelSchema.Field
             ( TelSchema.Polarity.Implicit, TelSchema.Polarity.Implicit, Text("name"),
               TelSchema.Scalar(IArray(Text("string"))), Unset )),
            validators = IArray.empty),
          layers = IArray(TelSchema.Layer(
            name     = Text("extra"),
            overlay  = TelSchema.Struct(
              members = IArray(TelSchema.Field
               ( TelSchema.Polarity.Loose, TelSchema.Polarity.Implicit, Text("email"),
                 TelSchema.Scalar(IArray(Text("string"))), Unset )),
              validators = IArray.empty),
            records = IArray.empty, scalars = IArray.empty, selects = IArray.empty)),
          sigil    = Unset,
          records  = IArray.empty,
          scalars  = IArray.empty,
          selects  = IArray.empty)

        val composed = TelSchemaLayers.compose(base)
        composed.document.members.length
      . assert(_ == 2)

      test(m"plain as[Person] decodes a conforming document"):
        val tel = Tel.parse(IArray.from("name Alice\nage 30\n".getBytes("UTF-8")))
        tel.as[Tests.PersonAge]
      . assert(_ == Tests.PersonAge(Text("Alice"), 30))

      test(m"asValidated validates and decodes a conforming document"):
        val schema = TelSchema(
          name     = Text("person"),
          document = TelSchema.Struct(
            members = IArray(
              TelSchema.Field
               ( TelSchema.Polarity.Implicit, TelSchema.Polarity.Implicit, Text("name"),
                 TelSchema.Scalar(IArray(Text("string"))), Unset ),
              TelSchema.Field
               ( TelSchema.Polarity.Implicit, TelSchema.Polarity.Implicit, Text("age"),
                 TelSchema.Scalar(IArray(Text("string"))), Unset )),
            validators = IArray.empty),
          layers  = IArray.empty,
          sigil   = Unset,
          records = IArray.empty,
          scalars = IArray.empty,
          selects = IArray.empty)

        given TelSchema = schema
        import TelSchemaDecoder.asValidated
        val tel = Tel.parse(IArray.from("name Alice\nage 30\n".getBytes("UTF-8")))
        tel.asValidated[Tests.PersonAge]
      . assert(_ == Tests.PersonAge(Text("Alice"), 30))

      test(m"duplicate layer name raises E205"):
        val layer = TelSchema.Layer
         ( name    = Text("dup"),
           overlay = TelSchema.Struct(IArray.empty, IArray.empty),
           records = IArray.empty, scalars = IArray.empty, selects = IArray.empty )

        val base = TelSchema(
          name = Text("base"),
          document = TelSchema.Struct(IArray.empty, IArray.empty),
          layers = IArray(layer, layer),
          sigil = Unset,
          records = IArray.empty, scalars = IArray.empty, selects = IArray.empty)

        capture[TelError](TelSchemaLayers.compose(base)).reason
      . assert(_ == TelError.Reason.DuplicateLayerName)

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

    suite(m"Mutation primitives"):
      def doc(source: String): Tel = Tel.parse(IArray.from(source.getBytes("UTF-8")))

      test(m"UpdateAtom rewrites the targeted inline atom"):
        val tel    = doc("name Alice\n")
        val ptr    = TelPointer.of(Text("name"))
        val result = Mutation(tel, Mutation.Op.UpdateAtom(ptr, 0, Text("Bob")))
        Tel.show(result.document.vouch)
      . assert(_ == Text("name Bob\n"))

      test(m"AttachRemark adds a remark to the targeted compound"):
        val tel    = doc("name Alice\n")
        val ptr    = TelPointer.of(Text("name"))
        val result = Mutation(tel, Mutation.Op.AttachRemark(ptr, Text("primary contact")))
        Tel.show(result.document.vouch)
      . assert(_ == Text("name Alice  # primary contact\n"))

      test(m"RemoveRemark drops a previously attached remark"):
        val tel    = doc("name Alice  # noted\n")
        val ptr    = TelPointer.of(Text("name"))
        val result = Mutation(tel, Mutation.Op.RemoveRemark(ptr))
        Tel.show(result.document.vouch)
      . assert(_ == Text("name Alice\n"))

      test(m"Insert appends a child compound to the parent"):
        val tel    = doc("contact\n  name Alice\n")
        val newCompound = Tel.Compound
                          (Text("email"),
                           IArray(Tel.Atom.Inline(Text("alice@example.com"), 1)),
                           Unset, IArray.empty)
        val ptr    = TelPointer.of(Text("contact"))
        val result = Mutation(tel, Mutation.Op.Insert(ptr, newCompound))
        Tel.show(result.document.vouch)
      . assert(_ == Text("contact\n  name Alice\n  email alice@example.com\n"))

      test(m"Delete removes the addressed compound"):
        val tel    = doc("name Alice\nemail alice@example.com\n")
        val ptr    = TelPointer.of(Text("email"))
        val result = Mutation(tel, Mutation.Op.Delete(ptr))
        Tel.show(result.document.vouch)
      . assert(_ == Text("name Alice\n"))

      test(m"InsertBefore places a new sibling before the target"):
        val tel    = doc("b two\n")
        val a      = Tel.Compound
                      (Text("a"), IArray(Tel.Atom.Inline(Text("one"), 1)), Unset, IArray.empty)
        val ptr    = TelPointer.of(Text("b"))
        val result = Mutation(tel, Mutation.Op.InsertBefore(ptr, a))
        Tel.show(result.document.vouch)
      . assert(_ == Text("a one\nb two\n"))

      test(m"InsertAfter places a new sibling after the target"):
        val tel    = doc("a one\n")
        val b      = Tel.Compound
                      (Text("b"), IArray(Tel.Atom.Inline(Text("two"), 1)), Unset, IArray.empty)
        val ptr    = TelPointer.of(Text("a"))
        val result = Mutation(tel, Mutation.Op.InsertAfter(ptr, b))
        Tel.show(result.document.vouch)
      . assert(_ == Text("a one\nb two\n"))

      test(m"Replace swaps a compound for a new one"):
        val tel    = doc("name Alice\n")
        val replacement = Tel.Compound
                           (Text("name"), IArray(Tel.Atom.Inline(Text("Charlie"), 1)),
                            Unset, IArray.empty)
        val ptr    = TelPointer.of(Text("name"))
        val result = Mutation(tel, Mutation.Op.Replace(ptr, replacement))
        Tel.show(result.document.vouch)
      . assert(_ == Text("name Charlie\n"))

      test(m"SetFlag attaches a flag-typed child compound"):
        val tel    = doc("opt\n")
        val ptr    = TelPointer.of(Text("opt"))
        val result = Mutation(tel, Mutation.Op.SetFlag(ptr, Text("enabled")))
        Tel.show(result.document.vouch)
      . assert(_ == Text("opt\n  enabled\n"))

      test(m"UnsetFlag removes a previously set flag"):
        val tel    = doc("opt\n  enabled\n")
        val ptr    = TelPointer.of(Text("opt"))
        val result = Mutation(tel, Mutation.Op.UnsetFlag(ptr, Text("enabled")))
        Tel.show(result.document.vouch)
      . assert(_ == Text("opt\n"))

      test(m"sequenced ops apply in order"):
        val tel    = doc("name Alice\n")
        val ptr    = TelPointer.of(Text("name"))
        val ops    = Seq
                      ( Mutation.Op.UpdateAtom(ptr, 0, Text("Bob")),
                        Mutation.Op.AttachRemark(ptr, Text("note")) )
        val result = Mutation(tel, ops)
        Tel.show(result.document.vouch)
      . assert(_ == Text("name Bob  # note\n"))

      test(m"pointer with no match raises PointerNotFound"):
        val tel = doc("name Alice\n")
        val ptr = TelPointer.of(Text("missing"))
        capture[MutationError](Mutation(tel, Mutation.Op.Delete(ptr))).reason
      . assert(_ == MutationError.Reason.PointerNotFound)

    suite(m"Tel.parse Text overload"):
      test(m"parse(Text) accepts a Gossamer Text directly"):
        val tel = Tel.parse(Text("name Alice\n"))
        tel.childCompounds.headOption.map(_.keyword).getOrElse(Text(""))
      . assert(_ == Text("name"))

    suite(m"Integration: parse → mutate → print → reparse"):
      def doc(source: String): Tel = Tel.parse(IArray.from(source.getBytes("UTF-8")))

      test(m"editing through the lens preserves surrounding formatting"):
        import dynamicTelAccess.enabled
        val original = doc("# header\nname Alice\nemail a@example.com\n")
        val lens = summon["email" is Lens from Tel onto Tel]
        val updated = lens.modify(original)(_ => Tel.scalar(Text("b@example.com")))
        Tel.show(updated.document.vouch)
      . assert(_ == Text("# header\nname Alice\nemail b@example.com\n"))

      test(m"a multi-step Edit log round-trips through the printer"):
        val original = doc("name Alice\n")
        val edited =
          original.edited
            ( Edit.at(TelPointer.of(Text("name"))).update(Text("Bob"))
           ++ Edit.at(TelPointer.Empty)
                  .insert(Edit.compound(Text("email"), Text("b@example.com"))) )

        val printed   = Tel.show(edited.document.vouch)
        val reparsed  = Tel.parse(IArray.from(printed.s.getBytes("UTF-8")))
        Tel.show(reparsed.document.vouch)
      . assert(_ == Text("name Bob\nemail b@example.com\n"))

    suite(m"Tel.modify and Lens given"):
      import dynamicTelAccess.enabled
      def doc(source: String): Tel = Tel.parse(IArray.from(source.getBytes("UTF-8")))

      test(m"modify replaces an existing field's compound"):
        val tel = doc("name Alice\n")
        val updated = tel.modify("name", Tel.scalar(Text("Bob")))
        updated.selectDynamic("name").primaryAtom
      . assert(_ == Text("Bob"))

      test(m"modify appends when the field is absent"):
        val tel = doc("name Alice\n")
        val updated = tel.modify("email", Tel.scalar(Text("a@b.c")))
        updated.selectDynamic("email").primaryAtom
      . assert(_ == Text("a@b.c"))

      test(m"Lens by field name reads the current value"):
        val tel = doc("name Alice\n")
        val lens = summon["name" is Lens from Tel onto Tel]
        lens(tel).primaryAtom
      . assert(_ == Text("Alice"))

      test(m"Lens.modify updates the field through the transform"):
        val tel = doc("name Alice\n")
        val lens = summon["name" is Lens from Tel onto Tel]
        val updated = lens.modify(tel)(_ => Tel.scalar(Text("Carol")))
        updated.selectDynamic("name").primaryAtom
      . assert(_ == Text("Carol"))

    suite(m"Edit DSL"):
      def doc(source: String): Tel = Tel.parse(IArray.from(source.getBytes("UTF-8")))

      test(m"single-op edit changes one atom"):
        val tel  = doc("name Alice\n")
        val edit = Edit.at(TelPointer.of(Text("name"))).update(Text("Bob"))
        Tel.show(tel.edited(edit).document.vouch)
      . assert(_ == Text("name Bob\n"))

      test(m"chained edits apply in order"):
        val tel = doc("name Alice\n")
        val edit = Edit.at(TelPointer.of(Text("name"))).update(Text("Bob"))
                ++ Edit.at(TelPointer.of(Text("name"))).attachRemark(Text("note"))

        Tel.show(tel.edited(edit).document.vouch)
      . assert(_ == Text("name Bob  # note\n"))

      test(m"Edit.compound helper builds an inline-atom compound"):
        val c = Edit.compound(Text("email"), Text("a@b.c"))
        c.keyword
      . assert(_ == Text("email"))

      test(m"inserting via Edit composes with deletion"):
        val tel  = doc("a 1\nb 2\n")
        val edit = Edit.at(TelPointer.of(Text("b"))).delete
                ++ Edit.at(TelPointer.of(Text("a"))).insertAfter(Edit.compound(Text("c"), Text("3")))

        Tel.show(tel.edited(edit).document.vouch)
      . assert(_ == Text("a 1\nc 3\n"))

      test(m"noop edit returns the document unchanged"):
        val tel = doc("name Alice\n")
        Tel.show(tel.edited(Edit.noop).document.vouch)
      . assert(_ == Text("name Alice\n"))

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
