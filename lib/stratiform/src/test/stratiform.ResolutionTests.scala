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

import proscenium.compat.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import charEncoders.utf8Encoder

// Layer-selective composition (§8.1/§20.3), signature decomposition
// under a layer selection, and the schema-resolution engine's step
// order (§8.2): embedded, built-in (pinned coordinate and signature),
// content-addressed stores, library with decomposition hints, the LIRA
// delegate, and failure.
object ResolutionTests extends Suite(m"Stratiform schema resolution tests"):

  private val layeredSource: Text = Text("""|tel 1.0
    |
    |name layered
    |
    |document
    |  field name String
    |
    |layer
    |  name alpha
    |  overlay
    |    field email String
    |
    |layer
    |  name beta
    |  overlay
    |    field phone String
    |
    |layer
    |  name gamma
    |  overlay
    |    field fax String
    |""".stripMargin)

  private def layeredDoc: Tel = layeredSource.read[Tel]
  private def layeredSchema: Tels = Tels.Reconstructor.fromTel(layeredDoc)
  private def layeredBytes: Data = summon[CharEncoder].encoded(layeredSource)

  // The composed document's field keywords, sorted, as one Text.
  private def memberKeywords(schema: Tels): Text =
    Text(schema.document.members.readable.toList
    . collect { case f: Tels.Field => f.keyword.s }.sorted.mkString(","))

  // The composed signature for a selection: the base hash followed by
  // the selected layers' hashes, in declaration order.
  private def signatureFor(selection: List[Text]): Data =
    val (base, layerHashes) = SchemaSignature.componentHashes(layeredDoc, Tels.Axiom.tels)
    val names = layeredSchema.layers.readable.toList.map(_.name)
    val byName = names.zip(layerHashes.stdlib).toMap
    SchemaSignature.encode(List.of(base :: selection.stdlib.map(byName(_))))

  private def pragma
    ( reference: Optional[Tel.Pragma.Reference] = Unset,
      layers:    List[Text]                     = List(),
      signature: Optional[Text]                 = Unset )
  :   Tel.Pragma =

    Tel.Pragma((1, 0), reference, layers, signature, Unset)

  private def telsCorpus: Data =
    val stream = getClass.getResourceAsStream("/stratiform/corpus/tels.tel").nn
    try Array.unsafeFrozen(stream.readAllBytes().nn) finally stream.close()

  private class RecordingDelegate(answer: Optional[Data]) extends Tels.Resolution.Delegate:
    @scala.caps.unsafe.untrackedCaptures var signatureCalls: Int = 0
    @scala.caps.unsafe.untrackedCaptures var selectorCalls: Int = 0

    def bySignature(signature: Data, reference: Optional[Tel.Pragma.Reference])
    :   Optional[Tels.Resolution.Body] raises Tels.Resolution.Error =

      signatureCalls += 1
      answer.let(Tels.Resolution.Body(_))

    def bySelector(reference: Tel.Pragma.Reference)
    :   Optional[Tels.Resolution.Body] raises Tels.Resolution.Error =

      selectorCalls += 1
      answer.let(Tels.Resolution.Body(_))

  def run(): Unit =
    suite(m"Layer-selective composition"):
      test(m"an empty selection composes the base alone"):
        memberKeywords(Tels.Layers.compose(layeredSchema, List()))
      . assert(_ == t"name")

      test(m"a single selection composes only that layer"):
        memberKeywords(Tels.Layers.compose(layeredSchema, List(t"alpha")))
      . assert(_ == t"email,name")

      test(m"a sparse selection skips unselected layers"):
        memberKeywords(Tels.Layers.compose(layeredSchema, List(t"alpha", t"gamma")))
      . assert(_ == t"email,fax,name")

      test(m"selecting every layer matches compose-all"):
        memberKeywords(Tels.Layers.compose(layeredSchema, List(t"alpha", t"beta", t"gamma")))
        == memberKeywords(Tels.Layers.compose(layeredSchema))
      . assert(_ == true)

      test(m"an out-of-order selection is E124"):
        capture[Tel.Error](Tels.Layers.compose(layeredSchema, List(t"gamma", t"alpha")))
        . reason.number
      . assert(_ == 124)

      test(m"a duplicate selection is E124"):
        capture[Tel.Error](Tels.Layers.compose(layeredSchema, List(t"alpha", t"alpha")))
        . reason.number
      . assert(_ == 124)

      test(m"an unknown layer name is a resolution error"):
        capture[Tels.Resolution.Error]
         (Tels.Layers.compose(layeredSchema, List(t"delta"))).reason
      . assert(_ == Tels.Resolution.Error.Reason.UnknownLayer(t"delta"))

    suite(m"Signature decomposition under a layer selection (§8.1)"):
      test(m"the base-only signature verifies with an empty selection"):
        SchemaSignature.verifySelection
         ( layeredDoc, layeredSchema, Tels.Axiom.tels, List(), signatureFor(List()) )
        true
      . assert(_ == true)

      test(m"a selected signature verifies against its selection"):
        SchemaSignature.verifySelection
         ( layeredDoc, layeredSchema, Tels.Axiom.tels, List(t"alpha", t"gamma"),
           signatureFor(List(t"alpha", t"gamma")) )
        true
      . assert(_ == true)

      test(m"a component-count mismatch is reported distinctly"):
        capture[Tels.Resolution.Error]:
          SchemaSignature.verifySelection
           ( layeredDoc, layeredSchema, Tels.Axiom.tels, List(t"alpha"), signatureFor(List()) )
        . reason
      . assert(_ == Tels.Resolution.Error.Reason.ComponentCount(2, 1))

      test(m"swapped layer components name the first diverging layer"):
        val (base, layerHashes) = SchemaSignature.componentHashes(layeredDoc, Tels.Axiom.tels)
        val hashes = layeredSchema.layers.readable.toList.map(_.name).zip(layerHashes.stdlib).toMap
        val swapped =
          SchemaSignature.encode(List.of(base :: scala.List(hashes(t"gamma"), hashes(t"alpha"))))

        capture[Tels.Resolution.Error]:
          SchemaSignature.verifySelection
           ( layeredDoc, layeredSchema, Tels.Axiom.tels, List(t"alpha", t"gamma"), swapped )
        . reason
      . assert(_ == Tels.Resolution.Error.Reason.LayerMismatch(t"alpha"))

      test(m"a layer hash in base position is a base mismatch"):
        val (base, layerHashes) = SchemaSignature.componentHashes(layeredDoc, Tels.Axiom.tels)
        val hashes = layeredSchema.layers.readable.toList.map(_.name).zip(layerHashes.stdlib).toMap
        val wrongBase =
          SchemaSignature.encode(List.of(hashes(t"gamma") :: scala.List(hashes(t"alpha"))))

        capture[Tels.Resolution.Error]:
          SchemaSignature.verifySelection
           ( layeredDoc, layeredSchema, Tels.Axiom.tels, List(t"alpha"), wrongBase )
        . reason
      . assert(_ == Tels.Resolution.Error.Reason.BaseMismatch)

    suite(m"Resolution engine (§8.2)"):
      test(m"the pinned built-in signature matches the canonical tels.tel"):
        Base256.encode(SchemaSignature.fromDocument(telsCorpus.read[Tel], Tels.Axiom.tels))
      . assert(_ == SchemaResolver.telsSignature)

      test(m"the pinned coordinate answers at the built-in step"):
        SchemaResolver.resolve(pragma(reference = Tel.Pragma.Reference.tels)).step
      . assert(_ == Tels.Resolution.Step.Builtin)

      test(m"the unpinned tels coordinate answers at the built-in step"):
        val reference = Tel.Pragma.Reference(t"specification.tel", t"tels", Unset)
        SchemaResolver.resolve(pragma(reference = reference)).step
      . assert(_ == Tels.Resolution.Step.Builtin)

      test(m"the built-in signature answers at the built-in step"):
        SchemaResolver.resolve(pragma(signature = SchemaResolver.telsSignature)).step
      . assert(_ == Tels.Resolution.Step.Builtin)

      test(m"a mismatched tels version pin does not answer as built-in"):
        val reference = Tel.Pragma.Reference
         ( t"specification.tel", t"tels", Tel.Pragma.Reference.Selector.Version(2, 0, 0) )

        capture[Tels.Resolution.Error](SchemaResolver.resolve(pragma(reference = reference)))
        . reason
        match
          case Tels.Resolution.Error.Reason.Unresolved(_, _) => true
          case _                                             => false
      . assert(_ == true)

      test(m"a cached body answers at the store step"):
        val store = Tels.Resolution.Store.Memory()
        val signature = signatureFor(List())
        store.cache(signature, layeredBytes)

        SchemaResolver.resolve
         ( pragma(signature = Base256.encode(signature)), stores = List(store) )
        . step
      . assert(_ == Tels.Resolution.Step.Cache)

      test(m"a tampered cached body fails verification"):
        val store = Tels.Resolution.Store.Memory()
        val signature = signatureFor(List())
        val tampered = summon[CharEncoder].encoded(
          Text("tel 1.0\n\nname other\n\ndocument\n  field other String\n"))
        store.cache(signature, tampered)

        capture[Tels.Resolution.Error]:
          SchemaResolver.resolve
           ( pragma(signature = Base256.encode(signature)), stores = List(store) )
        . reason
        match
          case Tels.Resolution.Error.Reason.Unverified(_) => true
          case _                                          => false
      . assert(_ == true)

      test(m"a library document answers through decomposition hints"):
        val signature = signatureFor(List(t"alpha"))

        val resolved = SchemaResolver.resolve
         ( pragma(layers = List(t"alpha"), signature = Base256.encode(signature)),
           library = List(layeredDoc) )

        (resolved.step, memberKeywords(resolved.schema))
      . assert(_ == (Tels.Resolution.Step.Library, t"email,name"))

      test(m"a bare reference resolves from the local store only"):
        val store = Tels.Resolution.Store.Memory()
        store.install(t"example.com", t"layered", layeredBytes)
        val delegate = RecordingDelegate(Unset)

        val resolved = SchemaResolver.resolve
         ( pragma(reference = Tel.Pragma.Reference(t"example.com", t"layered", Unset)),
           stores = List(store), delegate = delegate )

        (resolved.step, delegate.signatureCalls + delegate.selectorCalls)
      . assert(_ == (Tels.Resolution.Step.Cache, 0))

      test(m"an unresolved bare reference never reaches the delegate"):
        val delegate = RecordingDelegate(Unset)

        val reason =
          capture[Tels.Resolution.Error]:
            SchemaResolver.resolve
             ( pragma(reference = Tel.Pragma.Reference(t"example.com", t"absent", Unset)),
               delegate = delegate )
          . reason

        (reason, delegate.signatureCalls + delegate.selectorCalls)
      . assert(_ == (Tels.Resolution.Error.Reason.Unresolved
          (Tels.Resolution.Step.Cache, t"example.com/absent"), 0))

      test(m"a selector-form reference resolves through the delegate and is cached"):
        val store = Tels.Resolution.Store.Memory()
        val delegate = RecordingDelegate(layeredBytes)

        val reference = Tel.Pragma.Reference
         ( t"example.com", t"layered", Tel.Pragma.Reference.Selector.Version(1, 0, 0) )

        val first = SchemaResolver.resolve
         ( pragma(reference = reference), stores = List(store), delegate = delegate )

        val second = SchemaResolver.resolve
         ( pragma(signature = Base256.encode(first.signature)), stores = List(store) )

        (first.step, second.step, delegate.selectorCalls)
      . assert(_ == (Tels.Resolution.Step.Lira, Tels.Resolution.Step.Cache, 1))

      test(m"a signature unresolved without a delegate names the library step"):
        capture[Tels.Resolution.Error]:
          SchemaResolver.resolve(pragma(signature = Base256.encode(signatureFor(List(t"alpha")))))
        . reason
        match
          case Tels.Resolution.Error.Reason.Unresolved(step, _) => step
          case other                                            => other
      . assert(_ == Tels.Resolution.Step.Library)

      test(m"an embedded schema answers first"):
        val signature = signatureFor(List())

        SchemaResolver.resolve
         ( pragma(signature = Base256.encode(signature)),
           embedded = (signature, layeredBytes) )
        . step
      . assert(_ == Tels.Resolution.Step.Embedded)
