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

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import turbulence.*
import vacuous.*

// The schema-resolution engine of TEL §8.2, turning a pragma's schema
// identification into a composed schema through the resolution steps:
//
//   0. embedded-schema lookup (a self-contained BinTEL container)
//   1. built-in lookup — the `tels` meta-schema, matched by its pinned
//      coordinate `specification.tel/tels:2.0.0` or its signature,
//      without network access
//   2. content-addressed store lookup (tel schema cache, LIRA store, …)
//   3. library lookup (schema documents already in hand)
//   4. LIRA resolution, through the `Tels.Resolution.Delegate` SPI
//   5. failure, naming the deepest step attempted
//
// Every retrieved schema body is signature-verified: the schema's
// recomputed signature (base plus the pragma's selected layers, in
// order) must match the pragma's claimed signature byte-for-byte.
// Bare references (no selector, no signature) resolve only against a
// store's local reference entries, by design.
object SchemaResolver:
  import Tels.Resolution.{Error as ResolutionError, Step, Store, Delegate}
  import ResolutionError.Reason

  // The built-in `tels` meta-schema's composed signature: the BASE-256
  // palimpsest of `SchemaSignature.fromDocument` over the canonical
  // tels.tel source. Pinned as a golden value, and recomputed from the
  // corpus by the test suite.
  val telsSignature: Text = t"ÔŀưḞ2żbτȚÆAĄſЬMẍỳϋῩJλḤӛ3ñẉḢkŻẋzǓĥ"

  // A resolution outcome: the schema composed with the pragma's layer
  // selection; its source document (`Unset` for the built-in axiom);
  // the signature identity under which it was matched and is cached;
  // and the step that answered.
  case class Resolved(schema: Tels, document: Optional[Tel], signature: Data, step: Step)

  def resolve
    ( pragma:   Tel.Pragma,
      stores:   List[Tels.Resolution.Store]      = List(),
      library:  List[Tel]                        = List(),
      delegate: Optional[Tels.Resolution.Delegate] = Unset,
      embedded: Optional[(Data, Data)]           = Unset,
      axiom:    Tels                             = Tels.Axiom.tels )
    ( using Tactic[Tel.Error], Tactic[Bintel.Error], Tactic[ResolutionError] )
  :   Resolved =

    val selection = pragma.layers
    val claimed: Optional[Data] = pragma.signature.let(Base256.decode(_))
    val identifier = pragma.reference.let(_.text).or(pragma.signature).or(t"<unidentified>")

    // Compose, verify and identify a candidate schema document. The
    // recomputed signature over the base plus the selected layers is
    // the candidate's identity; a claimed pragma signature must agree.
    def accept(tel: Tel, step: Step): Resolved =
      val schema = Tels.Reconstructor.fromTel(tel)
      val composed = Tels.Validation.validate(schema, selection)

      claimed.let: signature =>
        SchemaSignature.verifySelection(tel, schema, axiom, selection, signature)

      val components = SchemaSignature.componentHashes(tel, axiom)
      val chosen = Tels.Layers.select(schema, selection)
      val names = schema.layers.readable.toList.map(_.name)
      val byName = names.zip(components(1).stdlib).toMap
      val chosenHashes = selection.stdlib.map(byName(_))
      val identity = SchemaSignature.encode(components(0) :: chosenHashes.to(List))

      Resolved(composed, tel, identity, step)

    var result: Optional[Resolved] = Unset

    // Step 0: an embedded schema, carried with its signature by a
    // self-contained BinTEL container. A claimed pragma signature must
    // match the embedded one.
    embedded.let: pair =>
      val matches = claimed.let(bytesEqual(_, pair(0))).or(true)
      if matches then result = accept(pair(1).read[Tel], Step.Embedded)

    // Step 1: the built-in meta-schema, answering to its pinned
    // coordinate (with or without the version pin) and to its
    // signature, in both cases without network access.
    if result.absent then
      val builtinSignature = Base256.decode(telsSignature)

      val byCoordinate = pragma.reference.let: reference =>
        reference.isTels && (reference.selector.absent || reference.selector.match
          case Tel.Pragma.Reference.Selector.Version(2, 0, 0) => true
          case _                                              => false)
      . or(false)

      val bySignature = claimed.let(bytesEqual(_, builtinSignature)).or(false)

      if byCoordinate || bySignature then
        val composed = Tels.Layers.compose(Tels.Axiom.tels, selection)
        result = Resolved(composed, Unset, builtinSignature, Step.Builtin)

    // Steps 2–3, signature form: any content-addressed store, then the
    // library of schema documents in hand, with the layer selections as
    // decomposition hints. Every hit is re-verified through `accept`.
    if result.absent then claimed.let: signature =>
      val storeIterator = stores.stdlib.iterator

      while storeIterator.hasNext && result.absent do
        storeIterator.next()(signature).let: body =>
          result = accept(body.read[Tel], Step.Cache)

      // Library documents are schema documents the caller already has
      // in hand, and must be valid schemas; a malformed one aborts
      // resolution rather than being skipped silently.
      val libraryIterator = library.stdlib.iterator

      while libraryIterator.hasNext && result.absent do
        val candidate = libraryIterator.next()
        val schema = Tels.Reconstructor.fromTel(candidate)
        val components = SchemaSignature.componentHashes(candidate, axiom)
        val named = schema.layers.readable.toList.map(_.name).zip(components(1).stdlib)

        val decomposed =
          SchemaSignature.decodeHinted(signature, components(0), named.to(List), selection)

        if decomposed.present then result = accept(candidate, Step.Library)

    // Steps 2–3, bare reference (no selector, no signature): the local
    // schema cache only — the developer's working copy. Never the
    // library, never the delegate, never the network.
    val bare = pragma.reference.let(_.selector.absent).or(false) && claimed.absent

    if result.absent && bare then pragma.reference.let: reference =>
      val storeIterator = stores.stdlib.iterator

      while storeIterator.hasNext && result.absent do
        storeIterator.next().reference(reference.domain, reference.name).let: body =>
          result = accept(body.read[Tel], Step.Cache)

    // Step 4: LIRA resolution by identifier form, through the delegate.
    // A resolved body passes through the same verification as any other
    // candidate, and is cached under its signature identity.
    if result.absent && !bare then delegate match
      case delegate: Tels.Resolution.Delegate =>
        claimed match
          case signature: Data =>
            delegate.bySignature(signature, pragma.reference).let: body =>
              val resolved = accept(body.data.read[Tel], Step.Lira)
              stores.stdlib.headOption.foreach(_.cache(resolved.signature, body.data))
              result = resolved

          case _ => pragma.reference match
            case reference: Tel.Pragma.Reference =>
              delegate.bySelector(reference).let: body =>
                val resolved = accept(body.data.read[Tel], Step.Lira)
                stores.stdlib.headOption.foreach(_.cache(resolved.signature, body.data))
                result = resolved

            case _ => ()

      case _ => ()

    // Step 5: failure, naming the deepest step attempted.
    result.or:
      val step =
        if bare then Step.Cache
        else if delegate.present then Step.Lira
        else Step.Library

      abort(ResolutionError(Reason.Unresolved(step, identifier)))

  private def bytesEqual(a: Data, b: Data): Boolean =
    a.length == b.length && {
      var i     = 0
      var equal = true

      while equal && i < a.length do
        if a.readable(i) != b.readable(i) then equal = false
        i += 1

      equal
    }
