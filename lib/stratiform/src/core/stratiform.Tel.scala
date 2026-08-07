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
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package stratiform


import scala.collection.immutable.Vector

import scala.caps

import proscenium.compat.*

import scala.collection.Factory
import scala.language.dynamics

import anticipation.*
import aperture.*
import contingency.*
import denominative.{Span, z}
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import wisteria.*
import zephyrine.*

// Presentation model from §17 of the TEL specification. The Scala AST is
// structurally identical to the reference implementation's AST so that
// upstream `.check` fixtures round-trip through a cross-language CheckTree
// (see stratiform.CheckFormat).
//
// `Tel` itself is a thin wrapper around a `Subtree` (either the document
// root or a single compound). Both Subtree variants share a `children:
// Array[Block]^{}` field, so flat traversal logic can address either form
// without case analysis.

object Tel extends Tel2:

  // Consolidated names for the schema-related types. `TelError` lives
  // at the top level by design (the other types are defined inline in
  // `object Tel` below as their canonical location).
  type Error = TelError
  val Error: TelError.type = TelError

  // The `Openable` instance for a writable TEL source:
  // `source.open[Tel](Read & Write)` parses on entry, offers the §22.2
  // machine operations on the handle, and writes the document back on
  // normal scope close when it was mutated (or unconditionally with
  // `TelFlag.Force`). Takes priority over `Tel2.telViewOpenable`
  // whenever the source is also writable.
  given telOpenable: [source]
  =>  ( readable: (source is Readable to Tel)^,
        writable: (source is Writable by Data)^ )
  =>  (TelOpenable[source]^{readable, writable}) =
    TelOpenable[source]()

  // The source `Position` type located by `tel.locate(pointer)` and carried on a
  // `Tel.Focus`. Defined canonically on `TelError` (where it also locates parse
  // errors); aliased here for symmetry with `Json.Ast.Position` / `Yaml.Ast.Position`.
  type Position = TelError.Position
  val Position: TelError.Position.type = TelError.Position

  // The focus carried by `Tel#as[T]` for multi-error accrual: a keyword path
  // identifying the field being decoded, and the source `Span` of the text the
  // error is about. Mirrors `Json.Focus` / `Yaml.Focus`. The `span` is populated
  // lazily by `withSpan(tel)` / `withKeySpan(tel)`, which delegate to
  // `tel.locate(pointer)` / `tel.locateKey(pointer)`, so a root parsed without
  // position tracking (hence no `positionIndex`) leaves `span` empty and the
  // success path pays nothing.
  case class Focus(pointer: TelPath = TelPath.Root, span: Span = Span.empty)
  derives CanEqual:
    def withSpan(tel: Tel): Focus = copy(span = tel.locate(pointer).lay(Span.empty)(_.span))

    def withKeySpan(tel: Tel): Focus =
      copy(span = tel.locateKey(pointer).lay(Span.empty)(_.span))

  // Fill in a source `Span` for every focus accrued so far by locating its
  // keyword path in `tel` — the counterpart of `Json#as`'s enrichment. A no-op
  // unless the ambient `Foci` is a tracking one and `tel` was parsed under
  // `parsing.trackPositions` (so carries a `positionIndex`); an unresolvable
  // path — a member absent from the source — keeps its pointer and leaves the
  // span empty. An error registered outside every `focus` block has no
  // focus at all and takes the root one, which renders as `#`, exactly as an
  // absent focus did. Public because `Tel#as` is `inline` and expands into
  // user modules, which may only reference public members.
  //
  // The two forms differ in which region of the compound they point at, and the
  // caller picks by phase: decoding says "this value is wrong", so `Tel#as` uses
  // the value; schema validation says "this member is wrong", so `Type.assign`
  // uses the keyword — an unknown-keyword diagnostic must underline the keyword,
  // which may be all the compound has.
  def supplementPositions(tel: Tel)(using foci: Foci[Tel.Focus]): Unit =
    foci.supplement(foci.length, _.let(_.withSpan(tel)).or(Tel.Focus()))

  def supplementKeyPositions(tel: Tel)(using foci: Foci[Tel.Focus]): Unit =
    foci.supplement(foci.length, _.let(_.withKeySpan(tel)).or(Tel.Focus()))

  extension (tel: Tel)
    def edited(revision: Revision): Tel raises MutationError = revision(tel)

    // Runtime-checks `tel` against `topic`, then re-types it as a schema-typed
    // `Tel of topic from topic`. The phantom `Topic` records the current position
    // within the schema (initially the root, `topic`) and `Origin` records the
    // root schema, so the `Dynamic` navigation macros can resolve fields and
    // child indices against `topic` at compile time — with no `dynamicTelAccess`
    // import required.
    //
    // The runtime conformance check is a decode-and-discard against `topic`'s
    // `Tel.Decodable` (a nonconformant value raises `TelError`): TEL scalars are
    // untyped text, so a structural walk over the schema cannot catch type mismatches
    // (e.g. a non-numeric `Int` field) that decoding does — hence decoding remains the
    // strict check. The carrying `Tel.Decodable` also supplies the schema for the
    // phantom anchor, so the schema is guaranteed coherent with the decoder used.
    def verify[topic](using topic is Tel.Decodable)
    :   (Tel of topic from topic) raises TelError =

      tel.as[topic]
      tel.asInstanceOf[Tel of topic from topic]

  // How a value of a codec's type occupies a compound line's atom positions,
  // approximating §20.2's member classification from Scala types alone
  // (issue #1694): `Scalar` is atom-assignable and never skipped, `Flag` is
  // atom-assignable when the atom text equals the keyword, and `Struct` (a
  // nested record, sum, or map) can only be filled by a keyword child.
  //
  // The schema-less approximation diverges from §20.2 in documented ways.
  // No derived codec is `Flag`-natured: a §20 flag is keyword presence
  // alone, whereas a Scala `Boolean` is a value, so it maps to `Scalar` and
  // reads and writes an explicit `true`/`false` atom — the mapping the
  // derived schema and BinTEL also use. A custom codec for a genuinely
  // flag-shaped type may opt into `Flag`. Likewise an all-singleton enum
  // decodes through its text codec and so is `Scalar`, and non-singleton
  // sums are `Struct` even where a schema's all-Flag select would be
  // atom-assignable.
  enum Nature:
    case Scalar, Flag, Struct

  object Encodable:
    // The shape is an explicit, nameable thunk (not by-name) so the instance's
    // capture set can name it: an honest result rather than a laundered-pure one.
    // Mirrors jacinta's `Json.Encodable.apply` (see rep/DECISIONS.md).
    def apply[value]
      ( shape0:    () => Morphology,
        nature0:   Tel.Nature = Tel.Nature.Struct,
        optional0: Boolean = false )
      ( lambda: (value -> Tel)^ )
    :   ((value is Tel.Encodable)^{shape0, lambda}) =

      new Tel.Encodable:
        type Self = value
        def encoded(value: value): Tel = lambda(value)
        def shape(): Morphology = shape0()
        override def nature: Tel.Nature = nature0
        override def optional: Boolean = optional0

  // A TEL encoder/decoder that also carries the format-neutral `Morphology` of exactly
  // what it reads/writes, so a fused `Encodable & Schematic` / `Decodable &
  // Schematic` (built by `telSchematics`) is coherent by construction — the shape
  // travels with the codec rather than being resolved independently. The `Morphology` is
  // reified into a concrete `Tels.Type` downstream. Mirrors jacinta's
  // `Json.Encodable`/`Json.Decodable`. Not itself `Schematic`.
  trait Encodable extends anticipation.Encodable:
    type Form = Tel
    def shape(): Morphology

    // The §20.2 member classification of this encoder's type — see
    // `Tel.Nature`. Conservatively `Struct` unless overridden.
    def nature: Tel.Nature = Tel.Nature.Struct

    // True when a field of this type may be legitimately absent (an
    // `Optional` wrapper), mirroring `Decodable.optional`.
    def optional: Boolean = false

    // The §22.2 canonical child form of a value — for derived products, the
    // compound carrying its leading inline atom run (`Mutation.construct`);
    // identical to `encoded` unless overridden. This is the embedding form
    // a canonicalizing parent uses for its nested values.
    def constructed(value: Self): Tel = encoded(value)

    // The §22.2 canonical presentation rooted as a document (the root
    // carries no atoms, §20.2): for derived products, the member fields as
    // top-level children whose own nested records carry inline runs.
    // Identical to `constructed` unless overridden.
    def canonicalized(value: Self): Tel = constructed(value)

  // The §22.2 canonical presentation of a value: `.encode` keeps the fully
  // keyword-child wire form (which typed navigation and the optics rely on);
  // this opt-in rendering puts leading scalar and flag fields of each nested
  // record in atom position per §22.2 `construct`. Both forms decode to the
  // same value (§19.1).
  def canonical[value](value: value)(using encodable: value is Tel.Encodable): Tel =
    encodable.canonicalized(value)

  object Decodable:
    // Explicit shape thunk, as in `Tel.Encodable.apply`.
    def apply[value]
      ( shape0:    () => Morphology,
        nature0:   Tel.Nature = Tel.Nature.Struct,
        optional0: Boolean = false )
      ( decoder: (value is distillate.Decodable in Tel)^ )
    :   ((value is Tel.Decodable)^{shape0, decoder}) =

      new Tel.Decodable:
        type Self = value
        def decoded(tel: Tel): value = decoder.decoded(tel)
        def shape(): Morphology = shape0()
        override def nature: Tel.Nature = nature0
        override def optional: Boolean = optional0

  trait Decodable extends distillate.Decodable:
    type Form = Tel
    def shape(): Morphology

    // True for collection decoders (`List`/`Set`): a repeated field. The product
    // decoder gathers all same-keyword sibling compounds for such a field and hands
    // them here as a Document; other fields read a single matching compound.
    def repeatable: Boolean = false

    // The §20.2 member classification of this decoder's type — see
    // `Tel.Nature`. Drives the derived product decoder's positional atom
    // pre-pass. Conservatively `Struct` (never atom-assignable) unless
    // overridden.
    def nature: Tel.Nature = Tel.Nature.Struct

    // True when a field of this type may be legitimately absent (an
    // `Optional` wrapper): with a declared default, this makes the field
    // non-required for the §20.2 step 3a skip rule.
    def optional: Boolean = false

    // What a field of this type yields when no child compound carries its
    // keyword — the AST counterpart of `Parsing.absent`. The default
    // replicates the derivation's historical fallback: decode an empty node
    // (primitives raise `Absent` from it).
    def absent()(using Tactic[TelError]): Self = decoded(Tel.empty)

  // The shared substance of `Tel.Parsable` and `Tel.Field`, mirroring
  // jacinta's `Json.Parsing`. The two subtraits add nothing: they exist so
  // that neither is a subtype of the other — a subtype relation in either
  // direction would make one family's givens candidates for the other's
  // queries (nominal instances clashing ambiguously with the field fallback,
  // and Wisteria's codec probe finding a generic type's own `Parsable` given
  // while deriving it).
  //
  // Unlike JSON — whose reader is at a self-describing token whenever a value
  // begins — a TEL value is an *entry*: a keyword line plus its subtree, whose
  // extent depends on the entry's indent. The operative method is therefore
  // `parse(reader, indent)`, invoked with the reader positioned right after
  // the entry's keyword; the instance consumes the entire entry (line
  // remainder plus subtree). The inherited no-indent `parse(reader)` is the
  // whole-document form used at the top level, where there is no entry line:
  // its default mirrors the AST's decode-of-a-document for a scalar (no
  // atoms, hence absent).
  trait Parsing extends distillate.Parsable:
    type Transport = Tel
    type Reader = TelReader
    def shape(): Morphology

    // True for collection parsers: a repeated field, gathered occurrence by
    // occurrence by the derived product parser — the direct counterpart of
    // `Tel.Decodable.repeatable`.
    def repeatable: Boolean = false

    // The §20.2 member classification of this parser's type — see
    // `Tel.Nature`. Drives the derived product parser's positional atom
    // pre-pass. Conservatively `Struct` unless overridden.
    def nature: Tel.Nature = Tel.Nature.Struct

    // True when a field of this type may be legitimately absent (an
    // `Optional` wrapper), mirroring `Tel.Decodable.optional`.
    def optional: Boolean = false

    // Parse one positionally-assigned atom as this field's value. Only
    // meaningful for `Scalar`-natured instances; the default reports the
    // §20.2 step 3c mismatch.
    def parseAtom(text: Text)(using Tactic[TelError]): Self =
      abort(TelError(TelError.Reason.AtomAtNonAssignablePos))

    // The value of a `Flag`-natured field made present by a bare atom
    // matching its keyword.
    def parseFlag()(using Tactic[TelError]): Self =
      abort(TelError(TelError.Reason.AtomAtNonAssignablePos))

    def parse(reader: TelReader^, indent: Int): Self

    def parse(reader: TelReader^): Self = absent()(using reader.errorTactic)

    // What a field of this type yields when no child compound carries its
    // keyword: an error unless overridden (the bridge delegates to its
    // decoder over an empty `Tel`, exactly like the AST derivation's
    // missing-field fallback).
    def absent()(using Tactic[TelError]): Self = abort(TelError(TelError.Reason.Absent))

  object Parsable:
    // The base of generated parsers: generated code is capture-erased, so
    // the bodies receive the reader as a neutral carrier, and the
    // capability is asserted here at the rim — the audited point — like the
    // reader's own accessors. (A generated override of `parse` itself would
    // narrow the trait's `TelReader^` parameter to a pure type, which
    // capture checking rejects at the instantiation site.)
    abstract class Direct[value] extends Tel.Parsable:
      type Self = value

      def shape(): Morphology = Morphology.Any

      protected def parseEntry(reader: AnyRef, indent: Int): value
      protected def parseWhole(reader: AnyRef): value

      def parse(reader: TelReader^, indent: Int): value =
        parseEntry(reader.asInstanceOf[AnyRef], indent)

      override def parse(reader: TelReader^): value = parseWhole(reader.asInstanceOf[AnyRef])

    // The call points for a nominal `Parsing` instance in an entry position
    // of a *generated* parser (a recursive record's own `Parsable`, a
    // hand-written one, or the `Tel.Field` fallback chain). Both travel as
    // neutral carriers — generated code is capture-erased — and the
    // capability is reasserted here, at the audited point.
    def parseField[value](parsing: AnyRef, reader: AnyRef, indent: Int): value =
      parsing.asInstanceOf[value is Tel.Parsing].parse(reader.asInstanceOf[TelReader^], indent)

    def absentField[value](parsing: AnyRef)(using Tactic[TelError]): value =
      parsing.asInstanceOf[value is Tel.Parsing].absent()

    def apply[value](shape0: => Morphology)(parser: (reader: TelReader^) => value)
    :   ((value is Tel.Parsable)^{parser}) =

      // Shape-thunk laundering as in `Tel.Encodable.apply`.
      val shape1: () -> Morphology = caps.unsafe.unsafeAssumePure:
        () => shape0

      new Tel.Parsable:
        type Self = value
        def parse(reader: TelReader^, indent: Int): value = parser(reader)
        def shape(): Morphology = shape1()

    // The universal bridge from the AST world: materialize one entry (or, at
    // the top level, the whole document) as a `Tel` and decode it. Field
    // types with only a `Tel.Decodable` keep working through this, and it is
    // the user's one-line escape hatch when a custom decoder must beat a
    // derived direct parser. A repeatable decoder (a custom collection)
    // keeps its gathering semantics: each occurrence is materialized
    // separately and the occurrences are decoded together as a synthetic
    // document, exactly as the AST derivation hands them over.
    def fromDecodable[value](decodable: (value is Tel.Decodable)^)
    :   ((value is Tel.Parsable)^{decodable}) =

      if decodable.repeatable then
        new Tel.Parsable with Gathering:
          type Self = value
          def shape(): Morphology = decodable.shape()
          override def repeatable: Boolean = true
          override def nature: Tel.Nature = decodable.nature
          override def optional: Boolean = decodable.optional

          def parse(reader: TelReader^, indent: Int): value =
            decodable.decoded(reader.value(indent))

          override def parse(reader: TelReader^): value = decodable.decoded(reader.document())
          override def absent()(using Tactic[TelError]): value = decodable.absent()
          def parseElement(reader: TelReader^, indent: Int): Any = reader.value(indent)

          // A positional atom becomes a synthetic one-atom compound, exactly
          // the element form `gathered` re-assembles into its document.
          def parseAtomElement(text: Text)(using Tactic[TelError]): Any =
            Tel.make(Tel.Compound(t"", Array.of(Tel.Atom.Inline(text, 1)), Unset, Array.empty))

          def gathered(elements: List[Any]): value =
            val compounds = Array.from:
              elements.stdlib.map: element =>
                element.asInstanceOf[Tel].subtree.absolve match
                  case compound: Tel.Compound => compound

            decodable.decoded(Tel.make(gatheredDocument(compounds)))
      else
        new Tel.Parsable:
          type Self = value
          def shape(): Morphology = decodable.shape()
          override def nature: Tel.Nature = decodable.nature
          override def optional: Boolean = decodable.optional

          def parse(reader: TelReader^, indent: Int): value =
            decodable.decoded(reader.value(indent))

          override def parse(reader: TelReader^): value = decodable.decoded(reader.document())
          override def absent()(using Tactic[TelError]): value = decodable.absent()

          // The AST bridge for a positional atom: decode a synthetic
          // one-atom compound, as the derived decoder hands one over.
          override def parseAtom(text: Text)(using Tactic[TelError]): value =
            decodable.decoded:
              Tel.make(Tel.Compound(t"", Array.of(Tel.Atom.Inline(text, 1)), Unset, Array.empty))

    // The one-line opt-in to direct parsing for a structural type:
    // `given MyType is Tel.Parsable = Tel.Parsable.derived` — a
    // Wisteria-derived direct parser, wrapped as a *nominal* `Parsable` so
    // it participates in the read trigger. Deliberately a method, not a
    // blanket given: a type parses directly only once it has opted in.
    inline def derived[value](using Reflection[value]): value is Tel.Parsable =
      fromField(ParsableDerivation.derivedOne[value])

    // The staged counterpart of `derived`: a macro-generated monomorphic
    // parser whose field values live in typed locals, whose keywords dispatch
    // through packed-`Long` literal comparisons, and whose record is built by
    // a direct constructor call — no `Array[Any]` buffer, no `Mirror`, no
    // per-field boxing. Semantics (wire keywords, §19.2 positional atom
    // assignment, gathering, first-match-wins duplicates, defaults, absents,
    // error foci) mirror `derived` exactly; the generated instance's
    // `shape()` is `Morphology.Any`. Requires a top-level or object-nested
    // case class with a single parameter list — sums, method-local classes
    // and other shapes use `derived`.
    inline def staged[value]: value is Tel.Parsable =
      ${ stratiform.internal.stagedParsable[value]('{ adversaria.relabelling[value, Tel] }) }

    def fromField[value](field0: (value is Tel.Parsing)^)
    :   ((value is Tel.Parsable)^{field0}) =

      new Tel.Parsable:
        type Self = value
        def parse(reader: TelReader^, indent: Int): value = field0.parse(reader, indent)
        override def parse(reader: TelReader^): value = field0.parse(reader)
        def shape(): Morphology = field0.shape()
        override def repeatable: Boolean = field0.repeatable
        override def nature: Tel.Nature = field0.nature
        override def optional: Boolean = field0.optional
        override def absent()(using Tactic[TelError]): value = field0.absent()
        override def parseAtom(text: Text)(using Tactic[TelError]): value = field0.parseAtom(text)
        override def parseFlag()(using Tactic[TelError]): value = field0.parseFlag()

    // The element-wise hooks of a repeatable (collection) parser. The
    // derived product parser gathers each same-keyword occurrence through
    // `parseElement` and builds the collection once the entry region ends —
    // the direct counterpart of the AST derivation collecting all matching
    // compounds into a synthetic document for the collection decoder.
    // The self type is capturing so implementing instances may capture their
    // element parser or decoder, like any other `Parsing` instance.
    private[stratiform] trait Gathering:
      self: Tel.Parsing^ =>
      def parseElement(reader: TelReader^, indent: Int): Any

      // One element built from a positionally-assigned atom: §19.2 lets a
      // repeatable member's occurrences split between inline atoms on the
      // parent line and later same-keyword children.
      def parseAtomElement(text: Text)(using Tactic[TelError]): Any

      def gathered(elements: List[Any]): Self

    // The synthetic document the AST derivation hands to a repeatable
    // decoder: one block containing the gathered compounds.
    private[stratiform] def gatheredDocument(compounds: Array[Tel.Compound]^{}): Tel.Document =
      Tel.Document
        ( Unset, Unset, Tel.LineEndings.Lf,
          Array.of(Tel.Block(Array.empty[Tel.Comment], Unset, compounds, 0)) )

    // Shared collection implementation, used by the nominal `Tel.Parsable`
    // given (elements must themselves be nominally `Parsable`, so the read
    // trigger stays opt-in) and by the `Tel.Field` given in `Tel2` (elements
    // resolve through the fallback chain). Sealed per the codec-thunk
    // pattern: a by-name parameter cannot be named in a capture set.
    def iterable[collection <: Iterable, element]
      ( field: => (element is Tel.Parsing)^ )
      ( using factory: Factory[element, collection[element]], tactic: Tactic[TelError] )
    :   collection[element] is Tel.Parsable =

      caps.unsafe.unsafeAssumePure:
        new Tel.Parsable with Gathering:
          type Self = collection[element]
          def shape(): Morphology = Morphology.Arr(field.shape())
          override def repeatable: Boolean = true
          override def nature: Tel.Nature = field.nature
          override def optional: Boolean = true

          def parseElement(reader: TelReader^, indent: Int): Any = field.parse(reader, indent)

          def parseAtomElement(text: Text)(using Tactic[TelError]): Any = field.parseAtom(text)

          def gathered(elements: List[Any]): collection[element] =
            val builder = factory.newBuilder
            elements.each: item => builder += item.asInstanceOf[element]
            builder.result()

          // A single entry decoded as a collection: one element — the AST
          // collection decoder's behavior when handed a lone compound.
          def parse(reader: TelReader^, indent: Int): collection[element] =
            val builder = factory.newBuilder
            builder += field.parse(reader, indent)
            builder.result()

          // A whole document decoded as a collection: every top-level entry
          // is an element, whatever its keyword — as on the AST path.
          override def parse(reader: TelReader^): collection[element] =
            val builder = factory.newBuilder
            var keyword = reader.keyword(0)

            while keyword.present do
              builder += field.parse(reader, 0)
              keyword = reader.keyword(0)

            builder.result()

    // Shared `Optional` implementation, mirroring the AST `optionalDecodable`
    // exactly: an entry with neither an inline atom nor a child compound
    // reads as `Unset` (and so does a missing keyword); anything else flows
    // to the inner parser. Stays in the `Field` world (the inner instance is
    // by-name, so recursive types tie their knot here, like collections) and
    // decides via a non-consuming reader probe. Sealed per the codec-thunk
    // pattern: a by-name parameter cannot be named in a capture set.
    def optionality[inner <: value, value >: Unset.type]
      ( field: => (inner is Tel.Parsing)^ )
      ( using tactic: Tactic[TelError] )
    :   value is Tel.Parsable =

      caps.unsafe.unsafeAssumePure:
        new Tel.Parsable:
          type Self = value
          def shape(): Morphology = Morphology.Opt(field.shape())
          override def nature: Tel.Nature = field.nature
          override def optional: Boolean = true

          def parse(reader: TelReader^, indent: Int): value =
            if reader.hasSubstance then field.parse(reader, indent)
            else
              reader.skipEntry(indent)
              Unset

          override def absent()(using Tactic[TelError]): value = Unset

          override def parseAtom(text: Text)(using Tactic[TelError]): value =
            field.parseAtom(text)

          override def parseFlag()(using Tactic[TelError]): value = field.parseFlag()

    // Sentinel for the derived product parser's value buffer: a slot still
    // `AbsentSlot` after the entry loop had no matching keyword
    // (`null`-checking would be unsound — `Optional` fields legitimately
    // store a null-backed `Unset`).
    private[stratiform] val AbsentSlot: AnyRef = new Object

    // Positional construction through the threaded `Mirror`, from the value
    // buffer the parse loop filled. `fromProduct` is the only construction
    // form that works for method-local and object-nested case classes.
    def assemble[derivation <: Product]
      ( reflection: ProductReflection[derivation], values: Array[Any]^{} )
    :   derivation =

      reflection.fromProduct(ArrayProduct(values))

    private final class ArrayProduct(values: Array[Any]^{}) extends Product:
      def canEqual(that: Any): Boolean = true
      def productArity: Int = values.length
      def productElement(index: Int): Any = values.readUnchecked(index)

    // The prior focus's pointer, extended by one step — evaluated only at
    // error-registration time, exactly as the AST derivation builds its
    // per-field focus.
    //
    // The span works inwards-out: `focus` blocks complete innermost-first, so a
    // span already on `base0` was stamped by a nearer, more specific frame and
    // is kept. Only a level that has none takes `span`, and a level with nothing
    // to point at (a field whose keyword never arrived) passes none.
    private def descend(base0: Optional[Tel.Focus], keyword: Text, span: Span = Span.empty)
    :   Tel.Focus =

      val pointer = base0.let(_.pointer).or(TelPath.Root).prepend(keyword)
      val inherited = base0.let(_.span).or(Span.empty)

      Tel.Focus(pointer, if inherited.exists then inherited else span)

    // Support points for staged parsers, which are generated into user
    // modules and so may only reference public members.

    // The wire keywords of a product's fields: `@name` renames applied
    // verbatim, camel→kebab otherwise — the same mapping as the derivations.
    def wireKeywords(names: Array[String]^{}, renames: Map[Text, Text]): Array[String]^{} =
      names.map { name => renames(name.tt).or(camelToKebab(name)).s }

    // A required primitive field whose keyword never arrived: the primitives'
    // `absent()` semantics — raise and continue with the sentinel. It carries no
    // span: the field is not in the source, so there is no text to point at,
    // exactly as an unresolvable pointer yields no span on the AST path.
    def missing[value](sentinel: value)(using Tactic[TelError]): value =
      raise(TelError(TelError.Reason.Absent)) yet sentinel

    // A present entry whose atom was missing or unparseable: the byte-parsed
    // primitives' fault split — a missing atom is `Absent`, a
    // present-but-unparseable one `NotScalar` with the offending atom's text.
    def scalarFault[value](reader: TelReader^, expected: Text, sentinel: value): value =
      if reader.primaryPresent
      then
        reader.fault(TelError.Reason.NotScalar(reader.primaryText.or(t""), expected))
        sentinel
      else reader.fault(TelError.Reason.Absent) yet sentinel

    // Focus bookkeeping for one field read, compiled away when the ambient
    // `Foci` is the inert default — the same short-circuit as the derived
    // parser's loop.
    //
    // `contingency.focus` runs its transform in a `finally`, after the block, so
    // by then the parser has read this field's entry — and, if the field is a
    // record, entries deeper than it. The keyword span is therefore captured
    // *before* the block, when the parser is still parked on this entry, and the
    // value span is asked for afterwards against the ordinal captured with it:
    // it answers only if the entry read was this one. A leaf field gets its
    // value; a record field, whose block descended, gets its keyword.
    inline def focusing[result](foci: Foci[Tel.Focus], reader: TelReader^, keyword: Text)
      ( inline block: => result )
    :   result =

      if !foci.active then block else
        val ordinal = reader.entryOrdinal
        val keywordSpan = reader.keywordSpan

        focus(using foci):
          val value = reader.valueSpan(ordinal)
          descend(prior, keyword, if value.exists then value else keywordSpan)

        . apply(block)

    // As `focusing`, but for a field with no entry of its own in the source — a
    // keyword that never arrived, or the tail of a repeatable field gathered
    // once the entry loop has finished. The parser is parked on whatever it read
    // last, which is not this field, so the focus takes its pointer and no span
    // rather than one pointing at unrelated text.
    inline def focusingUnlocated[result](foci: Foci[Tel.Focus], keyword: Text)
      ( inline block: => result )
    :   result =
      if foci.active then focus(using foci)(descend(prior, keyword))(block) else block

    // Linear keyword dispatch for the general step — an unpackable wire
    // keyword, or any keyword of a `@name`-annotated record.
    def keywordIndex(keys: Array[String]^{}, keyword: Text): Int =
      val count = keys.length
      val name: String = keyword.s
      var index = 0

      while index < count do
        if keys.readUnchecked(index) == name then return index
        index += 1

      -1

    // The repeatable-field hooks, looking through the `Field.Adapter` — for
    // staged parsers, which cannot name the private `Gathering` trait. A
    // field gathers only when its (unwrapped) instance is a repeatable
    // `Gathering`, exactly the derived engine's test.
    def repeats(parsing: Tel.Parsing): Boolean =
      val actual = unwrap(parsing)
      actual.repeatable && actual.isInstanceOf[Gathering]

    def parseElement(parsing: Tel.Parsing, reader: TelReader^, indent: Int): Any =
      (unwrap(parsing): @unchecked) match
        case gathering: Gathering => gathering.parseElement(reader, indent)

    // One element of a repeatable field built from a positionally-assigned
    // atom (§19.2): occurrences may split between inline atoms and later
    // same-keyword children.
    def parseAtomElement(parsing: Tel.Parsing, text: Text)(using Tactic[TelError]): Any =
      (unwrap(parsing): @unchecked) match
        case gathering: Gathering => gathering.parseAtomElement(text)

    def gathered[value](parsing: Tel.Parsing, elements: List[Any]): value =
      (unwrap(parsing): @unchecked) match
        case gathering: Gathering => gathering.gathered(elements).asInstanceOf[value]

    // ── §19.2 positional assignment support for staged parsers ──
    //
    // Staged parsers are generated into user modules, so the atom phase is
    // reached through public support points carrying neutral references, as
    // the reader and field instances are. `positionalTable` is built once
    // per generated instance (a lazy val); `positionalAssign` runs the atom
    // phase for one entry line, and the two accessors read its per-field
    // result by index — so generated code needs no collection API.

    def positionalTable
      ( keys:      Array[String]^{},
        natures:   Array[Tel.Nature]^{},
        instances: Array[Tel.Field | Null]^{},
        fallbacks: Array[Any]^{} )
    :   AnyRef =

      val count = keys.length
      val profiles = new scala.Array[Positional.Profile](count)
      var index = 0

      while index < count do
        val instance = instances.readUnchecked(index)

        // A primitive field's nature is fixed by its type at expansion; an
        // instance-backed field's comes from the instance itself.
        val nature = if instance == null then natures.readUnchecked(index) else instance.nature
        val repeatable = instance != null && instance.repeatable
        val optional = instance != null && instance.optional

        // As in the derived engines: a Flag field is never required (its
        // absence reads `false`), so the skip rule may pass over it.
        profiles(index) =
          Positional.Profile
            ( Text(keys.readUnchecked(index)),
              nature,
              repeatable,
              required =
                nature != Tel.Nature.Flag
                && !(optional || fallbacks.readUnchecked(index).asInstanceOf[Optional[Any]].present) )

        index += 1

      // The table and assignment travel as neutral carriers: both are fresh,
      // self-contained arrays of pure data, so the rim cast is honest — the
      // same discipline as `takeAtoms` and `Field.Adapter`.
      profiles.asInstanceOf[AnyRef]

    def positionalAssign(table: AnyRef, atoms: Array[Tel.Atom]^{})(using Tactic[TelError])
    :   AnyRef =

      val profiles = table.asInstanceOf[scala.Array[Positional.Profile]]
      val assigned = Positional.assign(atoms, profiles.asInstanceOf[Array[Positional.Profile]^{}])
      val result = new scala.Array[scala.Array[Text]](profiles.length)
      var index = 0

      while index < profiles.length do
        val texts = assigned.readable(index).stdlib
        val slot = new scala.Array[Text](texts.length)
        var occurrence = 0
        var rest = texts

        while rest.nonEmpty do
          slot(occurrence) = Positional.text(rest.head)
          occurrence += 1
          rest = rest.tail

        result(index) = slot
        index += 1

      result.asInstanceOf[AnyRef]

    def positionalCount(assignment: AnyRef, index: Int): Int =
      assignment.asInstanceOf[scala.Array[scala.Array[Text]]](index).length

    def positionalText(assignment: AnyRef, index: Int, occurrence: Int): Text =
      assignment.asInstanceOf[scala.Array[scala.Array[Text]]](index)(occurrence)

    // §20.2 step 5c from a staged parser: a non-repeatable field filled by a
    // positional atom, then again by a same-keyword child.
    def duplicateFill()(using Tactic[TelError]): Unit =
      raise(TelError(TelError.Reason.NonRepeatableTooMany))

    // The byte-parsed primitives' `parseAtom` semantics, for a staged
    // parser's positionally-assigned atom: an unparseable value is
    // `NotScalar` with the offending text, as on both other paths.
    def atomInt(text: Text)(using Tactic[TelError]): Int =
      val parsed = try Optional(text.s.toInt) catch case _: NumberFormatException => Unset
      parsed.or(raise(TelError(TelError.Reason.NotScalar(text, t"Int"))) yet 0)

    def atomLong(text: Text)(using Tactic[TelError]): Long =
      val parsed = try Optional(text.s.toLong) catch case _: NumberFormatException => Unset
      parsed.or(raise(TelError(TelError.Reason.NotScalar(text, t"Long"))) yet 0L)

    def atomBoolean(text: Text)(using Tactic[TelError]): Boolean =
      if text == t"true" then true else if text == t"false" then false
      else raise(TelError(TelError.Reason.NotScalar(text, t"Boolean"))) yet false

    // Field instances travel wrapped in the `Field.Adapter`; the engine
    // looks through it for repeatability and element hooks.
    private def unwrap(parsing: Tel.Parsing): Tel.Parsing = parsing match
      case adapter: Tel.Field.Adapter[?] => adapter.source
      case other                         => other

    // The derived product parser's engine. `fields0` is an explicit thunk
    // (nameable in the capture set, unlike a by-name) evaluated lazily, so
    // recursive derivation can defer sibling resolution; it yields, per
    // field, the wire keyword, the field's parser and its declared default
    // (or `Unset`). The entry loop lives here, in an ordinary method body —
    // no Wisteria per-field lambda ever closes over the reader.
    def product[derivation]
      ( fields0: () => Array[(String, Tel.Parsing, Any)]^{},
        make:    Array[Any]^{} -> derivation )
      ( using foci: Foci[Tel.Focus], tactic: Tactic[TelError] )
    :   ((derivation is Tel.Field)^{fields0, tactic}) =

      new Tel.Field:
        type Self = derivation

        private lazy val fields: Array[(String, Tel.Parsing, Any)]^{} = fields0()
        private lazy val keys: Array[String]^{} = fields.map(_(0))

        // Per-field positional profiles for the §19.2 atom pre-pass,
        // computed from the same table as the keyword dispatch. A Flag
        // field is never required: its absence parses `false`, so the skip
        // rule may pass over it — which the canonical encoder relies on
        // when it elides a false flag from a run.
        private lazy val profiles: Array[Positional.Profile]^{} =
          fields.map: (key, parsing, fallback) =>
            val actual = unwrap(parsing)

            Positional.Profile
              ( Text(key),
                actual.nature,
                actual.repeatable,
                required = actual.nature != Tel.Nature.Flag
                           && !(actual.optional || fallback.asInstanceOf[Optional[Any]].present) )

        def shape(): Morphology =
          val entries: List[(Text, Morphology)] =
            fields.map { (key, parser, _) => (Text(key), parser.shape()) }.to[List]

          Morphology.Obj
            ( entries, entries.collect { case (key, shape) if !shape.optional => key } )

        private def indexOf(keyword: Text): Int =
          val named = keys
          val count = named.length
          val name: String = keyword.s
          var index = 0

          while index < count do
            if named.readUnchecked(index) == name then return index
            index += 1

          -1

        // The value of a record field is its children, one level deeper than
        // its own entry line — after the entry line's own atoms fill fields
        // positionally (§19.2).
        def parse(reader: TelReader^, indent: Int): derivation =
          parseFields(reader, indent + 1, reader.lineAtoms())

        // A whole document's fields sit at indent zero; the root carries no
        // atoms (§20.2).
        override def parse(reader: TelReader^): derivation =
          parseFields(reader, 0, Array.empty)

        private def parseFields(reader: TelReader^, indent: Int, atoms: Array[Tel.Atom]^{})
        :   derivation =

          val entries = fields
          val count = entries.length
          val values = new scala.Array[Any](count)
          val atomFilled = new scala.Array[Boolean](count)
          var index = 0

          while index < count do
            values(index) = AbsentSlot
            index += 1

          // With the inert default `Foci`, per-field `focus` wrapping would
          // observably do nothing, so the hot loop skips it.
          val focused = foci.active

          // §19.2 positional pre-pass (issue #1694): the entry line's atoms
          // fill fields in declaration order before the keyword loop, so a
          // repeatable field's later same-keyword children append after them
          // (§18.3 step 4 — atoms precede children).
          if atoms.length > 0 then
            val assigned = Positional.assign(atoms, profiles)(using tactic)
            var slot = 0

            while slot < assigned.length do
              val slotAtoms = assigned.readUnchecked(slot).stdlib

              if slotAtoms.nonEmpty then
                val parsing = unwrap(entries.readUnchecked(slot)(1))

                inline def positioned[result](inline block: => result): result =
                  focusing(foci, reader, Text(keys.readUnchecked(slot)))(block)

                parsing match
                  case gathering: Gathering if parsing.repeatable =>
                    val buffer = scala.collection.mutable.ListBuffer.empty[Any]

                    slotAtoms.foreach: atom =>
                      buffer += positioned(gathering.parseAtomElement(Positional.text(atom)))

                    values(slot) = buffer

                  case _ =>
                    atomFilled(slot) = true

                    values(slot) =
                      if parsing.nature == Tel.Nature.Flag
                      then positioned(parsing.parseFlag())
                      else positioned(parsing.parseAtom(Positional.text(slotAtoms.head)))

              slot += 1

          var next: Optional[Text] = reader.keyword(indent)

          while next.present do
            val found = indexOf(next.vouch)

            if found < 0 then reader.skipEntry(indent)
            else
              val parsing = unwrap(entries.readUnchecked(found)(1))

              parsing match
                case gathering: Gathering if parsing.repeatable =>
                  // Every occurrence of a repeatable field accumulates, in
                  // document order — the AST's gather-all semantics.
                  val buffer = values(found) match
                    case buffer: scala.collection.mutable.ListBuffer[?] =>
                      buffer.asInstanceOf[scala.collection.mutable.ListBuffer[Any]]

                    case _ =>
                      val buffer = scala.collection.mutable.ListBuffer.empty[Any]
                      values(found) = buffer
                      buffer

                  buffer +=
                    ( if focused
                      then focusing(foci, reader, Text(keys.readUnchecked(found))):
                        gathering.parseElement(reader, indent)
                      else gathering.parseElement(reader, indent) )

                case _ =>
                  // A non-repeatable field keeps its first occurrence — the
                  // AST's `field()` semantics — and skips the rest. When the
                  // first fill came from a positional atom, the duplicate is
                  // §20.2 step 5c's E308 (the atom wins).
                  if !(values(found).asInstanceOf[AnyRef] eq AbsentSlot) then
                    if atomFilled(found)
                    then raise(TelError(TelError.Reason.NonRepeatableTooMany))(using tactic)

                    reader.skipEntry(indent)
                  else values(found) =
                    if focused
                    then focusing(foci, reader, Text(keys.readUnchecked(found))):
                      entries.readUnchecked(found)(1).parse(reader, indent)
                    else entries.readUnchecked(found)(1).parse(reader, indent)

            next = reader.keyword(indent)

          index = 0

          while index < count do
            val parsing = unwrap(entries.readUnchecked(index)(1))

            if parsing.repeatable then
              val elements: List[Any] = values(index) match
                case buffer: scala.collection.mutable.ListBuffer[?] => List.of(buffer.toList)
                case _                                              => Nil

              parsing match
                case gathering: Gathering =>
                  // A repeatable field never consults the declared default:
                  // zero occurrences build the empty collection, as the AST
                  // derivation decodes an empty synthetic document.
                  values(index) =
                    if focused
                    then focus(descend(prior, Text(keys.readUnchecked(index))))(gathering.gathered(elements))
                    else gathering.gathered(elements)

                case _ => ()
            else if values(index).asInstanceOf[AnyRef] eq AbsentSlot then
              val fallback = entries.readUnchecked(index)(2).asInstanceOf[Optional[Any]]

              values(index) =
                if fallback.present then fallback
                else if focused
                then focus(descend(prior, Text(keys.readUnchecked(index))))(entries.readUnchecked(index)(1).absent())
                else entries.readUnchecked(index)(1).absent()

            index += 1

          make(values.asInstanceOf[Array[Any]^{}])

  // The direct-parsing counterpart of `Tel.Decodable`: consumes compound
  // entries from a `TelReader` instead of walking a materialized `Tel`, so
  // `read[value in Tel]` can instantiate values without building the
  // document's AST. `Parsable` is the opt-in surface: explicit instances,
  // `Tel.Parsable.derived`, and the read trigger. It has no blanket fallback
  // given, so no read changes behavior until a type opts in; the fallback
  // belongs to its operational sibling, `Tel.Field`.
  trait Parsable extends Parsing

  object Field:
    // Adapts an opted-in nominal instance (or any other `Parsing`) for use
    // as a field parser. A named class (with the wrapped instance held as a
    // neutral carrier and reasserted at the rim, preserving the declared
    // capture) so the derived product parser can look through it when it
    // needs the underlying instance's repeatability and element hooks.
    private[stratiform] final class Adapter[value](source0: AnyRef) extends Tel.Field:
      type Self = value

      private[stratiform] def source: value is Tel.Parsing =
        source0.asInstanceOf[value is Tel.Parsing]

      def parse(reader: TelReader^, indent: Int): value = source.parse(reader, indent)
      override def parse(reader: TelReader^): value = source.parse(reader)
      def shape(): Morphology = source.shape()
      override def repeatable: Boolean = source.repeatable
      override def nature: Tel.Nature = source.nature
      override def optional: Boolean = source.optional
      override def absent()(using Tactic[TelError]): value = source.absent()
      override def parseAtom(text: Text)(using Tactic[TelError]): value = source.parseAtom(text)
      override def parseFlag()(using Tactic[TelError]): value = source.parseFlag()

    def apply[value](parsing: (value is Tel.Parsing)^)
    :   ((value is Tel.Field)^{parsing}) =

      Adapter[value](parsing.asInstanceOf[AnyRef])
      . asInstanceOf[(value is Tel.Field)^{parsing}]

  // The operational face of direct parsing: how a field's value is read from
  // a `TelReader`, whether directly or through the AST bridge. This is the
  // typeclass the product derivation resolves per field: `Tel2` carries its
  // element-wise instances and `Tel3` the universal fallback — declared as
  // members of this companion (like `Tel2.decodable`) so Wisteria's wrapper
  // detection excludes the fallback during codec probing.
  trait Field extends Parsing

  // Type assignment algorithm per §20.2 of the TEL specification.
  //
  // Document-conformance violations (E301-E311) go through `recoverNode` and
  // so record-and-continue under an accrual boundary, per §19.5; schema
  // errors (E210/E218) abort, because a malformed schema offers no document-
  // level recovery. Out of scope here: §20.1 schema-validity checking (E2xx
  // beyond reference resolution), tabulation-aware column assignment, and
  // diagnostic spans — a TelError raised during assignment carries no
  // position.
  object Type:
    import TelError.Reason
    import Tels.*

    // Record an E2xx/E3xx validation error and continue with `continuation` —
    // the §19.5 `IgnoreErroneousNode` recovery: discard the offending node and
    // keep validating siblings. Under the default `ThrowTactic` `raise` throws,
    // preserving fail-fast validation; under a `validate[Tel.Focus]` boundary's
    // `TrackTactic` it accrues and the continuation supplies the skipped value.
    private inline def recoverNode[value](reason: Reason)(continuation: => value)
      ( using Tactic[TelError], Foci[Tel.Focus] )
    :   value =

      raise(TelError(reason)) yet continuation

    // §20.2 RECOMMENDED defence against pathologically deep documents: type
    // assignment fail-stops beyond this many levels of compound nesting. It
    // carries no E-code because it is a property of the implementation, not
    // of the document.
    private val nestingLimit = 256

    def assign(tel: Tel, schema: Tels): Tel.Element raises TelError tracks Tel.Focus =
      val compounds: Array[Tel.Compound]^{} = tel.subtree.children.bind(_.compounds)
      val rootChildren = assignChildren(compounds, schema.document, schema, 1)

      val rootElements =
        applyConstraints(schema.document, Array.empty[Tel.Element], rootChildren, schema)

      // Locate every focus accrued by the walk against the root, at the
      // *keyword*: schema violations are about a member, not about the text of
      // its value, and an unknown keyword may have no value at all. Validator
      // errors (E310) are registered by the three-argument overload *after*
      // this runs, so they keep the pointer their element supplies but no
      // position.
      Tel.supplementKeyPositions(tel)

      Tel.Element.Node(keywordIndex = Unset, elementType = schema.document, children = rootElements)

    def assign(tel: Tel, schema: Tels, validators: Tel.Validator.Registry)
    :   Tel.Element raises TelError tracks Tel.Focus =

      val element = assign(tel, schema)
      validateElement(element, validators)
      element

    private def validateElement
      ( element: Tel.Element, registry: Tel.Validator.Registry )
    :   Unit raises TelError tracks Tel.Focus =

      element match
        case Tel.Element.Value(_, scalarType, text) =>
          scalarType.validators.each: name =>
            registry(Tel.Validator.Request.Scalar(name, text)) match
              case Tel.Validator.Response.Valid      => ()

              case Tel.Validator.Response.Invalid(_) =>
                recoverNode(Reason.ValidatorRejected)(())

        case Tel.Element.Node(_, elementType, children) =>
          children.each(validateElement(_, registry))

          elementType match
            case s: Struct =>
              s.validators.each: name =>
                registry(Tel.Validator.Request.Struct
                         ( name, element.asInstanceOf[Tel.Element.Node] ))
                match
                  case Tel.Validator.Response.Valid      => ()

                  case Tel.Validator.Response.Invalid(_) =>
                    recoverNode(Reason.ValidatorRejected)(())

            case _ => ()

    private def resolveType(t: Type, schema: Tels): Type raises TelError =
      t match
        case Reference(name) =>
          schema.records.find(_.name == name) match
            case Some(record) => Struct(record.members, record.validators)

            case None =>
              schema.scalars.find(_.name == name) match
                case Some(scalarDef) => Scalar(scalarDef.validators)

                case None =>
                  schema.selects.find(_.name == name) match
                    case Some(_) => abort(TelError(Reason.ReferenceKindMismatch))
                    case None    => abort(TelError(Reason.UnresolvedReference))

        case other => other

    private case class KeywordEntry
      ( flatIndex: Int,
        ordinal:   Int,
        entryType: Type,
        member:    Member,
        variant:   Optional[Variant] = Unset )

    // Builds a map from keyword Text → KeywordEntry where `flatIndex`
    // is the **flat keyword index** per BinTEL §5: each Field
    // contributes 1 entry, each SelectRef contributes 1 entry per
    // variant in the referenced SelectDefinition. The flat index
    // identifies the unique keyword position in the parent's flat-
    // keyword sequence — used as the `keywordIndex` of every
    // Tel.Element produced from this parent. `ordinal` is the member
    // index in `parent.members`: all variants of one SelectRef share
    // it, which is what lets them interleave under the §20.2 step 4c
    // contiguity rule.
    private def keywordMap(parent: Struct, schema: Tels)
    :   Map[Text, KeywordEntry] raises TelError =

      val builder = scala.collection.mutable.LinkedHashMap.empty[Text, KeywordEntry]
      var idx = 0
      var flatIdx = 0

      while idx < parent.members.length do
        parent.members.readUnchecked(idx) match
          case f: Tels.Field =>
            builder(f.keyword) = KeywordEntry(flatIdx, idx, f.fieldType, f)
            flatIdx += 1

          case s: SelectRef =>
            val selectDef = schema.selects.find(_.name == s.reference).getOrElse:
              abort(TelError(Reason.UnresolvedReference))

            var v = 0

            while v < selectDef.variants.length do
              val variant = selectDef.variants.readUnchecked(v)

              builder(variant.keyword) =
                KeywordEntry(flatIdx + v, idx, variant.variantType, s, Optional(variant))

              v += 1

            flatIdx += selectDef.variants.length

          case _: Exclude => ()

        idx += 1

      Map.of(builder.toMap)

    private def atomAssignable(member: Member, schema: Tels): Boolean raises TelError =
      member match
        case f: Tels.Field =>
          resolveType(f.fieldType, schema) match
            case _: Scalar => true
            case Flag      => true
            case _         => false

        case s: SelectRef =>
          val selectDef = schema.selects.find(_.name == s.reference).getOrElse:
            abort(TelError(Reason.UnresolvedReference))

          selectDef.variants.forall: v =>
            resolveType(v.variantType, schema) match
              case Flag => true
              case _    => false

        case _: Exclude => false

    private def selectDefinitionOf(select: SelectRef, schema: Tels)
    :   SelectDefinition raises TelError =

      schema.selects.find(_.name == select.reference).getOrElse:
        abort(TelError(Reason.UnresolvedReference))

    // Effective polarity per §20: `required` unless declared `Loose`.
    private def requiredOf(member: Member): Boolean = member match
      case f: Tels.Field => f.required != Polarity.Loose
      case s: SelectRef  => s.required != Polarity.Loose
      case _: Exclude    => false

    private def repeatableOf(member: Member): Boolean = member match
      case f: Tels.Field => f.repeatable == Polarity.Loose
      case s: SelectRef  => s.repeatable == Polarity.Loose
      case _: Exclude    => false

    // Flag-shaped per §20.2 step 3a: a Field resolving to Flag, or a
    // SelectRef all of whose variants resolve to Flag.
    private def flagShaped(member: Member, schema: Tels): Boolean raises TelError =
      member match
        case f: Tels.Field =>
          resolveType(f.fieldType, schema) match
            case Flag => true
            case _    => false

        case s: SelectRef => atomAssignable(s, schema)
        case _: Exclude   => false

    private def keywordMatches(member: Member, text: Text, schema: Tels)
    :   Boolean raises TelError =

      member match
        case f: Tels.Field => f.keyword == text
        case s: SelectRef  => selectDefinitionOf(s, schema).variants.exists(_.keyword == text)
        case _: Exclude    => false

    // Track both the member position (`pos` in parent.members) and the
    // running flat keyword index (`flatPos`) — Tel.Element.keywordIndex
    // uses flat positions per BinTEL §5.
    private def assignAtoms
      ( atoms:  Array[Tel.Atom]^{},
       parent: Struct,
       schema: Tels )
    :   Array[Tel.Element]^{} raises TelError tracks Tel.Focus =

      val results = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      var pos = 0
      var flatPos = 0
      var i = 0

      def flatWidthOf(member: Member): Int = member match
        case _: Tels.Field => 1
        case _: Exclude    => 0

        case s: SelectRef =>
          schema.selects.find(_.name == s.reference) match
            case Some(selectDef) => selectDef.variants.length
            case None            => 0

      while i < atoms.length do
        val atomText = atoms.readUnchecked(i) match
          case Tel.Atom.Inline(t, _)  => t
          case Tel.Atom.Source(t)     => t
          case Tel.Atom.Literal(_, t) => t

        // §20.2 step 3a: advance past non-required members that cannot take
        // this atom — those that are not atom-assignable, or are Flag-shaped
        // with a keyword the atom's text does not match. A Scalar member is
        // never skipped; a required member is never skipped.
        var scanning = true

        while scanning && pos < parent.members.length do
          val member = parent.members.readUnchecked(pos)

          val skippable = member match
            case _: Exclude => true

            case _ =>
              !requiredOf(member)
              && (!atomAssignable(member, schema)
                  || (flagShaped(member, schema) && !keywordMatches(member, atomText, schema)))

          if skippable then
            flatPos += flatWidthOf(member)
            pos += 1
          else scanning = false

        if pos >= parent.members.length then
          // §20.2 step 3b: more atoms than assignable member positions.
          // Recovery drops the whole excess run, reported once.
          recoverNode(Reason.TooManyAtoms)(())
          i = atoms.length
        else
          if !atomAssignable(parent.members.readUnchecked(pos), schema) then
            // §20.2 step 3c: an atom arrived at a required member that can
            // only be filled by compound children. Recovery drops the atom;
            // the member may still be filled by a child.
            recoverNode(Reason.AtomAtNonAssignablePos)(())
          else
            parent.members.readUnchecked(pos) match
              case f: Tels.Field =>
                resolveType(f.fieldType, schema) match
                  case s: Scalar =>
                    results += Tel.Element.Value(flatPos, s, atomText)

                    if f.repeatable != Polarity.Loose then
                      flatPos += 1
                      pos += 1

                  case Flag =>
                    if atomText == f.keyword then
                      results += Tel.Element.Node(flatPos, Flag, Array.empty)

                      if f.repeatable != Polarity.Loose then
                        flatPos += 1
                        pos += 1
                    else
                      // §20.2 step 3d: a required Flag member's atom must
                      // match its keyword. Recovery drops the atom.
                      recoverNode(Reason.AtomFlagKeywordMismatch)(())

                  case _ => () // unreachable behind `atomAssignable`

              case s: SelectRef =>
                val selectDef = selectDefinitionOf(s, schema)

                selectDef.variants.readable.zipWithIndex.find(_._1.keyword == atomText) match
                  case Some((_, variantOffset)) =>
                    results += Tel.Element.Node(flatPos + variantOffset, Flag, Array.empty)

                    if s.repeatable != Polarity.Loose then
                      flatPos += selectDef.variants.length
                      pos += 1

                  case None =>
                    // §20.2 step 3d: no variant keyword matches the atom at a
                    // required all-Flag SelectRef. Recovery drops the atom.
                    recoverNode(Reason.AtomVariantUnmatched)(())

              case _: Exclude => () // consumed by the skip scan

          i += 1

      Array.from(results)

    private def assignChildren
      ( compounds: Array[Tel.Compound]^{},
       parent:    Struct,
       schema:    Tels,
       depth:     Int )
    :   Array[Tel.Element]^{} raises TelError tracks Tel.Focus =

      val km = keywordMap(parent, schema)
      val results = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      var currentMember = -1
      val seenMembers = scala.collection.mutable.HashSet.empty[Int]
      var i = 0

      while i < compounds.length do
        val compound = compounds.readUnchecked(i)

        // Tag every error accrued for this compound (and its descendants) with
        // its keyword path — mirroring the decode derivation's per-field `focus`
        // — so that under a `validate[Tel.Focus]` boundary schema-validation
        // errors carry a pointer (and, for a tracked root, a source position via
        // `Focus.withPosition`) instead of the bare document root.
        focus({
          val base = prior.let(_.pointer).or(TelPath.Root)
          Tel.Focus(base.prepend(compound.keyword))
        }):
          // An unrecognised keyword is skipped (`IgnoreErroneousNode`): record it and
          // emit no element, so remaining siblings are still validated.
          km.get(compound.keyword) match
            case Some(entry) =>
              // §20.2 step 4c: all children of one member must form a single
              // contiguous run; variants of one SelectRef share an ordinal and
              // so interleave freely. Returning to an earlier member's run is
              // E309, reported once per returned-to run; the child is still
              // assigned.
              if entry.ordinal != currentMember then
                if currentMember >= 0 then seenMembers += currentMember

                if seenMembers.contains(entry.ordinal)
                then recoverNode(Reason.MembersNonContiguous)(())

                currentMember = entry.ordinal

              results += assignCompound(compound, entry, schema, depth)

            case None => recoverNode(Reason.UnknownKeyword)(())

        i += 1

      Array.from(results)

    private def applyConstraints
      ( parent:        Struct,
        atomElements:  Array[Tel.Element]^{},
        childElements: Array[Tel.Element]^{},
        schema:        Tels )
    :   Array[Tel.Element]^{} raises TelError tracks Tel.Focus =

      val results = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      results ++= atomElements.readable
      results ++= childElements.readable

      def flatWidth(member: Member): Int = member match
        case _: Tels.Field => 1
        case _: Exclude    => 0

        case s: SelectRef =>
          schema.selects.find(_.name == s.reference) match
            case Some(sd) => sd.variants.length
            case None     => 0

      def indexOf(element: Tel.Element): Int = element match
        case Tel.Element.Node(idx, _, _)  => idx.or(-1)
        case Tel.Element.Value(idx, _, _) => idx

      var memberIdx = 0
      var flatStart = 0

      while memberIdx < parent.members.length do
        val member = parent.members.readUnchecked(memberIdx)
        val width = flatWidth(member)

        // §20.2 step 5a: atoms and compound children assigned to the member
        // count against the same budget. A member's elements all carry flat
        // keyword indexes within its flat range.
        val fillCount = results.count: element =>
          val idx = indexOf(element)
          idx >= flatStart && idx < flatStart + width

        member match
          case f: Tels.Field =>
            // A missing required member is reported under its own keyword path.
            // It has no compound in the source, so `withPosition` leaves the
            // position Unset (locate finds no node) — but the pointer still names
            // the absent field.
            if requiredOf(f) && fillCount == 0 then focus({
              val base = prior.let(_.pointer).or(TelPath.Root)
              Tel.Focus(base.prepend(f.keyword))
            }):
              resolveType(f.fieldType, schema) match
                case s: Scalar => f.default match
                  case t: Text => results += Tel.Element.Value(flatStart, s, t)
                  case _       => recoverNode(Reason.RequiredMemberAbsent)(())

                case _ => recoverNode(Reason.RequiredMemberAbsent)(())

            // §20.2 step 5c; recovery reports the document invalid but
            // retains every occurrence.
            if !repeatableOf(f) && fillCount > 1
            then recoverNode(Reason.NonRepeatableTooMany)(())

          case s: SelectRef =>
            // §20.2 step 5b: defaults exist only on Scalar Fields, so an
            // absent required SelectRef is always E307.
            if requiredOf(s) && fillCount == 0
            then recoverNode(Reason.RequiredMemberAbsent)(())

            if !repeatableOf(s) && fillCount > 1
            then recoverNode(Reason.NonRepeatableTooMany)(())

          case _: Exclude => ()

        flatStart += width
        memberIdx += 1

      Array.from(results)

    private def assignCompound
      ( compound: Tel.Compound,
       entry:    KeywordEntry,
       schema:   Tels,
       depth:    Int )
    :   Tel.Element raises TelError tracks Tel.Focus =

      if depth > nestingLimit then abort(TelError(Reason.NestingLimitExceeded))

      val resolved = resolveType(entry.entryType, schema)

      resolved match
        case s: Struct =>
          val atomElements = assignAtoms(compound.atoms, s, schema)
          val childCompounds: Array[Tel.Compound]^{} = compound.children.bind(_.compounds)
          val childElements = assignChildren(childCompounds, s, schema, depth + 1)
          val allElements = applyConstraints(s, atomElements, childElements, schema)
          Tel.Element.Node(entry.flatIndex, s, allElements)

        case s: Scalar =>
          // §20.2 step 1: a Scalar-typed compound is a leaf with at most one
          // atom (of any presentation form) and no compound children. Extra
          // atoms are E302 and children are E301; recovery keeps the first
          // atom and ignores the children.
          if compound.atoms.length > 1 then recoverNode(Reason.TooManyAtoms)(())

          if compound.children.exists(_.compounds.nonEmpty)
          then recoverNode(Reason.NonStructCompound)(())

          val text = compound.atoms.headOption.map:
            case Tel.Atom.Inline(t, _)  => t
            case Tel.Atom.Source(t)     => t
            case Tel.Atom.Literal(_, t) => t

          .getOrElse(t"")

          Tel.Element.Value(entry.flatIndex, s, text)

        case Flag =>
          if compound.atoms.nonEmpty || compound.children.nonEmpty then
            recoverNode(Reason.FlagWithContent)(())

          Tel.Element.Node(entry.flatIndex, Flag, Array.empty)

        case _: Reference =>
          recoverNode(Reason.UnresolvedReference):
            Tel.Element.Node(entry.flatIndex, Flag, Array.empty)

  // Validator infrastructure per §21 of the TEL specification.
  object Validator:

    enum Request:
      case Scalar(method: Text, value: Text)
      case Struct(method: Text, element: Tel.Element.Node)

    enum Diagnostic:
      case Scalar
        ( message: Text,
          span:    Optional[(Int, Int)] = Unset )

      case Struct
        ( message: Text,
          fields:  Map[Text, Diagnostic] = Map.empty )

    enum Response:
      case Valid
      case Invalid(diagnostic: Diagnostic)

    object Registry:
      val builtins: Registry = new Registry:
        override def apply(request: Request): Response = request match
          case Request.Scalar(method, value) => method.s match
            case "string"     => Response.Valid
            case "identifier" => identifier(value)
            case "type-name"  => typeName(value)
            case "sigil"      => sigilCheck(value)
            case _            => unknown(method)

          case Request.Struct(method, _) =>
            Response.Invalid(Diagnostic.Struct(
              t"validator '${method}' not applicable to struct values"))

      def withFallback(custom: Registry): Registry = new Registry:
        override def apply(request: Request): Response =
          custom(request) match
            case Response.Valid                                       => Response.Valid
            case Response.Invalid(d) if isUnknown(d)                  => builtins(request)
            case other                                                => other

      private def isUnknown(d: Diagnostic): Boolean = d match
        case Diagnostic.Scalar(m, _) => m.s.startsWith("unknown validator")
        case _                       => false

      private def identifier(value: Text): Response =
        val s = value.s

        if s.isEmpty then fail(t"the identifier must not be empty", (0, 0))
        else if s.startsWith("-") then fail(t"the identifier must not begin with a hyphen", (0, 1))
        else if s.endsWith("-") then fail(t"the identifier must not end with a hyphen",
          (s.length - 1, s.length))
        else if s.contains("--") then fail(t"the identifier must not contain consecutive hyphens",
          (s.indexOf("--"), s.indexOf("--") + 2))
        else
          var i = 0

          while i < s.length do
            val c = s.charAt(i)

            if !(c == '-' || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')) then
              return fail
                ( t"identifier character '$c' must be lowercase ASCII letter, digit, or hyphen",
                  (i, i + 1) )

            i += 1

          Response.Valid

      private def typeName(value: Text): Response =
        val s = value.s

        if s.isEmpty then fail(t"the type name must not be empty", (0, 0))
        else
          val first = s.charAt(0)

          if !(first >= 'A' && first <= 'Z') then
            fail(t"the type name must start with an uppercase ASCII letter", (0, 1))
          else
            var i = 1

            while i < s.length do
              val c = s.charAt(i)

              if !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) then
                return fail(t"type-name character '$c' must be ASCII alphanumeric", (i, i + 1))

              i += 1

            Response.Valid

      private def sigilCheck(value: Text): Response =
        val s = value.s

        if s.length != 1 then fail(t"the sigil must be a single character", (0, s.length))
        else
          val c = s.charAt(0)

          if c == ' ' || c == '\n' || c == '\r' || c == '\t' then
            fail(t"the sigil must not be whitespace", (0, 1))
          else if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then
            fail(t"the sigil must not be a letter or digit", (0, 1))
          else if "()[]{}<>".indexOf(c.toInt) >= 0 then
            fail(t"the sigil must not be a parenthetical symbol", (0, 1))
          else
            Response.Valid

      private def unknown(method: Text): Response =
        Response.Invalid(Diagnostic.Scalar(t"unknown validator '${method}'"))

      private def fail(message: Text, span: (Int, Int)): Response =
        Response.Invalid(Diagnostic.Scalar(message, span))

    trait Registry:
      def apply(request: Request): Response

  object Element:
    case class Node
      ( keywordIndex: Optional[Int],
        elementType:  Tels.Type,
        children:     Array[Element]^{} )
    extends Element

    case class Value
      ( keywordIndex: Int,
        scalarType:   Tels.Scalar,
        text:         Text )
    extends Element

  // Semantic model from §18.2 of the TEL specification — the result
  // of applying type assignment (§20.2) to the presentation model.
  sealed trait Element

  object Pointer:
    case class Step(keyword: Text, index: Optional[Int])

    val Empty: Pointer = Pointer(Array.empty)

    def of(keywords: Text*): Pointer =
      Pointer(Array.from(keywords.map(Step(_, Unset))))

  // Logical address into a Tel document. A pointer is an ordered
  // sequence of `Step`s; each step selects a child compound from the
  // current node by keyword. When several siblings share a keyword
  // the optional `index` disambiguates them — `Unset` means "the
  // first" (index 0).
  case class Pointer(steps: Array[Pointer.Step]^{}):
    def / (keyword: Text): Pointer =
      Pointer(steps :+ Pointer.Step(keyword, Unset))

    def / (keyword: Text, index: Int): Pointer =
      Pointer(steps :+ Pointer.Step(keyword, index))

    def isEmpty: Boolean = steps.length == 0

  enum LineEndings:
    case Lf, Crlf

  case class Pragma
    ( version: (Int, Int), schema: Optional[Text], sigil: Optional[Char] )

  // Document-level prologue carried alongside a `Tel` value when it is
  // loaded via `text.load[Tel]`. The `Document[Tel]` pair lets callers
  // read the schema identifier, interpreter directive, and chosen line
  // endings without inspecting the presentation AST directly.
  case class Metadata
    ( interpreterDirective: Optional[Text],
      pragma:               Optional[Pragma],
      lineEndings:          LineEndings )

  sealed trait Subtree:
    def children: Array[Block]^{}

  case class Document
    ( interpreterDirective: Optional[Text],
      pragma:               Optional[Pragma],
      lineEndings:          LineEndings,
      children:             Array[Block]^{} )
  extends Subtree

  case class Block
    ( comments:           Array[Comment]^{},
      tabulation:         Optional[Tabulation],
      compounds:          Array[Compound]^{},
      trailingBlankLines: Int )

  case class Comment(text: Text)

  case class Tabulation(markerOffsets: Array[Int]^{}, headings: Array[Text]^{})

  case class Compound
    ( keyword:  Text,
      atoms:    Array[Atom]^{},
      remark:   Optional[Text],
      children: Array[Block]^{} )
  extends Subtree

  object Atom:
    object Inline:
      def apply(text: Text, precedingSpaces: Int): Inline =
        new Inline(null, 0, 0, text.s, precedingSpaces)

      // Parser entry: references a slice of the per-parse atom arena. The
      // arena array is shared with sibling atoms committed before the
      // arena's next growth event. UTF-8 decode is deferred until
      // `.text` is first accessed.
      private[stratiform] def fromArena
        ( arena: scala.Array[Byte], off: Int, len: Int, precedingSpaces: Int )
      :   Inline =

        // The arena slice is committed before any subsequent arena mutation, so the
        // atom never observes writes through the shared array.
        scala.caps.unsafe.unsafeAssumePure(new Inline(arena, off, len, null, precedingSpaces))

      def unapply(i: Inline): (Text, Int) = (i.text, i.precedingSpaces)

    // Inline is a regular class (not a case class) so its `text` can be
    // materialised lazily. The parser writes each atom's UTF-8 bytes into
    // a shared per-parse "arena" (a growing byte buffer); each Inline
    // remembers the slice as `(arenaBytes, byteOff, byteLen)`. The
    // String allocation and UTF-8 decode only happen on first `.text`
    // access — consumers that never inspect an atom (parser-throughput
    // benchmarks, partial decoders) skip the per-atom String allocation
    // entirely. The arena array is not aliased with the parser's
    // streaming buffer, so the atom's lifetime is independent of the
    // Cursor it was parsed from. When the arena needs to grow the parser
    // allocates a fresh array and leaves the old one alive via the
    // Inlines that point at it; multiple arena arrays may therefore be
    // kept alive across a parse, but no per-atom byte[] is allocated.
    // Apply / unapply preserve the previous case-class construction and
    // pattern-match syntax; equality and hashCode are derived manually
    // to match the prior structural behaviour.
    final class Inline private[stratiform]
      ( private val bytes:           scala.Array[Byte] | Null,
        private val byteOff:         Int,
        private val byteLen:         Int,
        @scala.caps.unsafe.untrackedCaptures
        private var _text:           String | Null,
        val precedingSpaces:         Int )
    extends Atom:

      def text: Text =
        val t = _text

        if t == null then
          val b = bytes.nn
          val s = new String(b, byteOff, byteLen, java.nio.charset.StandardCharsets.UTF_8)
          _text = s
          Text(s)
        else
          Text(t)

      override def equals(other: Any): Boolean = other match
        case o: Inline => text == o.text && precedingSpaces == o.precedingSpaces
        case _         => false

      override def hashCode: Int = text.hashCode * 31 + precedingSpaces

      override def toString: String = s"Inline($text,$precedingSpaces)"

    case class Source(text: Text)                       extends Atom
    case class Literal(delimiter: Text, text: Text)     extends Atom

  sealed trait Atom

  // Parse a byte stream into a Tel value wrapping the document. Used
  // internally by the Aggregable and Loadable typeclasses; user code
  // should prefer `bytes.read[Tel]` or `text.load[Tel]`.
  private[stratiform] def parse(bytes: Data): Tel raises TelError =
    Tel(Tel.Parser.parse(bytes))

  // Schema-aware parse: the parser uses the §19.5 schema-aware E107
  // recovery rule when it encounters an odd-indented line. Useful
  // when the consumer wants tolerant parsing of indentation typos
  // against a known schema.
  private[stratiform] def parse(bytes: Data, schema: Tels): Tel raises TelError =
    Tel(Tel.Parser.parse(bytes, schema))

  // Parse a multi-document source (§6.1) into its sequence of documents.
  // `parseAll` is eager; `parseStream` parses lazily on demand. Used internally
  // by the collection Aggregable typeclasses; user code should prefer
  // `bytes.read[List[Tel]]` or `bytes.read[Chain[Tel]]`.
  private[stratiform] def parseAll(bytes: Data): List[Tel] raises TelError =
    Tel.Parser.parseDocuments(bytes).map(Tel(_))

  private[stratiform] def parseStream(bytes: Data): Chain[Tel] raises TelError =
    Tel.Parser.parseStream(bytes).map(Tel(_))

  // ── Position tracking (editor / tooling support) ──────────────────────────
  //
  // A flat `Array[Int]^{}` of position descriptors produced alongside the
  // presentation tree by `Tel.parseTracked`. All internal references are stored
  // as offsets relative to the start of the containing node descriptor, so any
  // slice taken at a descriptor boundary is itself a valid `PositionIndex` —
  // mirrors `jacinta.Json.PositionIndex` and `ypsiloid.Yaml.PositionIndex`.
  //
  // Layout per node descriptor at `offset`:
  //   data(offset)              size of this descriptor (including children)
  //   data(offset + 1)          1-indexed source line of the node's keyword
  //   data(offset + 2)          1-indexed source column of the node's keyword
  //   data(offset + 3)          length in characters of the keyword
  //   data(offset + 4)          1-indexed column of the first inline atom, or 0 for none
  //   data(offset + 5)          length in characters of the inline-atom run, or 0
  //   data(offset + 6)          child count
  //   data(offset + 7 + k)      relative offset (from `offset`) of the k-th child
  //   … child descriptors laid out contiguously …
  // The document root occupies the outermost descriptor with a synthetic
  // (line 1, column 1, length 0) header; its children are the top-level compounds.
  //
  // The value fields locate the compound's inline atoms — the text a decode error
  // is about — so `locate` can point at the value while `locateKey` points at the
  // keyword. No value *line* is stored: inline atoms are by construction on the
  // compound's own line, so it is always `data(offset + 1)`. A column of 0 means
  // the compound has no inline atoms at all (a parent, a flag, a remark-only line,
  // or a source/literal payload, whose lines are not spannable by a `Line`-mode
  // `Span`); those fall back to the keyword. Columns being 1-indexed makes 0
  // unambiguous.
  //
  // Represented as the stdlib's immutable array (pure by construction): the frozen
  // `Array[Int]^{}` form makes every `PositionIndex`-holding field carry a fresh `any.rd`.
  opaque type PositionIndex = scala.IArray[Int]

  // Ints per compound in the parser's flat pre-order record stream, and the fixed
  // part of a descriptor's header. Named once so the parser's append site and
  // `buildIndex`'s fold cannot drift apart.
  private[stratiform] inline val positionStride = 5
  private[stratiform] inline val descriptorHeader = 7

  object PositionIndex:
    private[stratiform] def apply(data: Array[Int]^{}): PositionIndex = data.readable

  extension (positionIndex: PositionIndex)
    private[stratiform] def ints: Array[Int]^{} = Array.frozen(positionIndex)

  // Parse `bytes` into a `Tel` carrying a `PositionIndex`, so that
  // `tel.locate(pointer)` / `tel.locateKey(pointer)` resolve a node's keyword
  // path to its source `Position`, and accrued `Tel.Focus`es can be located via
  // `withPosition`. Reached from the `aggregable`, `aggregableIn` and `loadable`
  // givens only when `parsing.trackPositions` is in scope; otherwise the
  // untracked `parse` runs and `positionIndex` is left Unset, so the throughput
  // path is unaffected.
  private def parseTracked(bytes: Data): Tel raises TelError =
    val (document, triples) = Tel.Parser.parseTracked(bytes)
    Tel(document, Optional(PositionIndex(buildIndex(document, triples))))

  // Parse `bytes` honouring the in-scope `Tracking` toggle (`parsing.trackPositions`).
  private[stratiform] def parseTracking(bytes: Data)(using PositionTracking): Tel raises TelError =
    summon[PositionTracking] match
      case PositionTracking.On  => parseTracked(bytes)
      case PositionTracking.Off => parse(bytes)

  // Fold the parser's flat pre-order stream of `positionStride`-int records — one
  // per compound, in recursive-descent order — into the navigable packed
  // descriptor layout documented on `PositionIndex`. The AST supplies the tree
  // shape; the records supply the coordinates. Runs only under `parseTracked`,
  // never on the hot path.
  private[stratiform] def buildIndex(document: Tel.Document, records: Array[Int]^{})
  :   Array[Int]^{} =
    var cursor = 0

    def build
      ( node:        Tel.Subtree,
        line:        Int,
        column:      Int,
        length:      Int,
        valueColumn: Int,
        valueLength: Int )
    :   scala.Array[Int] =

      val children = node.children.bind(_.compounds)
      val childDescriptors = new scala.Array[scala.Array[Int]](children.length)
      var k = 0

      while k < children.length do
        val childLine        = records.readUnchecked(cursor)
        val childColumn      = records.readUnchecked(cursor + 1)
        val childLength      = records.readUnchecked(cursor + 2)
        val childValueColumn = records.readUnchecked(cursor + 3)
        val childValueLength = records.readUnchecked(cursor + 4)
        cursor += positionStride

        childDescriptors(k) =
          build
           ( children.readUnchecked(k),
             childLine,
             childColumn,
             childLength,
             childValueColumn,
             childValueLength )

        k += 1

      val header = descriptorHeader + children.length
      var total = header
      k = 0

      while k < children.length do
        total += childDescriptors(k).length
        k += 1

      val buffer = new scala.Array[Int](total)
      buffer(0) = total
      buffer(1) = line
      buffer(2) = column
      buffer(3) = length
      buffer(4) = valueColumn
      buffer(5) = valueLength
      buffer(6) = children.length
      var offset = header
      k = 0

      while k < children.length do
        buffer(descriptorHeader + k) = offset
        System.arraycopy(childDescriptors(k), 0, buffer, offset, childDescriptors(k).length)
        offset += childDescriptors(k).length
        k += 1

      buffer

    Array.frozen(scala.IArray.unsafeFromArray(build(document, 1, 1, 0, 0, 0)))

  // Resolves a `TelPath` to the source `Position` recorded in a tracked `Tel`'s
  // `PositionIndex`. Exposed uniformly as `tel.locate(path)` / `tel.locateKey(path)`
  // through zephyrine's `Positionable`, matching `jacinta.Json` and `ypsiloid.Yaml`:
  // `locate` gives the value — the compound's inline atoms, falling back to the
  // keyword when it has none — and `locateKey` gives the keyword.
  given positionable: Tel is Positionable by TelPath to TelError.Position =
    new Positionable:
      type Self    = Tel
      type Operand = TelPath
      type Result  = TelError.Position

      def locate(value: Tel, path: TelPath): Optional[TelError.Position] =
        value.positionIndex.let: index =>
          walkIndex(value.subtree, index.ints, 0, Sequence.from(path.keywords.stdlib), 0, false)

      def locateKey(value: Tel, path: TelPath): Optional[TelError.Position] =
        value.positionIndex.let: index =>
          walkIndex(value.subtree, index.ints, 0, Sequence.from(path.keywords.stdlib), 0, true)

  // Walk the packed `PositionIndex` alongside the AST, following `segments`
  // (a root-first keyword path) from the descriptor at `offset`.
  //
  // TEL has no separate key and value *nodes* — a compound is one node — but it
  // does have two distinct regions of source text, and a diagnostic wants
  // whichever the message is about. `keyMode` picks: the keyword for
  // `locateKey`, the inline-atom run for `locate`, matching what those two mean
  // in `jacinta.Json` and `ypsiloid.Yaml`. A compound with no inline atoms — a
  // parent, a flag, a remark-only line, or one whose value is a source or
  // literal payload on later lines, which a `Line`-mode `Span` cannot express —
  // has no value region, and falls back to its keyword. The root has no keyword,
  // so `locateKey` yields `Unset` there.
  private def walkIndex
    ( node:     Tel.Subtree,
      data:     Array[Int]^{},
      offset:   Int,
      segments: Sequence[Text],
      i:        Int,
      keyMode:  Boolean )
  :   Optional[TelError.Position] =

    if i >= segments.length then
      if keyMode && i == 0 then Unset
      else
        val valueColumn = data.readUnchecked(offset + 4)

        if keyMode || valueColumn == 0
        then TelError.Position
              ( line   = data.readUnchecked(offset + 1),
                column = data.readUnchecked(offset + 2),
                length = Optional(data.readUnchecked(offset + 3)) )
        else TelError.Position
              ( line   = data.readUnchecked(offset + 1),
                column = valueColumn,
                length = Optional(data.readUnchecked(offset + 5)) )
    else
      val children = node.children.bind(_.compounds)
      val k = children.indexWhere { child => segments(denominative.Ordinal.zerary(i)).lay(false)(_ == child.keyword) }

      if k < 0 then Unset
      else
        val child = offset + data.readUnchecked(offset + descriptorHeader + k)
        walkIndex(children.readUnchecked(k), data, child, segments, i + 1, keyMode)

  // Concatenate the chunks of a `Chain[Data]` source into a single byte array.
  private[stratiform] def concatenate(source: Chain[Data]): Data =
    import denominative.nil

    // A single in-memory block — the common case — is returned as-is
    // rather than copied into a fresh array (jacinta's single-chunk fast
    // path; the copy dominated the entry cost of fast direct reads).
    if !source.nil && source.tail.nil then source.head else
      var acc    = Array.empty[Byte]
      var stream = source

      while !stream.nil do
        acc = acc ++ stream.head
        stream = stream.tail

      acc

  // `bytes.read[Tel]` for any Chain[Data] source: concatenates the
  // chunks and parses the result. The metadata (interpreter directive,
  // pragma, line-endings) is *not* surfaced — use `.load[Tel]` to
  // recover those alongside the value. Per §6.1, single-document parsing
  // stops at the first document separator; content after it is ignored.
  given aggregable: (tactic: Tactic[TelError], tracking: PositionTracking)
  =>  ((Tel is Aggregable by Data)^{tactic}) =
    source => parseTracking(concatenate(source))

  // Direct parsing: when the value knows how to consume compound entries
  // itself, the document's AST is never materialized. Declared here (not in
  // `Tel2`, where the `Decodable`-based `aggregableIn` lives) so it wins
  // whenever a `Tel.Parsable` exists, and is otherwise inapplicable —
  // existing code resolves exactly as before.
  given aggregableParsed: [value]
  =>  ( parsable: (value is Tel.Parsable)^ )
  =>  ( tactic: Tactic[TelError], tracking: PositionTracking )
  =>  ( ((value in Tel) is Aggregable by Data)^{parsable, tactic} ) =
    source => parseDirect(concatenate(source), parsable).asInstanceOf[value in Tel]

  // Whole-`Data` direct read: when the entire content is already in hand,
  // parse it in place rather than wrapping it in a one-element stream —
  // jacinta's `readableParsed` precedent. Concrete in `Data`, so it beats
  // the composed pipeline by specificity.
  given readableParsed: [value]
  =>  ( parsable: (value is Tel.Parsable)^ )
  =>  ( tactic: Tactic[TelError], tracking: PositionTracking )
  =>  ( (Data is Readable to (value in Tel))^{parsable, tactic} ) =
    data => parseDirect(data, parsable).asInstanceOf[value in Tel]

  // Direct-parsing counterpart of `parse`: drives a `Tel.Parsable` instance
  // over the input through a `TelReader`, so no document AST is built for the
  // values the instance reads directly.
  //
  // There is no `PositionIndex` here — the result is the caller's value, not a
  // `Tel`, so there is nothing to resolve a pointer against once the parse is
  // over. `parsing.trackPositions` instead turns on the line scan's extent
  // measurement, and each focus and error takes its span from the parser as it
  // goes. Without it the parse is exactly what it always was.
  private def parseDirect[value](input: Data, parsable: (value is Tel.Parsable)^)
    ( using tactic: Tactic[TelError], tracking: PositionTracking )
  :   value =

    import zephyrine.Lineation.untrackedData
    val parser = Tel.Parser.borrow()
    parser.reset(Cursor[Data](input), Unset)

    parser.spanTracking = tracking match
      case PositionTracking.On  => true
      case PositionTracking.Off => false

    parser.directProlog()
    parsable.parse(TelReader(parser, tactic))

  // Direct-parsing primitives, mirroring the `Tel2` primitive decodables
  // exactly: a missing atom is `Absent` and an unparseable one is
  // `NotScalar`, each raised (not aborted) with the same sentinel
  // continuation as `primitiveFault`, so per-field accrual under a
  // `validate[Tel.Focus]` boundary behaves identically on both paths.
  // Genuinely pure — parse-time raising happens through the tactic the
  // `TelReader` carries.
  private def primitiveParsable[value](shape0: Morphology, expected: Text, sentinel: value)
    ( convert: Text -> Optional[value] )
  :   value is Tel.Parsable =

    new Tel.Parsable:
      type Self = value
      def shape(): Morphology = shape0
      override def nature: Tel.Nature = Tel.Nature.Scalar

      def parse(reader: TelReader^, indent: Int): value =
        reader.atom().lay(reader.fault(TelError.Reason.Absent) yet sentinel): atom =>
          convert(atom).or:
            reader.fault(TelError.Reason.NotScalar(atom, expected)) yet sentinel

      override def absent()(using Tactic[TelError]): value =
        raise(TelError(TelError.Reason.Absent)) yet sentinel

      override def parseAtom(text: Text)(using Tactic[TelError]): value =
        convert(text).or:
          raise(TelError(TelError.Reason.NotScalar(text, expected))) yet sentinel

  given textParsable: Text is Tel.Parsable =
    primitiveParsable(Morphology.Str, t"Text", t""): atom => atom

  given stringParsable: String is Tel.Parsable =
    primitiveParsable(Morphology.Str, t"String", ""): atom => atom.s

  // `Int`/`Long`/`Boolean` distinguish the two ways a byte-parsed primitive
  // can be `Unset` through `Parsable.scalarFault`: a missing atom raises
  // `Absent`, a present-but-unparseable one raises `NotScalar(text, expected)`
  // with the offending atom's text — exactly the `primitiveFault` split the
  // AST path performs, so the two paths' errors agree.
  //
  // They read their value straight from the atom's arena
  // bytes (`reader.int()`/`long()`/`boolean()`), so a valid scalar never
  // materializes the value `String` that `atom()` would — only a rejected one
  // does, for its error. `Double` keeps the text path: replicating
  // `Double.parseDouble`'s grammar in bytes buys little (doubles are the
  // rarest leaf and the format is broad).
  given intParsable: Int is Tel.Parsable =
    new Tel.Parsable:
      type Self = Int
      def shape(): Morphology = Morphology.Whole
      override def nature: Tel.Nature = Tel.Nature.Scalar

      def parse(reader: TelReader^, indent: Int): Int =
        reader.int().lay(Parsable.scalarFault(reader, t"Int", 0))(identity)

      override def absent()(using Tactic[TelError]): Int =
        raise(TelError(TelError.Reason.Absent)) yet 0

      override def parseAtom(text: Text)(using Tactic[TelError]): Int =
        val parsed = try Optional(text.s.toInt) catch case _: NumberFormatException => Unset
        parsed.or(raise(TelError(TelError.Reason.NotScalar(text, t"Int"))) yet 0)

  given longParsable: Long is Tel.Parsable =
    new Tel.Parsable:
      type Self = Long
      def shape(): Morphology = Morphology.Whole
      override def nature: Tel.Nature = Tel.Nature.Scalar

      def parse(reader: TelReader^, indent: Int): Long =
        reader.long().lay(Parsable.scalarFault(reader, t"Long", 0L))(identity)

      override def absent()(using Tactic[TelError]): Long =
        raise(TelError(TelError.Reason.Absent)) yet 0L

      override def parseAtom(text: Text)(using Tactic[TelError]): Long =
        val parsed = try Optional(text.s.toLong) catch case _: NumberFormatException => Unset
        parsed.or(raise(TelError(TelError.Reason.NotScalar(text, t"Long"))) yet 0L)

  given doubleParsable: Double is Tel.Parsable =
    primitiveParsable(Morphology.Real, t"Double", 0.0): atom =>
      try atom.s.toDouble catch case _: NumberFormatException => Unset

  given booleanParsable: Boolean is Tel.Parsable =
    new Tel.Parsable:
      type Self = Boolean
      def shape(): Morphology = Morphology.Bool
      override def nature: Tel.Nature = Tel.Nature.Scalar

      def parse(reader: TelReader^, indent: Int): Boolean =
        reader.boolean().lay(Parsable.scalarFault(reader, t"Boolean", false))(identity)

      override def absent()(using Tactic[TelError]): Boolean =
        raise(TelError(TelError.Reason.Absent)) yet false

      override def parseAtom(text: Text)(using Tactic[TelError]): Boolean =
        Parsable.atomBoolean(text)

  // `Tel` reads itself directly through the AST bridge — the direct
  // counterpart of `telDecodable`.
  given telParsable: Tel is Tel.Parsable =
    Tel.Parsable.fromDecodable(Tel.telDecodable)

  // Nominal counterpart of `Tel2.fieldCollection`: a collection reads
  // directly only when its element type has opted in.
  given iterableParsable: [collection <: Iterable, element]
  =>  ( factory: Factory[element, collection[element]], tactic: Tactic[TelError] )
  =>  ( parsable: => (element is Tel.Parsable)^ )
  =>  collection[element] is Tel.Parsable =
    Tel.Parsable.iterable[collection, element](parsable)

  // Alias counterparts: the opaque prelude collections do not conform to
  // `Iterable`, so each parses at the underlying stdlib type and casts.
  given listParsable: [list <: List, element]
  =>  ( tactic: Tactic[TelError] )
  =>  ( parsable: => (element is Tel.Parsable)^ )
  =>  list[element] is Tel.Parsable =
    Tel.Parsable.iterable[scala.collection.immutable.List, element](parsable)
    . asInstanceOf[list[element] is Tel.Parsable]

  given setParsable: [set <: Set, element]
  =>  ( tactic: Tactic[TelError] )
  =>  ( parsable: => (element is Tel.Parsable)^ )
  =>  set[element] is Tel.Parsable =
    Tel.Parsable.iterable[scala.collection.immutable.Set, element](parsable)
    . asInstanceOf[set[element] is Tel.Parsable]

  given seriesParsable: [sequence <: Sequence, element]
  =>  ( tactic: Tactic[TelError] )
  =>  ( parsable: => (element is Tel.Parsable)^ )
  =>  sequence[element] is Tel.Parsable =
    Tel.Parsable.iterable[Vector, element](parsable)
    . asInstanceOf[sequence[element] is Tel.Parsable]

  // The direct read of a field type carried by a plain text codec — the
  // direct counterpart of `Tel2.decodable`'s `Decodable in Text` branch,
  // including its absence behavior (an empty `Tel`'s primary atom is the
  // empty text, which the codec receives verbatim).
  private[stratiform] def textCodecParsable[value]
    ( using codec: value is distillate.Decodable in Text )
  :   (value is Tel.Parsable) =

    new Tel.Parsable:
      type Self = value
      def shape(): Morphology = Morphology.Str
      override def nature: Tel.Nature = Tel.Nature.Scalar
      def parse(reader: TelReader^, indent: Int): value = codec.decoded(reader.atom().or(t""))
      override def parse(reader: TelReader^): value = codec.decoded(t"")
      override def absent()(using Tactic[TelError]): value = codec.decoded(t"")
      override def parseAtom(text: Text)(using Tactic[TelError]): value = codec.decoded(text)

  // `source.read[List[Tel]]` / `read[Chain[Tel]]` for a multi-document source
  // (§6.1). `List[Tel]` parses every document eagerly; `Chain[Tel]` parses
  // them lazily on demand (the more specific instance wins over turbulence's
  // generic `Chain` Aggregable, which would otherwise wrap the whole source as
  // a single element).
  given listAggregable: (tactic: Tactic[TelError]) => ((List[Tel] is Aggregable by Data)^{tactic}) =
    source => parseAll(concatenate(source))

  given streamAggregable: (tactic: Tactic[TelError])
  =>  ((Chain[Tel] is Aggregable by Data)^{tactic}) =
    source => parseStream(concatenate(source))

  // `text.load[Tel]` for any Chain[Text] source: concatenates the
  // chunks, UTF-8 encodes, parses, and pairs the resulting Tel with a
  // `Tel.Metadata` carrying the document's prologue.
  given loadable: (tactic: Tactic[TelError], buffering: Buffering, tracking: PositionTracking)
  =>  ((Tel is Loadable by Text)^{tactic}) = stream =>
    // The whole document materializes once (the parser is whole-input), but the
    // Text stream is transcoded to UTF-8 through the encoder duct and memoized
    // straight to bytes — the parser's own input type — rather than through an
    // intermediate whole-document `String` and a second `getBytes` copy. The
    // non-consume `load` crosses to `memoize` as a neutral reference.
    val bytes: Data =
      stream.asInstanceOf[AnyRef].asInstanceOf[(zephyrine.Stream[Text] over zephyrine.Credit)^]
      . via(hieroglyph.charEncoders.utf8Encoder)
      . asInstanceOf[(zephyrine.Stream[Data] over zephyrine.Credit)^]
      . memoize

    // The metadata comes off the parsed `Tel`'s own subtree rather than from a
    // second, untracked `Tel.Parser.parse`, so that under `parsing.trackPositions`
    // the loaded document keeps its `positionIndex`.
    val tel = parseTracking(bytes)

    tel.subtree match
      case doc: Tel.Document =>
        turbulence.Document
         ( tel, Tel.Metadata(doc.interpreterDirective, doc.pragma, doc.lineEndings) )

      case _ =>
        turbulence.Document(tel, Tel.Metadata(Unset, Unset, Tel.LineEndings.Lf))

  // Renders a document's presentation back to text, reversing the presentation parser. The whole
  // line-based serialization lives in this instance so that `.show` is the single route to TEL
  // text: each emission appends one or more lines to the producer, with the document's line ending
  // inserted between lines but never after the last (total newlines = lines - 1; trailing
  // blank-line counts realised by appending that many empty lines).
  given documentShowable: Tel.Document is Showable = document =>
    import scala.language.unsafeNulls

    Producer.collect[Text](): producer =>
      val newline = document.lineEndings match
        case Tel.LineEndings.Lf   => "\n"
        case Tel.LineEndings.Crlf => "\r\n"

      var first = true

      // Emit one line, inserting the document's line ending between lines but never after the last.
      def out(text: String): Unit =
        if first then first = false else producer.put(Text(newline))
        producer.put(Text(text))

      def emitCompound(compound: Tel.Compound, indent: Int, sigil: Char): Unit =
        val pad = "  "*indent
        val line = StringBuilder()
        line.append(pad)
        line.append(compound.keyword.s)

        var trailingAtom: Optional[Tel.Atom] = Unset

        compound.atoms.each:
          case atom @ Tel.Atom.Source(_)     => trailingAtom = atom
          case atom @ Tel.Atom.Literal(_, _) => trailingAtom = atom

          case Tel.Atom.Inline(text, precedingSpaces) =>
            var k = 0
            while k < precedingSpaces do { line.append(' '); k += 1 }
            line.append(text.s)

        compound.remark.let: remark =>
          // Two spaces before the sigil ensure correct re-parsing regardless of whether the
          // preceding atoms put the line into hard-space mode: in hard mode only hard spaces
          // terminate phrases, so a single space before `#` would be absorbed as atom content.
          // §18.1 permits a minimum hard space before remark introducers.
          line.append("  ")
          line.append(sigil)
          line.append(' ')
          line.append(remark.s)

        out(line.toString)

        trailingAtom match
          case Tel.Atom.Source(text) =>
            val sourcePad = "  "*(indent + 2)
            // §14 "Convention A": `text` is LF-separated with no trailing LF, so each LF-delimited
            // segment is one source line (an empty segment is a blank line with no indentation).
            val sourceText = text.s
            var start = 0

            while start <= sourceText.length do
              val nl = sourceText.indexOf('\n', start)
              val end = if nl < 0 then sourceText.length else nl
              val seg = sourceText.substring(start, end).nn
              out(if seg.isEmpty then "" else sourcePad + seg)
              if nl < 0 then start = sourceText.length + 1 else start = nl + 1

          case Tel.Atom.Literal(delimiter, text) =>
            val delimiterLine = "  "*(indent + 3) + delimiter.s
            out(delimiterLine)
            val payload = text.s
            var start = 0

            while start <= payload.length do
              val nl = payload.indexOf('\n', start)
              val end = if nl < 0 then payload.length else nl
              out(payload.substring(start, end).nn)
              if nl < 0 then start = payload.length + 1 else start = nl + 1

            out(delimiterLine)

          case _ => ()

        // indexed: the foreach lambda would capture the exclusive producer transitively
        var childIndex = 0
        while childIndex < compound.children.length do
          emitBlock(compound.children.readUnchecked(childIndex), indent + 1, sigil)
          childIndex += 1

      def emitBlock(block: Tel.Block, indent: Int, sigil: Char): Unit =
        val pad = "  "*indent

        block.comments.each: comment =>
          val text = comment.text.s

          if text.isEmpty then out(s"$pad$sigil") else out(s"$pad$sigil $text")

        block.tabulation.let: tab =>
          val line = StringBuilder()
          var i = 0

          while i < tab.markerOffsets.length do
            val targetCol = tab.markerOffsets.readUnchecked(i)
            while line.length < targetCol do line.append(' ')
            line.append(sigil)
            val heading = tab.headings.readUnchecked(i).s

            if heading.nonEmpty then
              line.append(' ')
              line.append(heading)

            i += 1

          out(line.toString)

        block.compounds.each(emitCompound(_, indent, sigil))

        var b = 0

        while b < block.trailingBlankLines do
          out("")
          b += 1

      val sigil = document.pragma.let(_.sigil.or('#')).or('#')

      document.interpreterDirective.let: payload => out("#!" + payload.s)

      document.pragma.let: pragma =>
        val parts = scala.collection.mutable.ArrayBuffer.empty[String]
        parts += "tel"
        parts += s"${pragma.version._1}.${pragma.version._2}"
        pragma.schema.let: s => parts += s.s
        pragma.sigil.let: c => parts += c.toString
        out(parts.mkString(" "))
        out("")

      document.children.each(emitBlock(_, 0, sigil))

  // Render a `Tel` value. A `Tel` produced by `parse` is rooted at a Document and renders
  // presentation-preservingly; one produced by `encode` is rooted at a Compound, so its children
  // are wrapped in a Document first (otherwise encoded values would render as empty text). Composes
  // with `documentShowable`.
  given showable: Tel is Showable = tel =>
    val document = tel.subtree match
      case document: Document => document
      case other              => Document(Unset, Unset, LineEndings.Lf, other.children)

    document.show

  // Macro-friendly factory: bypasses the private constructor so generated
  // code from the `tel"…"` interpolator can produce Tel values without
  // tripping the inline-private-constructor restriction.
  def make(subtree: Subtree): Tel = new Tel(subtree)

  // camelCase → kebab-case: insert `-` before each interior uppercase
  // letter and lowercase it. `firstName` → `first-name`. Used by the
  // dynamic accessor to map Scala identifier names to TEL keywords.
  private[stratiform] def camelToKebab(s: String): Text =
    val sb = StringBuilder()
    var i = 0

    while i < s.length do
      val c = s.charAt(i)

      if c >= 'A' && c <= 'Z' then
        if i > 0 then sb.append('-')
        sb.append((c - 'A' + 'a').toChar)
      else
        sb.append(c)

      i += 1

    Text(sb.toString)

  // Replace the first compound with the given keyword across all
  // blocks; if no compound matches, append the new compound to the
  // last block (creating a fresh block if there are none).
  private[stratiform] def replaceOrAppendCompound
    ( blocks: Array[Block]^{}, keyword: Text, compound: Compound )
  :   Array[Block]^{} =

    var b = 0
    var found = false
    val out = scala.collection.mutable.ArrayBuffer.from(blocks.readable)

    while b < out.length && !found do
      val cs = out(b).compounds
      val idx = cs.indexWhere(_.keyword == keyword)

      if idx >= 0 then
        out(b) = out(b).copy(compounds = cs.updated(idx, compound))
        found = true

      b += 1

    if !found then
      if out.isEmpty then out += Block(Array.empty, Unset, Array.of(compound), 0)
      else
        val lastIdx = out.length - 1
        out(lastIdx) = out(lastIdx).copy(compounds = out(lastIdx).compounds :+ compound)

    Array.from(out)

  // Replace the `index`-th child compound (flattened across blocks) by applying
  // `transform`, preserving block structure and surrounding formatting. An
  // out-of-range index leaves the children unchanged. Used by the panopticon
  // `Ordinal` optic.
  private[stratiform] def withChildCompound
    ( blocks: Array[Block]^{}, index: Int, transform: Compound => Compound )
  :   Array[Block]^{} =

    var offset = 0

    Array.frozen:
      blocks.readable.map: block =>
        val compounds = block.compounds
        val base = offset
        offset += compounds.length

        if index >= base && index < base + compounds.length
        then
          block.copy(compounds = compounds.updated(index - base, transform(compounds.readUnchecked(index - base))))
        else
          block

  // Apply `transform` to every child compound (flattened across blocks),
  // preserving block structure. Used by the panopticon `Each` optic.
  private[stratiform] def mapChildCompounds(blocks: Array[Block]^{}, transform: Compound => Compound)
  :   Array[Block]^{} =

    blocks.map: block => block.copy(compounds = block.compounds.map(transform))

  private[stratiform] object Parser:
    import scala.language.unsafeNulls

    // The rim cast as a static helper (`TelReader.reveal`'s counterpart): a
    // fresh exclusive view of a parser reached through an erased reference,
    // sound by the pool's one-parser-per-thread construction guarantee.
    def reveal(parser: AnyRef): Parser^ = parser.asInstanceOf[Parser^]

    // Checker-opaque rim variants of the four entry points that macro-staged
    // parsers reach through a capture-erased `TelReader`: hosted on the
    // companion (not the parser) so no exclusive `Parser.this` is in scope,
    // each re-asserts the pool's per-thread exclusivity via `reveal`. Public
    // because staged parsers are generated into user modules.
    def erasedDirectKeyword(parser: AnyRef, indent: Int)(using Tactic[TelError]): Text | Null =
      reveal(parser).directKeyword(indent)

    def erasedDirectValue(parser: AnyRef, indent: Int)(using Tactic[TelError]): Tel =
      reveal(parser).directValue(indent)

    def erasedDirectDocument(parser: AnyRef)(using Tactic[TelError]): Tel =
      reveal(parser).directDocument()

    def erasedDirectEntrySubstance(parser: AnyRef)(using Tactic[TelError]): Boolean =
      reveal(parser).directEntrySubstance()

    // Per-thread cached parser. The parser's mutable state (scratch buffers,
    // keyword cache, StringBuilder, ArrayBuffer of ancestors, etc.) is
    // expensive to allocate fresh on every call; reusing the same parser
    // across calls on a thread amortises those allocations to ~one per
    // thread-lifetime. `reset()` re-initialises all per-parse state at the
    // start of each call. The atom arena is REPLACED (not reused) so any
    // `Tel.Atom.Inline` instances returned from prior parses keep their
    // backing bytes valid; the previous arena array stays alive via those
    // Inlines and gets GC'd once all references to the prior Document are
    // released.
    // The pool is a checker-opaque boundary (jacinta's Parser pool precedent):
    // ThreadLocal cannot carry capture-typed arguments; per-thread single ownership is
    // the pool's construction guarantee, reasserted at the rim by `borrow()`.
    private val cached: ThreadLocal[AnyRef] =
      new ThreadLocal[AnyRef]:
        override def initialValue(): AnyRef = (new Parser()).asInstanceOf[AnyRef]

    private[stratiform] def borrow(): Parser^ = cached.get.nn.asInstanceOf[Parser^]

    def parse(cursor: Cursor[Data, {}]^): (Tactic[TelError]^) ?->{cursor} Tel.Document =
      val p = borrow()
      p.reset(cursor, Unset)
      p.parse()

    def parse(cursor: Cursor[Data, {}]^, schema: Tels)
    :   (Tactic[TelError]^) ?->{cursor} Tel.Document =

      val p = borrow()
      p.reset(cursor, schema: Optional[Tels])
      p.parse()

    // Convenience overloads for callers that already have the whole input as
    // a single byte chunk. The cursor is constructed with `untrackedData`
    // lineation: Parser tracks the 1-indexed source line via its own
    // `lineNo` field (bumped at every LF-consumption point) and never reads
    // `cursor.line` / `cursor.column`. Driving cursor's lineation would
    // record (line, column) on every `cursor.mark` call — for a parser that
    // marks O(atoms + keywords + remarks + payloads) times per parse this is
    // significant wasted work, since the recorded offsets are never consulted.
    def parse(input: Data): Tel.Document raises TelError =
      import zephyrine.Lineation.untrackedData
      val p = borrow()
      p.reset(Cursor[Data](input), Unset)
      p.parse()

    def parse(input: Data, schema: Tels): Tel.Document raises TelError =
      import zephyrine.Lineation.untrackedData
      val p = borrow()
      p.reset(Cursor[Data](input), schema: Optional[Tels])
      p.parse()

    // Tracked parse: one document plus the flat pre-order (line, column, length)
    // triples backing `Tel.PositionIndex`. Uses `untrackedData` like the plain
    // path — coordinates come from the parser's own `lineNo` / `leadingSpaces`
    // bookkeeping, not the cursor's lineation.
    def parseTracked(input: Data): (Tel.Document, Array[Int]^{}) raises TelError =
      import zephyrine.Lineation.untrackedData
      val p = borrow()
      p.reset(Cursor[Data](input), Unset)
      p.tracking = true
      p.spanTracking = true

      try (p.parse(), Array.from(p.positionRecords))
      finally
        p.tracking = false
        p.spanTracking = false

    // Streaming parse (§6.1) of a multi-document source into the documents it
    // contains. `parseDocuments` is eager; `parseStream` parses lazily on demand.
    def parseDocuments(input: Data): List[Tel.Document] raises TelError =
      import zephyrine.Lineation.untrackedData
      val p = borrow()
      p.reset(Cursor[Data](input), Unset)
      p.parseAllDocuments()

    def parseStream(input: Data): Chain[Tel.Document] raises TelError =
      import zephyrine.Lineation.untrackedData
      // A dedicated parser instance, not the shared per-thread cache: the lazy
      // tail parses later document(s) on demand and must own its parser state.
      val p = new Parser()
      p.reset(Cursor[Data](input), Unset)
      p.documentStream(first = true)

    // The shared empty-children array: `Array.empty[Tel.Block]` builds a
    // fresh zero-length array through a reflective `ClassTag` on every call,
    // and `parseChildren` returns it once per leaf entry.
    private val EmptyBlocks: Array[Tel.Block]^{} = Array.empty[Tel.Block]

    private final val SP: Byte = 0x20
    private final val LF: Byte = 0x0A
    private final val CR: Byte = 0x0D
    private final val BOM0: Byte = 0xEF.toByte
    private final val BOM1: Byte = 0xBB.toByte
    private final val BOM2: Byte = 0xBF.toByte

    // ── SWAR (SIMD-within-a-register) byte scan helpers ──────────────────────
    // The inner byte-scan loops in the parser read a `Long` of 8 bytes at a
    // time from the buffer and use the classic "haszero" trick to detect a
    // target byte across all 8 lanes in two arithmetic ops. On long content
    // runs (literal payloads, source-atom lines, inline atoms) this replaces
    // an 8-iteration byte loop with one Long load plus a couple of bitwise
    // operations.

    // Reads a little-endian `Long` from eight bytes at `pos`. Assembled by hand
    // rather than through a `java.lang.invoke.VarHandle` byte-array view so that
    // this compiles and links on Scala.js (which has no `java.lang.invoke`); the
    // JIT folds this to an equivalent load on the JVM.
    private def longView(bytes: scala.Array[Byte], pos: Int): Long =
      (bytes(pos) & 0xffL) |
        ((bytes(pos + 1) & 0xffL) << 8) |
        ((bytes(pos + 2) & 0xffL) << 16) |
        ((bytes(pos + 3) & 0xffL) << 24) |
        ((bytes(pos + 4) & 0xffL) << 32) |
        ((bytes(pos + 5) & 0xffL) << 40) |
        ((bytes(pos + 6) & 0xffL) << 48) |
        ((bytes(pos + 7) & 0xffL) << 56)

    private final val OnesMask:     Long = 0x0101010101010101L
    private final val HighBitsMask: Long = 0x8080808080808080L

    // The byte `b` replicated across all 8 lanes of a Long.
    private inline def replicate(b: Byte): Long = (b & 0xFFL) * OnesMask

    // Pre-computed replications for the constant scan targets.
    private final val SpRepl: Long = (SP & 0xFFL) * OnesMask
    private final val LfRepl: Long = (LF & 0xFFL) * OnesMask
    private final val CrRepl: Long = (CR & 0xFFL) * OnesMask

    // Per-lane "is this byte zero?" mask: each 0x80 marks a zero byte in `v`.
    private inline def haszero(v: Long): Long =
      (v - OnesMask) & ~v & HighBitsMask

    // Per-lane "does this byte equal the target?" mask.
    private inline def matchByte(v: Long, replicated: Long): Long =
      haszero(v ^ replicated)

    // A compound-line scan's stop bytes — space, LF, CR — in one mask; zero
    // means all eight bytes are keyword or atom content. Little-endian
    // (`longView`), so `numberOfTrailingZeros(mask) >> 3` is the offset of
    // the first stop byte.
    private inline def contentStops(word: Long): Long =
      matchByte(word, SpRepl) | matchByte(word, LfRepl) | matchByte(word, CrRepl)

    // The packed-keyword printability test: true when the first `len` bytes
    // of `packed` are all printable ASCII (0x21–0x7E), so the packed form
    // cannot alias a shorter keyword's or a sentinel. The tail is filled
    // with a printable byte so only real content can trip the below-0x21,
    // DEL or high-bit conditions.
    private inline def printableWord(packed: Long, len: Int): Boolean =
      val tail = if len == 8 then 0L else -1L << (len*8)
      val filled = packed | (tail & 0x2121212121212121L)
      val below = (filled - 0x2121212121212121L) & ~filled & HighBitsMask
      val del = { val x = filled ^ 0x7F7F7F7F7F7F7F7FL
                  (x - OnesMask) & ~x & HighBitsMask }

      (below | del | (packed & HighBitsMask)) == 0L

    // Carries the look-ahead state for the next unconsumed line. Parsed once
    // by `fillHead`, then consulted by recursive-descent functions to decide
    // whether to continue at the current indent, descend, ascend, or stop.
    // Reused (mutated in place) across every line — single allocation per
    // parser instance.
    //
    // `startLine` is the 1-indexed source line number of this line. We store
    // the line number directly (not a byte position) because the streaming
    // parser uses narrow per-leaf holds: once a hold closes the cursor may
    // compact the buffer, so a byte offset recorded earlier can no longer
    // be used to compute a line number later. Line numbers are bumped
    // incrementally at every LF-consumption point (consumeLineEnding plus
    // the two raw LF advances in parseLiteralAtom).
    private[stratiform] final class LineHead extends caps.Mutable:
      var leadingSpaces: Int = 0
      var indentLevels:  Int = 0      // (leadingSpaces - margin) / 2 or -1
      var blank:         Boolean = false
      var eof:           Boolean = false
      var startLine:     Int = 1      // 1-indexed source line of this line
      // §5/§6.1: this line is a document separator — exactly two resolved-sigil
      // characters at column zero and nothing else. It ends the current document
      // (like EOF) and the next document begins on the following line. Recognised
      // only at the structural level, so it is computed only for non-blank,
      // non-EOF, zero-indent lines.
      var separator:     Boolean = false

  // Holds an exclusive cursor in a field, so the parser is itself a capability
  // (fresh per instance; one per thread via the pool).
  private[stratiform] final class Parser() extends caps.ExclusiveCapability, caps.Mutable:
    import java.lang as jl
    import java.nio.charset.StandardCharsets
    import scala.language.unsafeNulls
    import denominative.*
    import fulminate.*
    import TelError.Reason

    import Parser.*

    // ── Local snapshot ────────────────────────────────────────────────────────
    // Mirrors YamlParser's pattern: a parser-local snapshot of the cursor's
    // buffer keeps `bytes`/`pos`/`bufEnd` as plain fields so the JIT can hold
    // them in registers across hot byte loops. Sync to the cursor before any
    // mark/slice/refill operation; resync after.
    //
    // `cursor` and `schema` are `var`s rather than constructor args because
    // the parser is cached per-thread and reset across calls. `reset()`
    // re-binds both before each `parse()` invocation.

    @scala.caps.unsafe.untrackedCaptures
    var cursor0: AnyRef = null

    // An exclusive view of the bound cursor: the untracked field reads as
    // read-only, and a per-use fresh capability keeps it apart from `this`.
    private inline def cursor: Cursor[Data, {}]^ = cursor0.asInstanceOf[Cursor[Data, {}]^]
    var schema: Optional[Tels] = Unset
    @scala.caps.unsafe.untrackedCaptures
    var bytes0: AnyRef = null

    // An exclusive view of the buffer snapshot: the untracked `AnyRef` field
    // keeps the mutable array's capture out of the parser's fields.
    private inline def bytes: scala.Array[Byte]^ = bytes0.asInstanceOf[scala.Array[Byte]^]
    var pos:    Int = 0
    var bufEnd: Int = 0

    // Incrementally tracked 1-indexed source line number of the current
    // cursor position. Bumped at every LF-consumption point — `consumeLineEnding`
    // and the two raw `advance()` calls over LF in `parseLiteralAtom`. Stays
    // valid across hold boundaries (and therefore across buffer compaction)
    // because it doesn't depend on resident buffer bytes. The cursor's own
    // `lineation` is left disabled on the hot path; we don't drive
    // `cursor.unsafeBumpLine` because column reconstruction is done from the
    // current buffer (always inside a hold containing the line's bytes) by
    // `columnForCurrentBytePos`, which doesn't depend on the cursor's
    // `columnNo`.
    var lineNo: Int = 1

    // ── Parser state ──────────────────────────────────────────────────────────

    var margin: Int = 0
    var sigil:  Byte = '#'.toByte
    var crlfMode: Boolean = false
    var lineEndingsDetected: Boolean = false
    var lineEndings: Tel.LineEndings = Tel.LineEndings.Lf

    // Look-ahead record describing the next unconsumed line.
    var head: LineHead^ = new LineHead

    // Tracks state for the §9 CommentNotPreceded check.
    // `prevLineWasBoundary` is true iff the immediately-previous consumed line
    // was a blank, a comment, the prologue (directive / pragma), or we are
    // still at start-of-file — i.e. any line that may legitimately precede a
    // comment. `prevContentLeadingSpaces` records the leadingSpaces of the
    // most-recent content line (compound or tabulation), used to compare
    // against the indent at which a new comment appears.
    var prevLineWasBoundary: Boolean = true
    var prevContentLeadingSpaces: Int = -1

    // E102 detection: a "tel" / "tel …" line at column 0 that isn't the first
    // non-blank line is a misplaced pragma. We set this flag whenever a
    // non-blank line (directive, pragma, comment, compound, tabulation) has
    // been consumed.
    var hasConsumedNonBlankLine: Boolean = false

    // Parser surfaces an empty sentinel line when the input ends with LF
    // (so a file "code\n" parses as two lines: "code" and ""). The streaming
    // parser hits EOF after the LF, so the next consumeTrailingBlanksFor at
    // the innermost block needs to claim one extra blank. Set when the LF we
    // just consumed is the final byte of the document; consumed exactly once
    // by the first consumeTrailingBlanksFor that reaches EOF.
    var documentEndsWithLf: Boolean = false

    // ── Position tracking (set by `Parser.parseTracked`) ──────────────────────
    // When `tracking` is on, the parser appends one `Tel.positionStride`-int
    // record per compound — (line, column, keyword length, value column, value
    // length), in recursive-descent (pre-order) sequence — to `positionRecords`,
    // from which `Tel.buildIndex` folds the navigable `PositionIndex`. Off by
    // default, so the throughput-optimised parse path is untouched (no record is
    // written and no per-compound work is added).
    private[stratiform] var tracking: Boolean = false
    private[stratiform] val positionRecords = scala.collection.mutable.ArrayBuffer.empty[Int]

    // Measuring a compound line's inline-atom run. Both parse paths want the
    // same answer — the AST path folds it into the `PositionIndex`, the direct
    // path stamps it straight onto a `Tel.Focus` — so it is computed once, in
    // the line scan they share (`parseCompoundLineRest`), rather than twice.
    //
    // `spanTracking` gates it: on under `parseTracked`, and on the direct path
    // under `parsing.trackPositions`. Off, the scan pays one never-taken branch
    // per atom and nothing else.
    //
    // `lineValueOrigin` is the 1-indexed column just past the keyword, set by
    // whichever side read the keyword; `lineValueColumn` is the first atom's
    // column (0 for a line with no inline atoms) and `lineValueLength` the run's
    // extent, both in characters.
    private[stratiform] var spanTracking:    Boolean = false
    private[stratiform] var lineValueOrigin: Int = 1
    private[stratiform] var lineValueColumn: Int = 0
    private[stratiform] var lineValueLength: Int = 0

    // The direct path reads entries in a stream, so `directEntry*` describe
    // whichever entry was stepped to most recently — a nested one clobbers its
    // parent's. Stamping a focus therefore needs to know *which* entry the
    // recorded value extent belongs to: `directEntrySeq` numbers the entries and
    // `directValueSeq` records the number current when the extent was measured.
    // When they differ, the block being focused read something deeper, and the
    // focus falls back to the keyword span captured before it ran.
    private[stratiform] var directEntrySeq: Int = 0
    private[stratiform] var directValueSeq: Int = -1

    // Ancestor stack of Struct types known for each open compound, used by
    // §19.5's schema-aware E107 recovery. The element at index `i` is the
    // schema struct corresponding to the compound at depth `i+1` (so the
    // document root at depth 0 is implicit and refers to `schema.document`).
    // `Unset` entries mark compounds whose schema position couldn't be
    // resolved (e.g. an unknown keyword) — both shallower and deeper
    // recovery treat that ancestor as schema-blind.
    val ancestors =
      scala.collection.mutable.ArrayBuffer.empty[Optional[Tels.Struct]]

    // Reusable string builder for source-atom / literal-atom payloads.
    val sb: jl.StringBuilder = new jl.StringBuilder(256)

    // Atom-bytes arena: one growing byte buffer per parser instance into which
    // every inline atom's UTF-8 bytes are written. Each `Tel.Atom.Inline`
    // stores `(arenaArray, offset, length)` referring to a slice of the
    // arena. When the arena's capacity is exhausted, the parser allocates a
    // fresh, larger backing array; previously committed atoms keep the old
    // arena array alive via their slice references. This replaces the prior
    // "one freshly-allocated exact-size byte[] per atom" scheme: a workload
    // with N atoms now performs O(log capacity) arena allocations instead of
    // N per-atom allocations.
    //
    // `inFlightStart` is the arena offset where the currently-open atom
    // started; -1 when no atom is open. On a mid-atom arena growth we carry
    // the in-flight bytes (`atomArena(inFlightStart..arenaPos)`) into the
    // new array at offset 0 so the atom remains contiguous; the old array
    // (which still holds completed atoms) stays alive via their Inline
    // references.
    var atomArena0:    AnyRef = (new scala.Array[Byte](256)).asInstanceOf[AnyRef]

    // An exclusive view of the arena: the untracked `AnyRef` field keeps the
    // mutable array's capture out of the parser's fields.
    private inline def atomArena: scala.Array[Byte]^ = atomArena0.asInstanceOf[scala.Array[Byte]^]
    var arenaPos:      Int = 0
    var inFlightStart: Int = -1

    // Direct-primary-atom capture: when `directPrimaryOnly` is set (by the
    // primitive readers), `commit()` consumes the first inline atom's bytes
    // straight into the mode-appropriate slot and allocates no `Tel.Atom.Inline`
    // at all. Numeric modes parse the value out of the arena bytes with no
    // `String` at all; only the `Text` mode materializes one. The value is
    // captured on commit (not deferred) because a later atom on the same line
    // may grow the arena and move the bytes.
    private inline val PrimaryText = 0
    private inline val PrimaryLong = 1
    private inline val PrimaryBoolean = 2
    private inline val PrimaryInt = 3

    var directPrimaryOnly:    Boolean       = false
    var directPrimaryMode:    Int           = PrimaryText
    private[stratiform] var directPrimaryPresent: Boolean       = false
    var directPrimaryOk:      Boolean       = false
    private[stratiform] var directPrimaryText:    String | Null = null
    var directPrimaryLongVal: Long          = 0L
    var directPrimaryBoolVal: Boolean       = false

    // Parse `atomArena[off until off+len]` as a base-10 integer with exactly
    // `java.lang.Long.parseLong`'s acceptance (optional single leading `+`/`-`,
    // then ASCII digits, in `Long` range) — accumulating as a negative so
    // `Long.MinValue` is representable. Sets `directPrimaryLongVal` and returns
    // whether the slice parsed; on `false` the caller reproduces the AST path's
    // `NumberFormatException => Unset`, so semantics match byte-for-byte.
    private update def parseArenaLong(off: Int, len: Int): Boolean =
      if len == 0 then false else
        var i = 0
        var negative = false
        val first = atomArena(off)

        if first == '-'.toByte then { negative = true; i = 1 }
        else if first == '+'.toByte then i = 1

        if i == len then false else
          var result = 0L
          val limit = if negative then Long.MinValue else -Long.MaxValue
          val multMin = limit / 10L
          var ok = true

          while ok && i < len do
            val digit = atomArena(off + i) - '0'.toByte

            if digit < 0 || digit > 9 || result < multMin then ok = false
            else
              result *= 10L
              if result < limit + digit then ok = false else { result -= digit; i += 1 }

          if ok then directPrimaryLongVal = if negative then result else -result
          ok

    private update def beginInFlightAtom(): Unit = inFlightStart = arenaPos

    private update def endInFlightAtom(): Int =
      val len = arenaPos - inFlightStart
      inFlightStart = -1
      len

    private inline def arenaInFlightOffset: Int = inFlightStart

    private update def ensureArenaSpace(n: Int): Unit =
      if arenaPos + n > atomArena.length then growArena(n)

    // Non-inline: keep the slow path out of the inline budget. Allocates a
    // fresh arena array sized to at least `arenaPos + n`, copies the
    // in-flight atom's bytes (if any) into it starting at offset 0, and
    // resets `arenaPos` accordingly. Already-committed atoms continue to
    // reference the previous array.
    update def growArena(n: Int): Unit =
      val inFlightLen = if inFlightStart >= 0 then arenaPos - inFlightStart else 0
      val needed = inFlightLen + n
      var newCap = (atomArena.length * 2).max(256)
      while newCap < needed do newCap *= 2
      val newArena = new scala.Array[Byte](newCap)

      if inFlightLen > 0 then System.arraycopy(atomArena, inFlightStart, newArena, 0, inFlightLen)

      // The cast erases the fresh array's capture: it is confined to this parser.
      atomArena0 = newArena.asInstanceOf[AnyRef]
      if inFlightStart >= 0 then inFlightStart = 0
      arenaPos = inFlightLen

    // An exclusive view for writes: the untracked field reads as read-only.
    private inline def arenaTarget: scala.Array[Byte]^ = atomArena.asInstanceOf[scala.Array[Byte]^]

    private update def appendToArena(b: Byte): Unit =
      ensureArenaSpace(1)
      arenaTarget(arenaPos) = b
      arenaPos += 1

    private update def appendToArenaRange(src: scala.Array[Byte], off: Int, len: Int): Unit =
      ensureArenaSpace(len)
      System.arraycopy(src, off, atomArena, arenaPos, len)
      arenaPos += len

    // ── Honeycomb-style scratch buffers ───────────────────────────────────────
    // A single parser-lifetime array per child-collection type holds every
    // pending child across the whole recursive descent. Each
    // `parseBlock` / `parseCompoundLine` invocation snapshots the current
    // index at scope-entry; on scope-exit it computes `count = current - start`,
    // snapshots that range into a freshly allocated exact-size, frozen `Array`
    // (see `takeAtoms` and its siblings), and rewinds the index.
    // Empty scopes return `Array.empty` without any allocation.
    //
    // This replaces one `mutable.ArrayBuffer` (which itself allocates a backing
    // array, grows geometrically, and copies on growth) + one `Array.from`
    // call per scope with a single exact-size `Array.copyOfRange` allocation
    // per non-empty scope. For typical workloads (deep nesting, many siblings)
    // it eliminates several thousand allocations per parse.

    @scala.caps.unsafe.untrackedCaptures
    var scratchAtoms0: AnyRef = (new scala.Array[Tel.Atom](16)).asInstanceOf[AnyRef]

    // An exclusive view of the scratch stack: the untracked `AnyRef` field
    // keeps the mutable array's capture out of the parser's fields.
    private inline def scratchAtoms: scala.Array[Tel.Atom]^ =
      scratchAtoms0.asInstanceOf[scala.Array[Tel.Atom]^]
    var atomScratchIx:    Int = 0

    @scala.caps.unsafe.untrackedCaptures
    var scratchComments0: AnyRef = (new scala.Array[Tel.Comment](8)).asInstanceOf[AnyRef]

    // An exclusive view of the scratch stack: the untracked `AnyRef` field
    // keeps the mutable array's capture out of the parser's fields.
    private inline def scratchComments: scala.Array[Tel.Comment]^ =
      scratchComments0.asInstanceOf[scala.Array[Tel.Comment]^]
    var commentScratchIx: Int = 0

    @scala.caps.unsafe.untrackedCaptures
    var scratchCompounds0: AnyRef = (new scala.Array[Tel.Compound](8)).asInstanceOf[AnyRef]

    // An exclusive view of the scratch stack: the untracked `AnyRef` field
    // keeps the mutable array's capture out of the parser's fields.
    private inline def scratchCompounds: scala.Array[Tel.Compound]^ =
      scratchCompounds0.asInstanceOf[scala.Array[Tel.Compound]^]
    var compoundScratchIx: Int = 0

    @scala.caps.unsafe.untrackedCaptures
    var scratchBlocks0: AnyRef = (new scala.Array[Tel.Block](16)).asInstanceOf[AnyRef]

    // An exclusive view of the scratch stack: the untracked `AnyRef` field
    // keeps the mutable array's capture out of the parser's fields.
    private inline def scratchBlocks: scala.Array[Tel.Block]^ =
      scratchBlocks0.asInstanceOf[scala.Array[Tel.Block]^]
    var blockScratchIx:   Int = 0

    private update def reserveAtom(): Unit =
      if atomScratchIx >= scratchAtoms.length then
        val grown = new scala.Array[Tel.Atom](scratchAtoms.length*2)
        System.arraycopy(scratchAtoms, 0, grown, 0, atomScratchIx)
        scratchAtoms0 = grown.asInstanceOf[AnyRef]

    private update def reserveComment(): Unit =
      if commentScratchIx >= scratchComments.length then
        val grown = new scala.Array[Tel.Comment](scratchComments.length*2)
        System.arraycopy(scratchComments, 0, grown, 0, commentScratchIx)
        scratchComments0 = grown.asInstanceOf[AnyRef]

    private update def reserveCompound(): Unit =
      if compoundScratchIx >= scratchCompounds.length then
        val grown = new scala.Array[Tel.Compound](scratchCompounds.length*2)
        System.arraycopy(scratchCompounds, 0, grown, 0, compoundScratchIx)
        scratchCompounds0 = grown.asInstanceOf[AnyRef]

    private update def reserveBlock(): Unit =
      if blockScratchIx >= scratchBlocks.length then
        val grown = new scala.Array[Tel.Block](scratchBlocks.length*2)
        System.arraycopy(scratchBlocks, 0, grown, 0, blockScratchIx)
        scratchBlocks0 = grown.asInstanceOf[AnyRef]

    // Exclusive views for writes: the untracked fields read as read-only.
    private inline def atomsTarget: scala.Array[Tel.Atom]^ =
      scratchAtoms.asInstanceOf[scala.Array[Tel.Atom]^]

    private inline def commentsTarget: scala.Array[Tel.Comment]^ =
      scratchComments.asInstanceOf[scala.Array[Tel.Comment]^]

    private inline def compoundsTarget: scala.Array[Tel.Compound]^ =
      scratchCompounds.asInstanceOf[scala.Array[Tel.Compound]^]

    private inline def blocksTarget: scala.Array[Tel.Block]^ =
      scratchBlocks.asInstanceOf[scala.Array[Tel.Block]^]

    private update def pushAtom(atom: Tel.Atom): Unit =
      reserveAtom()
      atomsTarget(atomScratchIx) = atom
      atomScratchIx += 1

    private update def pushComment(c: Tel.Comment): Unit =
      reserveComment()
      commentsTarget(commentScratchIx) = c
      commentScratchIx += 1

    private update def pushCompound(c: Tel.Compound): Unit =
      reserveCompound()
      compoundsTarget(compoundScratchIx) = c
      compoundScratchIx += 1

    private update def pushBlock(b: Tel.Block): Unit =
      reserveBlock()
      blocksTarget(blockScratchIx) = b
      blockScratchIx += 1

    // Extract `count` items ending at the current index, rewind, and return
    // them as a frozen array. Empty scopes return the shared empty array with
    // zero allocation.
    private update def takeAtoms(count: Int): Array[Tel.Atom]^{} =
      if count == 0 then Array.empty[Tel.Atom]
      else
        val result = new scala.Array[Tel.Atom](count)
        System.arraycopy(scratchAtoms, atomScratchIx - count, result, 0, count)
        atomScratchIx -= count
        result.asInstanceOf[Array[Tel.Atom]^{}]

    private update def takeComments(count: Int): Array[Tel.Comment]^{} =
      if count == 0 then Array.empty[Tel.Comment]
      else
        val result = new scala.Array[Tel.Comment](count)
        System.arraycopy(scratchComments, commentScratchIx - count, result, 0, count)
        commentScratchIx -= count
        result.asInstanceOf[Array[Tel.Comment]^{}]

    private update def takeCompounds(count: Int): Array[Tel.Compound]^{} =
      if count == 0 then Array.empty[Tel.Compound]
      else
        val result = new scala.Array[Tel.Compound](count)
        System.arraycopy(scratchCompounds, compoundScratchIx - count, result, 0, count)
        compoundScratchIx -= count
        result.asInstanceOf[Array[Tel.Compound]^{}]

    private update def takeBlocks(count: Int): Array[Tel.Block]^{} =
      if count == 0 then EmptyBlocks
      else
        val result = new scala.Array[Tel.Block](count)
        System.arraycopy(scratchBlocks, blockScratchIx - count, result, 0, count)
        blockScratchIx -= count
        result.asInstanceOf[Array[Tel.Block]^{}]

    // ── parseCompoundLine result channels ──────────────────────────────────────
    // parseCompoundLine deposits its keyword + remark into these single-slot
    // fields rather than returning a Tel.Compound. The atoms it reads remain
    // on the scratchAtoms stack; the caller (parseBlock) optionally pushes
    // an extra Source/Literal atom, then takes the entire atom run at once.
    // This eliminates the prior `parsed.copy(atoms = finalAtoms, children = ...)`
    // double-allocation: Tel.Compound is built exactly once with its final
    // atoms and children set.
    var compoundLineKeyword: Text = t""
    var compoundLineRemark:  Optional[Text] = Unset

    // ── Keyword interning cache ───────────────────────────────────────────────
    // 64-slot two-Long fingerprint cache. The byte-level variant: the first
    // four bytes pack into `low` and the next four into `high`. ASCII keywords
    // fingerprint injectively; multibyte UTF-8 keywords fingerprint by raw
    // bytes (still injective for ≤8 bytes).
    @scala.caps.unsafe.untrackedCaptures
    val keyCache:     scala.Array[String] = new scala.Array[String](64)
    @scala.caps.unsafe.untrackedCaptures
    val keyCacheLow:  scala.Array[Long]   = new scala.Array[Long](64)
    @scala.caps.unsafe.untrackedCaptures
    val keyCacheHigh: scala.Array[Long]   = new scala.Array[Long](64)

    // Exclusive views for writes: the untracked fields read as read-only.
    private inline def keyCacheTarget: scala.Array[String]^ = keyCache.asInstanceOf[scala.Array[String]^]

    private inline def keyCacheLowTarget: scala.Array[Long]^ =
      keyCacheLow.asInstanceOf[scala.Array[Long]^]

    private inline def keyCacheHighTarget: scala.Array[Long]^ =
      keyCacheHigh.asInstanceOf[scala.Array[Long]^]

    // ── Substrate ─────────────────────────────────────────────────────────────

    update def syncTo(): Unit =
      cursor.unsafeAdvanceBy(pos - cursor.unsafePos(using Unsafe))(using Unsafe)

    update def syncFrom(): Unit =
      // The cast erases the buffer view's capture: it is confined to this parser.
      bytes0 = cursor.buffer(using Unsafe).asInstanceOf[AnyRef]
      pos    = cursor.unsafePos(using Unsafe)
      bufEnd = cursor.unsafeWriteEnd(using Unsafe)

    private update inline def more: Boolean = pos < bufEnd || moreSlow()

    update def moreSlow(): Boolean =
      syncTo()

      if cursor.more then { syncFrom(); true } else { syncFrom(); false }

    private inline def peek: Byte = bytes(pos)

    private update inline def advance(): Unit = pos += 1

    // Ensure that at least `n` bytes are available from the current position
    // (or that we are at EOF). Pulls chunks from the cursor as needed. After
    // this returns, either `pos + n <= bufEnd` or we have hit EOF and no more
    // bytes will arrive. Used by the multi-byte look-ahead points (CR/LF
    // pairs, sigil + soft-space + content checks, pragma detection).
    //
    // The trick: mark, advance up to `n` steps to force successive refills,
    // then cue back. Inside the outer `hold` block, the mark prevents the
    // buffer from compacting past our current position, so the bytes we
    // need stay resident.
    update def ensureLookahead(n: Int): Unit =
      if pos + n <= bufEnd then ()
      else
        syncTo()
        val mk = cursor.mark(using Cursor.shared)
        var steps = 0

        while steps < n && cursor.more do
          cursor.advance()
          steps += 1

        cursor.cue(mk)
        syncFrom()

    // Peek the byte one past the current position, refilling if necessary.
    // Returns -1 if there is no byte one past the current position.
    private update def peekNext(): Int =
      ensureLookahead(2)
      if pos + 1 < bufEnd then bytes(pos + 1) & 0xff else -1

    // ── SWAR in-buffer scans ─────────────────────────────────────────────────
    // Each `scanUntilN` method advances the parser-local `pos` until either a
    // byte matching one of the targets is found, or `pos == bufEnd`. It does
    // NOT refill the cursor — callers wrap the call in a refill loop when
    // they need to scan across chunk boundaries.

    // Advance pos until bytes(pos) == target1, or pos == bufEnd.
    private update def scanUntil1(target1: Byte, repl1: Long): Unit =
      while pos + 8 <= bufEnd do
        val v = Parser.longView(bytes, pos)
        val mask = Parser.matchByte(v, repl1)

        if mask != 0L then
          pos += (java.lang.Long.numberOfTrailingZeros(mask) >>> 3)
          return

        pos += 8

      while pos < bufEnd && bytes(pos) != target1 do pos += 1

    // Advance pos until bytes(pos) ∈ {target1, target2}, or pos == bufEnd.
    private update def scanUntil2(target1: Byte, target2: Byte, repl1: Long, repl2: Long): Unit =
      while pos + 8 <= bufEnd do
        val v = Parser.longView(bytes, pos)
        val combined = Parser.matchByte(v, repl1) | Parser.matchByte(v, repl2)

        if combined != 0L then
          pos += (java.lang.Long.numberOfTrailingZeros(combined) >>> 3)
          return

        pos += 8

      while pos < bufEnd && bytes(pos) != target1 && bytes(pos) != target2 do pos += 1

    // Advance pos until bytes(pos) ∈ {a, b, c}, or pos == bufEnd.
    private update def scanUntil3
      ( a: Byte, b: Byte, c: Byte,
        rA: Long, rB: Long, rC: Long )
    :   Unit =

      while pos + 8 <= bufEnd do
        val v = Parser.longView(bytes, pos)

        val combined =
          Parser.matchByte(v, rA) | Parser.matchByte(v, rB) | Parser.matchByte(v, rC)

        if combined != 0L then
          pos += (java.lang.Long.numberOfTrailingZeros(combined) >>> 3)
          return

        pos += 8

      while pos < bufEnd && bytes(pos) != a && bytes(pos) != b && bytes(pos) != c do pos += 1

    // Advance pos until bytes(pos) ∈ {a, b, c, d}, or pos == bufEnd.
    private update def scanUntil4
      ( a: Byte, b: Byte, c: Byte, d: Byte,
        rA: Long, rB: Long, rC: Long, rD: Long )
    :   Unit =

      while pos + 8 <= bufEnd do
        val v = Parser.longView(bytes, pos)

        val combined =
          Parser.matchByte(v, rA) |
            Parser.matchByte(v, rB) |
            Parser.matchByte(v, rC) |
            Parser.matchByte(v, rD)

        if combined != 0L then
          pos += (java.lang.Long.numberOfTrailingZeros(combined) >>> 3)
          return

        pos += 8
      while pos < bufEnd && bytes(pos) != a && bytes(pos) != b && bytes(pos) != c && bytes(pos) != d
      do pos += 1

    // ── Errors ────────────────────────────────────────────────────────────────

    // Compute the column corresponding to the current cursor position
    // (1-indexed; 1 = first byte of line). Walks the *current* buffer backwards
    // looking for the most-recent LF. Because errors are always raised while a
    // hold is active around the line being parsed, the line's starting LF (or
    // start-of-stream) is always inside the resident region of the buffer.
    private update def columnForCurrentBytePos(): Int =
      var i = pos - 1
      var col = 1

      while i >= 0 && bytes(i) != LF do
        col += 1
        i -= 1

      col

    // Every error carries a `Span` covering the offending text: `length` is the
    // extent, in characters, of the token being read. Sites that know the token
    // they rejected (a keyword, a pragma phrase, a hard-space run, a literal's
    // delimiter, an indent) pass it; the rest fall back to the single character
    // at the reported column.
    private update def errorHere(reason: Reason, length: Int = 1)
    :   (Tactic[TelError]^) ?->{this} Nothing =

      val column = columnForCurrentBytePos()
      abort(TelError(reason, TelError.spanAt(lineNo, column, length)))

    // Direct line-number variant: callers pass the 1-indexed source line of
    // the offending location and its column.
    private update def errorAt(reason: Reason, line: Int, column: Int, length: Int = 1)
    :   (Tactic[TelError]^) ?->{this} Nothing =

      abort(TelError(reason, TelError.spanAt(line, column, length)))

    // Recoverable-error variants of `errorHere`/`errorAt`. They `raise` the error
    // (rather than `abort`) and continue with `continuation`, which realises the
    // §19.5 `recoveryOf(reason)` strategy. Under the default `ThrowTactic` `raise`
    // throws, so parsing stays fail-fast and identical to `errorHere`/`errorAt`;
    // under a `validate`-installed `TrackTactic` the error accrues and parsing
    // continues, so a document with several recoverable defects reports them all.
    // The continuation MUST advance the cursor past the offending input (or the
    // surrounding flow must), otherwise an enclosing scan loop would not progress.
    private update inline def recoverHere[T](reason: Reason, length: Int = 1)
      ( continuation: => T )
      ( using Tactic[TelError]^ )
    :   T =

      raise(TelError(reason, TelError.spanAt(lineNo, columnForCurrentBytePos(), length)))
      continuation

    private update inline def recoverAt[T](reason: Reason, line: Int, column: Int, length: Int = 1)
      ( continuation: => T )
      ( using Tactic[TelError]^ )
    :   T =

      raise(TelError(reason, TelError.spanAt(line, column, length)))
      continuation

    // ── Mark / hold plumbing ──────────────────────────────────────────────────
    //
    // The streaming parser uses narrow per-leaf holds: every method that takes
    // a `cursor.mark` (or that calls down to one — `peekNext`, `ensureLookahead`,
    // `consumeLineEnding`, `sliceText`, etc.) wraps its mark-using scope in
    // `inHold`. Outside any hold, the cursor is free to compact away consumed
    // bytes when a refill needs space; that's exactly the streaming property
    // we want. State carried between holds is restricted to plain primitives
    // — `head.*`, `lineNo`, `prevLineWasBoundary`, accumulated `sb` content —
    // never byte offsets into a particular buffer.
    //
    // `inHold` nests safely: an inner `inHold` does not change `holdStart`,
    // it just executes inside the outer hold's protection. `Cursor.Held` is
    // a witness type and `Cursor.shared` is a singleton we always pass to
    // `cursor.mark` — there's no run-time tracking of "are we in a hold"
    // inside the parser because no run-time decision depends on it; the
    // compile-time `using Cursor.Held` requirement at every mark call site
    // is the static guarantee that the code path is reachable only from
    // inside a hold.
    private update inline def inHold[T](inline body: => T): T =
      syncTo()

      cursor.hold:
        try
          syncFrom()
          body
        finally syncTo()

    private update def beginMark(): Cursor.Mark =
      syncTo()
      cursor.mark(using Cursor.shared)

    // Materialise the byte range between `start` and the current position as
    // a UTF-8 `String`. The only allocation point in the keyword / atom path.
    // Non-inline: the cursor.slice lambda's path-dependent `addressable.Storage`
    // resolves once here against `this.cursor`.
    private update def sliceText(start: Cursor.Mark): String =
      syncTo()
      val endMk = cursor.mark(using Cursor.shared)

      cursor.slice(start, endMk): (storage, off, len) =>
        val arr = storage.asInstanceOf[scala.Array[Byte]]
        if len <= 0 then "" else new String(arr, off, len, StandardCharsets.UTF_8)

    // Rewind to a held mark — the bail of a speculative fast scan (the
    // `directEntrySubstance` idiom).
    private update def rewindTo(mark: Cursor.Mark): Unit =
      syncTo()
      cursor.cue(mark)
      syncFrom()

    // ── Reset (per-thread reuse) ──────────────────────────────────────────────
    //
    // Re-initialise the parser for a new parse. Called by the cached factory
    // in `object Parser` before each `parse()` invocation. Every mutable
    // field that survives across parses is reset here; reusable arrays
    // (scratch buffers, ancestors, sb) are cleared in place so we don't pay
    // their allocation again. The atom arena is REPLACED with a fresh array
    // because previously-parsed Inlines reference the old one — we cannot
    // overwrite their bytes. The keyword-fingerprint cache is preserved
    // across resets so frequent keywords stay interned.
    private[stratiform] update def reset(c: Cursor[Data, {}]^, s: Optional[Tels]): Unit =
      cursor0 = c.asInstanceOf[AnyRef]
      schema = s
      bytes0 = null
      pos    = 0
      bufEnd = 0
      lineNo = 1
      tracking = false
      spanTracking = false
      positionRecords.clear()
      lineValueOrigin = 1
      lineValueColumn = 0
      lineValueLength = 0
      directEntrySeq = 0
      directValueSeq = -1
      directEntryKeywordLen = 0
      margin = 0
      sigil  = '#'.toByte
      crlfMode = false
      lineEndingsDetected = false
      lineEndings = Tel.LineEndings.Lf
      head.leadingSpaces = 0
      head.indentLevels  = 0
      head.blank = false
      head.eof   = false
      head.startLine = 1
      head.separator = false
      prevLineWasBoundary = true
      prevContentLeadingSpaces = -1
      hasConsumedNonBlankLine = false
      documentEndsWithLf = false
      ancestors.clear()
      sb.setLength(0)
      // Fresh arena: previous parse's Inlines hold the old array via their
      // (arena, off, len) backing reference, so it stays alive as long as
      // those Inlines are referenced.
      atomArena0    = (new scala.Array[Byte](256)).asInstanceOf[AnyRef]
      arenaPos      = 0
      inFlightStart = -1
      // Null out the scratch arrays so they don't pin references from the
      // previous parse. Cheap (one pass over what are typically small arrays);
      // worth it to let GC collect the previous Document's transient nodes
      // promptly while the parser sits in the ThreadLocal between calls.
      java.util.Arrays.fill(scratchAtoms.asInstanceOf[scala.Array[AnyRef]], null)
      atomScratchIx = 0
      java.util.Arrays.fill(scratchComments.asInstanceOf[scala.Array[AnyRef]], null)
      commentScratchIx = 0
      java.util.Arrays.fill(scratchCompounds.asInstanceOf[scala.Array[AnyRef]], null)
      compoundScratchIx = 0
      java.util.Arrays.fill(scratchBlocks.asInstanceOf[scala.Array[AnyRef]], null)
      blockScratchIx = 0
      compoundLineKeyword = t""
      compoundLineRemark  = Unset

    // Soft reset between documents in a stream (§6.1). Re-initialises only
    // per-document state; preserves the live cursor position, the continuous
    // `lineNo` counter, and the whole-source line-ending mode (§4). The BOM is
    // significant only at the true start of the source, so it is not re-checked.
    // A fresh `atomArena` is allocated per document because each parsed
    // document's `Inline` atoms keep their own arena alive.
    private[stratiform] update def resetForNextDocument(): Unit =
      margin = 0
      sigil  = '#'.toByte
      head.leadingSpaces = 0
      head.indentLevels  = 0
      head.blank = false
      head.eof   = false
      head.startLine = lineNo
      head.separator = false
      prevLineWasBoundary = true
      prevContentLeadingSpaces = -1
      hasConsumedNonBlankLine = false
      documentEndsWithLf = false
      ancestors.clear()
      sb.setLength(0)
      atomArena0    = (new scala.Array[Byte](256)).asInstanceOf[AnyRef]
      arenaPos      = 0
      inFlightStart = -1
      java.util.Arrays.fill(scratchAtoms.asInstanceOf[scala.Array[AnyRef]], null)
      atomScratchIx = 0
      java.util.Arrays.fill(scratchComments.asInstanceOf[scala.Array[AnyRef]], null)
      commentScratchIx = 0
      java.util.Arrays.fill(scratchCompounds.asInstanceOf[scala.Array[AnyRef]], null)
      compoundScratchIx = 0
      java.util.Arrays.fill(scratchBlocks.asInstanceOf[scala.Array[AnyRef]], null)
      blockScratchIx = 0
      compoundLineKeyword = t""
      compoundLineRemark  = Unset

    // ── Top-level parse ───────────────────────────────────────────────────────

    // Single-document parse (§6.1): reads exactly one document and stops at the
    // first document separator. Any content after the separator is left
    // unconsumed and need not be valid TEL.
    update def parse(): (Tactic[TelError]^) ?->{this} Tel.Document =
      syncFrom()
      checkBom()
      parseOneDocument()

    // Parse one document from the current cursor position, stopping at the first
    // document separator or at end of input. The caller has already synced from
    // the cursor and (for the first document only) checked the BOM. On return,
    // `head` is parked either on a separator (`head.separator`) or at EOF.
    private update def parseOneDocument(): (Tactic[TelError]^) ?->{this} Tel.Document =
      val directive = parseInterpreterDirective()
      val pragma = parsePragma()
      fillHead()  // park head at the next line

      if directive.absent && pragma.absent then determineMargin()
      else
        // A directive or pragma may be followed by blank lines before the
        // first content line; consume them so parseChildren can dispatch
        // on a real content line.
        margin = 0
        while head.blank && !head.eof do fillHead()

      val children = parseChildren(parentIndent = -1)
      Tel.Document(directive, pragma, lineEndings, children)

    // ── Document streams (§6.1) ────────────────────────────────────────────────

    // Streaming parse: yield each document of the source in order, parsed
    // independently. Eager — all documents are parsed (and any TelError raised)
    // before returning. A separator followed only by blank lines or nothing
    // yields no trailing empty document; two consecutive separators yield one
    // empty document between them.
    update def parseAllDocuments(): (Tactic[TelError]^) ?->{this} List[Tel.Document] =
      syncFrom()
      checkBom()
      val buffer = scala.collection.mutable.ListBuffer.empty[Tel.Document]
      var first = true
      var continue = true

      while continue do
        if !first then resetForNextDocument()
        first = false
        val doc = parseOneDocument()
        val terminatedBySeparator = head.separator
        if terminatedBySeparator then consumeSeparatorLine()
        // A document terminated by a separator is always emitted (even when
        // empty — the empty document between two separators). A document
        // terminated by EOF is dropped if empty: this is the trailing
        // separator-then-blanks case, and the entirely-blank source.
        if terminatedBySeparator || !documentIsEmpty(doc) then buffer += doc
        if !terminatedBySeparator then continue = false

      buffer.to(List)

    // Lazy streaming parse: documents are parsed on demand as the returned
    // `Chain` is forced. Mirrors turbulence's deferred `Streamable` readers,
    // which likewise capture the error capability in the lazy tail; the consumer
    // must drive the stream within the `raises TelError` handler's scope. Uses a
    // dedicated parser instance (never the shared per-thread cache) so the
    // parser state survives across element demands.
    private update def documentStream(first: Boolean)
    :   (Tactic[TelError]^) ?->{this} Chain[Tel.Document] =

      if first then
        syncFrom()
        checkBom()
      else
        resetForNextDocument()

      val doc = parseOneDocument()

      if head.separator then
        consumeSeparatorLine()
        doc #:: documentStream(first = false)
      else if documentIsEmpty(doc) then
        Chain.empty
      else
        Chain(doc)

    // A document with no prologue and no children: the empty document yielded
    // between two separators, or the absence of any document at the end of a
    // stream.
    private update def documentIsEmpty(doc: Tel.Document): Boolean =
      doc.children.nil && doc.interpreterDirective.absent && doc.pragma.absent

    // `head` is parked on a separator (the cursor sits on its first sigil).
    // Consume both sigil bytes and the terminating line-ending so the next
    // document begins on the following line. At true EOF the separator has no
    // line-ending and `consumeLineEnding` is a no-op.
    private update def consumeSeparatorLine(): (Tactic[TelError]^) ?->{this} Unit = inHold:
      ensureLookahead(2)
      advance()  // first sigil
      advance()  // second sigil
      consumeLineEnding()
      prevLineWasBoundary = true

    // ── BOM ──────────────────────────────────────────────────────────────────

    private update def checkBom(): (Tactic[TelError]^) ?->{this} Unit =
      if more && peek == BOM0 then
        // §19.5 SkipBom: drop the byte-order mark and parse from the next byte.
        recoverHere(Reason.BomPresent):
          advance()
          if more && peek == BOM1 then advance()
          if more && peek == BOM2 then advance()

    // ── Line endings ─────────────────────────────────────────────────────────

    // Detect line-endings mode from the first LF seen. CR before LF → CRLF
    // mode. After detection, every subsequent byte is checked against the
    // mode: in LF mode a CR is E121; in CRLF mode a lone LF (not preceded by
    // CR) is E121. The detection is `lazy` — driven by `consumeLfFromHere`,
    // which is the only place we consume an LF.
    private update def detectLineEndingMode(crBefore: Boolean): Unit =
      if !lineEndingsDetected then
        lineEndingsDetected = true
        crlfMode = crBefore
        lineEndings = if crBefore then Tel.LineEndings.Crlf else Tel.LineEndings.Lf

    // ── Prologue ─────────────────────────────────────────────────────────────

    // Reads "#!..." line if present. The directive payload excludes the
    // "#!" prefix and the terminating LF.
    private update def parseInterpreterDirective(): (Tactic[TelError]^) ?->{this} Optional[Text] = inHold:
      // We can peek the first two bytes without consuming.
      if !more then Unset
      else if peek != '#'.toByte then Unset
      else
        val second = peekNext()

        if second != '!'.toInt then Unset
        else
          // Consume "#!"
          advance()
          advance()
          val mk = beginMark()
          // Read until LF or CR
          while more && peek != LF && peek != CR do advance()
          val payload = sliceText(mk)
          consumeLineEnding()
          prevLineWasBoundary = true
          hasConsumedNonBlankLine = true
          Text(payload)

    // Reads a pragma line ("tel ..." or "tel") if present as the first
    // non-blank line. Marks before consuming any blanks; if the first
    // non-blank line is NOT a pragma, cues back so the cursor remains at
    // the original position and the caller can run determineMargin from
    // scratch. The mark must survive the blank-line scan, so the entire
    // body runs inside one hold.
    private update def parsePragma(): (Tactic[TelError]^) ?->{this} Optional[Tel.Pragma] = inHold:
      // `pragmaStartAbs` is the absolute byte position of the start of the
      // first non-blank line, used to check the 4096-byte pragma cap (§3.5)
      // against absolute stream position rather than the (possibly compacted)
      // buffer-relative `pos`. Computed via cursor.position after syncTo so it
      // remains correct once we narrow holds elsewhere in the parser.
      val mk = beginMark()
      val savedBoundary = prevLineWasBoundary
      val savedLineNo = lineNo

      // Skip blank lines (consuming them; we may cue back).
      var foundPragma: Optional[Tel.Pragma] = Unset
      var done = false

      while !done do
        val spaceCount = countLeadingSpaces()

        if !more then done = true
        else if peek == LF || peek == CR then
          consumeLineEnding()
        else
          // First non-blank line. Is it a pragma?
          if spaceCount == 0 && startsWithPragma() then
            val pragmaLine = lineNo
            syncTo()
            val pragmaStartAbs = cursor.position.n0

            // §19.5 AllowOversize: record the cap breach but keep parsing the pragma.
            if pragmaStartAbs >= 4096 then recoverAt(Reason.PragmaTooLong, pragmaLine, 1)(())

            val pragmaMk = beginMark()
            while more && peek != LF && peek != CR do advance()
            val payload = sliceText(pragmaMk)
            syncTo()
            val pragmaEndAbs = cursor.position.n0

            // The whole pragma line is the offending text, and it has now been read.
            if pragmaEndAbs > 4096
            then recoverAt(Reason.PragmaTooLong, pragmaLine, 1, payload.length)(())

            consumeLineEnding()
            prevLineWasBoundary = true
            hasConsumedNonBlankLine = true
            foundPragma = Optional(parsePragmaContent(payload, pragmaLine))

          done = true

      if foundPragma.present then foundPragma
      else
        // Not a pragma — cue back to before any blanks were consumed.
        syncTo()
        cursor.cue(mk)
        syncFrom()
        prevLineWasBoundary = savedBoundary
        lineNo = savedLineNo
        Unset

    // Count leading spaces at the current position (does not consume the LF /
    // content byte). Stops at LF, CR, EOF, or any non-space byte. Does NOT
    // advance lineation across LF — caller must handle that.
    private update def countLeadingSpaces(): Int =
      var count = 0

      // A word at a time while the window allows: indent runs are short, so
      // one load usually decides.
      while pos + 8 <= bufEnd && {
        val nonSpace =
          ~Parser.matchByte(Parser.longView(bytes, pos), Parser.SpRepl) & Parser.HighBitsMask

        if nonSpace == 0L then
          pos += 8
          count += 8
          true
        else
          val run = java.lang.Long.numberOfTrailingZeros(nonSpace) >> 3
          pos += run
          count += run
          false
      } do ()

      while more && peek == SP do
        advance()
        count += 1

      count

    // Consume one line-ending sequence (LF or CR LF). Validates that bare CR
    // (in LF mode) and bare LF (in CRLF mode) raise E121. Sets
    // `documentEndsWithLf` when the LF we just consumed is the final byte of
    // the document — used by consumeTrailingBlanksFor to count the virtual
    // empty trailing line that Parser surfaces when its lines array ends
    // with a sentinel empty entry.
    private update def consumeLineEnding()(using Tactic[TelError]): Unit =
      if !more then ()
      else if peek == LF then
        // §19.5 CollapseLineEndings: accept the inconsistent ending and carry on.
        if !lineEndingsDetected then detectLineEndingMode(crBefore = false)
        else if crlfMode then recoverHere(Reason.BadLineEnding)(())

        advance()
        lineNo += 1
        if !more then documentEndsWithLf = true
      else if peek == CR then
        val next = peekNext()

        // A bare CR (not followed by LF) cannot be consumed coherently, so it stays
        // fatal; an LF-mode CR LF is recoverable (CollapseLineEndings).
        if next != LF.toInt then errorHere(Reason.BadLineEnding)
        else
          if !lineEndingsDetected then detectLineEndingMode(crBefore = true)
          else if !crlfMode then recoverHere(Reason.BadLineEnding)(())

          advance()  // CR
          advance()  // LF
          lineNo += 1
          if !more then documentEndsWithLf = true
      else
        // not at a line ending — caller error
        ()

    // True iff the cursor is at the start of a pragma line (already past any
    // leading spaces, on the first content byte). A pragma is "tel" followed
    // by EOL or " ". Requires four bytes of look-ahead, or three bytes at EOF.
    private update def startsWithPragma(): Boolean =
      ensureLookahead(4)
      if pos + 2 < bufEnd &&
        bytes(pos) == 't'.toByte && bytes(pos + 1) == 'e'.toByte &&
        bytes(pos + 2) == 'l'.toByte
      then
        if pos + 3 < bufEnd
        then bytes(pos + 3) == SP || bytes(pos + 3) == LF || bytes(pos + 3) == CR
        else true  // exactly "tel" at EOF
      else
        false

    private update def parsePragmaContent(content: String, line: Int)
    :   (Tactic[TelError]^) ?->{this} Tel.Pragma =

      val (parts, offsets) = splitPragmaPhrases(content)

      // §19.5 RestartFromPragma: a non-`tel` head is recorded but parsing continues.
      if parts.head != "tel"
      then recoverAt(Reason.PragmaNotFirst, line, offsets.head + 1, parts.head.length)(())

      val version =
        if parts.length >= 2 then parseVersion(parts.stdlib(1), line, offsets.stdlib(1) + 1) else (1, 0)

      // §19.5 IgnoreExtraPragmaAtoms: only parts 2 and 3 are read below, so excess
      // atoms are already ignored once the error is recorded. The span runs from the
      // first excess atom to the end of the line.
      if parts.length > 4
      then recoverAt(Reason.ExtraPragmaContent, line, offsets.stdlib(4) + 1, content.length - offsets.stdlib(4))
        ( () )

      val schemaText: Optional[Text] =
        if parts.length >= 3 then
          val s = parts.stdlib(2)
          // §8.1: the schema identifier is either an HTTP/HTTPS URL (with a
          // `://`) or a bare BASE-256-encoded schema signature. The BASE-256
          // alphabet (§4) is exactly the Unicode letters and ASCII digits, so
          // a bare signature is a non-empty run of letters/digits with no
          // whitespace or punctuation. Palimpsest length/decodability are
          // checked later, at signature resolution — not at pragma parse time.
          val isUrl = s.indexOf("://") >= 0

          val isBase256 =
            s.nonEmpty && s.forall: c => Character.isLetter(c) || (c >= '0' && c <= '9')

          // §19.5 IgnoreSchemaId: a malformed schema identifier is dropped (Unset).
          if !isUrl && !isBase256
          then recoverAt(Reason.BadSchemaIdentifier, line, offsets.stdlib(2) + 1, s.length)(Unset)
          else Text(s): Optional[Text]
        else
          Unset

      val pragmaSigil: Optional[Char] =
        if parts.length >= 4 && parts.stdlib(3).length == 1 then
          val c = parts.stdlib(3).charAt(0)
          // §19.5 UseDefaultSigil: an invalid sigil is dropped, keeping the default.
          if c.isLetterOrDigit then recoverAt(Reason.BadSigil, line, offsets.stdlib(3) + 1, 1)(Unset)
          else
            sigil = c.toByte
            c: Optional[Char]
        else
          Unset

      Tel.Pragma(version, schemaText, pragmaSigil)

    // `column` is the 1-indexed column of the version phrase within the pragma
    // line, so a malformed version is spanned at the phrase itself.
    private update def parseVersion(s: String, line: Int, column: Int)
    :   (Tactic[TelError]^) ?->{this} (Int, Int) =

      // §19.5 IgnoreVersion: a malformed version falls back to (0, 0) so the rest
      // of the document is still parsed (and its defects accrued).
      val dot = s.indexOf('.')

      if dot <= 0 || dot == s.length - 1
      then recoverAt(Reason.BadVersion, line, column, s.length)((0, 0))
      else
        try
          val major = s.substring(0, dot).toInt
          val minor = s.substring(dot + 1).toInt

          if major < 0 || minor < 0
          then recoverAt(Reason.BadVersion, line, column, s.length)((0, 0))
          else (major, minor)

        catch case _: NumberFormatException =>
          recoverAt(Reason.BadVersion, line, column, s.length)((0, 0))

    // Splits a pragma line into its phrases, pairing each with its 0-based
    // character offset within the line so that an error in one phrase — a bad
    // version, sigil or schema identifier — is spanned at that phrase rather
    // than at the start of the line.
    private update def splitPragmaPhrases(content: String): (List[String], List[Int]) =
      val parts = scala.collection.mutable.ListBuffer.empty[String]
      val offsets = scala.collection.mutable.ListBuffer.empty[Int]
      val builder = new StringBuilder()
      var i = 0
      var start = 0
      var hardSpaceMode = false

      while i < content.length do
        val ch = content.charAt(i)

        if ch == ' ' then
          var j = i
          while j < content.length && content.charAt(j) == ' ' do j += 1
          val runLength = j - i

          if !hardSpaceMode && runLength == 1 then
            if builder.nonEmpty then
              parts += builder.toString
              offsets += start
              builder.clear()

            i += 1
          else
            if !hardSpaceMode then hardSpaceMode = true

            if builder.nonEmpty then
              parts += builder.toString
              offsets += start
              builder.clear()

            i = j
        else
          if builder.isEmpty then start = i
          builder.append(ch)
          i += 1

      if builder.nonEmpty then
        parts += builder.toString
        offsets += start

      (List.of(parts.toList), List.of(offsets.toList))

    // ── Margin determination ─────────────────────────────────────────────────

    // Sets `margin` to the leadingSpaces of the first non-blank content line.
    // The caller's outer fillHead has already parked head at the first line;
    // we walk through blanks if needed.
    private update def determineMargin(): (Tactic[TelError]^) ?->{this} Unit =
      while head.blank && !head.eof do fillHead()

      if !head.eof then
        margin = head.leadingSpaces
        head.indentLevels = 0  // by definition of margin

    // ── §19.5 schema-aware E107 recovery ─────────────────────────────────────

    // Peek the keyword of the line that head is currently parked at (i.e. read
    // it without advancing past the line). Uses mark + cue to restore the
    // cursor's byte position, lineation, and the parser's local snapshot.
    private update def peekKeyword(): (Tactic[TelError]^) ?->{this} Text =
      val outerMark = beginMark()
      val kw = readKeyword()
      syncTo()
      cursor.cue(outerMark)
      syncFrom()
      kw

    // Resolve `t` to a Struct, optionally following one Reference indirection.
    // Returns Unset for Scalars, Flag, Select etc.
    private update def resolveTypeToStruct(t: Tels.Type, s: Tels): Optional[Tels.Struct] =
      t match
        case struct: Tels.Struct => struct

        case Tels.Reference(name) =>
          s.records.find(_.name == name) match
            case Some(rec) => Tels.Struct(rec.members, rec.validators)
            case None      => Unset

        case _ => Unset

    // Resolve `keyword` against `parent`'s direct Field / SelectRef members.
    // Returns the Struct type if the keyword names a child whose resolved
    // type is a Struct, or Unset otherwise.
    private update def resolveKeywordStruct(parent: Tels.Struct, keyword: Text, s: Tels)
    :   (Tactic[TelError]^) ?->{this} Optional[Tels.Struct] =

      var found: Optional[Tels.Type] = Unset
      var i = 0

      while i < parent.members.length && found.absent do
        parent.members.readUnchecked(i) match
          case f: Tels.Field =>
            if f.keyword == keyword then found = f.fieldType

          case sr: Tels.SelectRef =>
            s.selects.find(_.name == sr.reference).foreach: selectDef =>
              selectDef.variants.find(_.keyword == keyword).foreach: variant =>
                found = variant.variantType

          case _: Tels.Exclude => ()

        i += 1

      found.let(resolveTypeToStruct(_, s))

    // Does `parent` admit `keyword` in its direct Field / SelectRef set?
    private update def keywordAdmissible(parent: Tels.Struct, keyword: Text, s: Tels): Boolean =
      var i = 0
      var hit = false

      while !hit && i < parent.members.length do
        parent.members.readUnchecked(i) match
          case f: Tels.Field =>
            if f.keyword == keyword then hit = true

          case sr: Tels.SelectRef =>
            s.selects.find(_.name == sr.reference).foreach: selectDef =>
              if selectDef.variants.exists(_.keyword == keyword) then hit = true

          case _: Tels.Exclude => ()

        i += 1

      hit

    // Push a child compound's resolved struct onto the ancestor stack. No-op
    // (records Unset for depth bookkeeping) without a schema.
    private update def pushAncestor(keyword: Text): (Tactic[TelError]^) ?->{this} Unit =
      schema.let: s =>
        val parent: Optional[Tels.Struct] =
          if ancestors.nil then s.document else ancestors(ancestors.length - 1)

        val resolved: Optional[Tels.Struct] =
          parent.let(resolveKeywordStruct(_, keyword, s))

        ancestors += resolved

      .or:
        ancestors += Unset

    private update def popAncestor(): Unit =
      if ancestors.nonEmpty then ancestors.remove(ancestors.length - 1)

    // §19.5 odd-indent recovery. Without a schema, raise E107. With a schema,
    // compute admissibility at both candidate depths via the current ancestor
    // stack and pick deeper if and only if shallower is invalid AND deeper is
    // valid; tie-break favours shallower.
    private update def recoverOddIndent(spaces: Int, line: Int): (Tactic[TelError]^) ?->{this} Int =
      val rel = spaces - margin

      schema.let: s =>
        val shallower = rel / 2
        val deeper    = shallower + 1
        val keyword   = peekKeyword()

        val shallowerParent: Optional[Tels.Struct] =
          if shallower == 0 then s.document
          else if shallower - 1 < ancestors.length then ancestors(shallower - 1)
          else Unset

        val deeperParent: Optional[Tels.Struct] =
          if shallower < ancestors.length then ancestors(shallower) else Unset

        val shallowerValid = shallowerParent.let(keywordAdmissible(_, keyword, s)).or(false)
        val deeperValid = deeperParent.let(keywordAdmissible(_, keyword, s)).or(false)

        if !shallowerValid && deeperValid then deeper else shallower

      . or:
        // §19.5 ShallowerIndent: round an odd indent down to the nearer level. The
        // leading spaces are already consumed, so returning a level cannot stall.
        // The offending text is the line's indent run.
        recoverAt(Reason.OddIndentation, line, 1, spaces)(rel / 2)

    // ── Line-head fill ───────────────────────────────────────────────────────

    // Consume the next line's leading spaces and parks at the first content
    // byte (or LF/EOF for a blank line). Updates `head` in place. Always
    // advances past EOF cleanly: head.eof = true, head.blank = true.
    //
    // The 1-indexed line number is read from the incrementally-maintained
    // `lineNo` counter (bumped at every LF-consumption point), so the success
    // path pays no per-line lineation cost. Runs inside its own hold so the
    // intermediate `consumeLineEnding` (which calls `peekNext` → `ensureLookahead`,
    // a mark-using lookahead) and `recoverOddIndent` (which calls `peekKeyword`,
    // also mark-using) execute inside an active `cursor.hold` scope.
    private update def fillHead()(using Tactic[TelError]): Unit = inHold:
      head.startLine = lineNo
      head.separator = false

      val spaces = countLeadingSpaces()
      head.leadingSpaces = spaces

      if !more then
        head.eof = true
        head.blank = true
        head.indentLevels = -1
      else if peek == LF || peek == CR then
        head.blank = true
        head.eof = false
        head.indentLevels = -1
        consumeLineEnding()
        // The line we just consumed was blank — it acts as a boundary for
        // E109.
        prevLineWasBoundary = true
      else
        head.blank = false
        head.eof = false
        // A document separator is recognised only at column zero (§5). The
        // resolved sigil is already in `sigil` (the pragma, if any, was parsed
        // before any body fillHead). When the line is a separator we leave the
        // cursor parked on its first sigil; the multi-document driver consumes
        // the separator line, and the body-parsing loops treat it like EOF.
        head.separator = spaces == 0 && headLineIsSeparator()
        val rel = spaces - margin

        head.indentLevels =
          // §19.5 AdjustMargin: a line indented less than the margin sits at level 0.
          // The indent run itself is the offending text (at least one character, so
          // an unindented line under a positive margin still has a visible span).
          if rel < 0 then recoverAt(Reason.LessThanMargin, head.startLine, 1, spaces.max(1))(0)
          else if rel % 2 == 0 then rel / 2
          else recoverOddIndent(spaces, head.startLine)

    // True iff the cursor is parked on the first byte of a document separator:
    // exactly two `sigil` bytes followed immediately by a line-ending or EOF —
    // no third character and (given the zero-indent precondition) no leading or
    // trailing spaces. Does not consume.
    private update def headLineIsSeparator(): Boolean =
      ensureLookahead(3)
      pos + 1 < bufEnd &&
        bytes(pos) == sigil && bytes(pos + 1) == sigil &&
        (pos + 2 >= bufEnd || bytes(pos + 2) == LF || bytes(pos + 2) == CR)

    // ── parseChildren ────────────────────────────────────────────────────────

    private update def parseChildren(parentIndent: Int): (Tactic[TelError]^) ?->{this} Array[Tel.Block]^{} =
      val expected = parentIndent + 1

      if head.eof || head.separator || head.indentLevels < expected
      then EmptyBlocks
      else
        val start = blockScratchIx

        // A line more indented than `expected` is misplaced. Under fail-fast the
        // first such line raises (unchanged); under a `validate` boundary we record
        // it and recover (SkipOverIndented / ShallowerIndent) by clamping it to
        // `expected` so `parseBlock` consumes it as a sibling — guaranteeing the
        // cursor advances every iteration, so the parser keeps going to EOF and
        // accrues every defect (e.g. for LSP diagnostics) without ever stalling.
        while !head.eof && !head.separator && head.indentLevels >= expected do
          if head.indentLevels > expected then
            val line   = head.startLine
            val indent = head.leadingSpaces
            val lastIx = blockScratchIx - 1

            val reason =
              if lastIx < start then Reason.OverIndentation
              else
                val last = scratchBlocks(lastIx)

                if last.tabulation.present && last.compounds.nil then Reason.RowWrongIndent
                else if last.tabulation.present || last.compounds.nil then Reason.ChildOfNonCompound
                else Reason.OverIndentation

            recoverAt(reason, line, 1, indent.max(1))(())
            head.indentLevels = expected

          parseBlock(expected)  // pushes one block onto scratchBlocks; consumes ≥1 line

        takeBlocks(blockScratchIx - start)

    // ── parseBlock ───────────────────────────────────────────────────────────

    // Parses one block at the given indent: leading comments, optional
    // tabulation header, a run of compounds (each possibly with source /
    // literal / children attached), then trailing blank lines (those that
    // semantically belong to this block). Pushes the resulting Tel.Block
    // onto the parser's `scratchBlocks` stack; the caller (parseChildren)
    // takes a contiguous range when its loop completes.
    private update def parseBlock(indent: Int): (Tactic[TelError]^) ?->{this} Unit =
      val commentStart  = commentScratchIx
      val compoundStart = compoundScratchIx

      // Leading comments at this indent.
      while !head.eof && !head.separator && !head.blank && head.indentLevels == indent &&
        isCommentBody()
      do
        // §9 E109 check — fires only if the immediately preceding line was a
        // content line (compound / tabulation) at indent ≥ this comment's.
        // §19.5 AttachComment: record the misplacement but attach the comment.
        if !prevLineWasBoundary && prevContentLeadingSpaces >= 0 &&
          prevContentLeadingSpaces >= margin + indent * 2
        then recoverAt(Reason.CommentNotPreceded, head.startLine, head.leadingSpaces + 1, 1)(())

        val text = parseCommentLine()
        pushComment(Tel.Comment(text))
        prevLineWasBoundary = true
        hasConsumedNonBlankLine = true
        fillHead()

      val hasComments = commentScratchIx > commentStart

      // Interior blanks: if we have comments and the next line is blank, look
      // ahead to see whether the blanks separate the comments from a compound
      // group at the same indent. If so, consume them as interior whitespace.
      if hasComments && !head.eof && head.blank then
        skipInteriorBlanksIfFollowedByContentAtIndent(indent)

      // Optional tabulation header.
      val tabulation: Optional[Tel.Tabulation] =
        if !head.eof && !head.separator && !head.blank && head.indentLevels == indent &&
          isTabulationBody()
        then
          val ls = head.leadingSpaces
          val tab = parseTabulationLine()
          prevContentLeadingSpaces = ls
          prevLineWasBoundary = false
          hasConsumedNonBlankLine = true
          fillHead()
          Optional(tab)
        else
          Unset

      // Compound loop.
      var keepLoop = true
      while keepLoop && !head.eof && !head.separator && !head.blank && head.indentLevels == indent
      do
        if isCommentBody() || isTabulationBody() then keepLoop = false
        else
          val compoundLeadingSpaces = head.leadingSpaces
          val compoundLine = head.startLine
          // §16.2: validate tabulated rows BEFORE parseCompoundLine consumes
          // the row's bytes. validateTabulatedRowInline uses mark + cue so the
          // cursor remains parked at the row start for parseCompoundLine.
          if tabulation.present then
            validateTabulatedRowInline(compoundLeadingSpaces, tabulation.vouch, compoundLine)

          // parseCompoundLine pushes the compound's inline atoms onto
          // scratchAtoms and deposits keyword + remark into the parser's
          // compoundLine* fields. The atoms stay on the stack so we can
          // append an optional source/literal extra atom alongside them
          // and take the entire run at once into the final Tel.Compound,
          // skipping the previous `parsed.copy(...)` double-allocation.
          val atomsStart = atomScratchIx
          parseCompoundLine(compoundLine)
          val compoundKeyword = compoundLineKeyword
          val compoundRemark  = compoundLineRemark

          // Record this compound's position *before* descending, so the record
          // stream is pre-order (parent before children) — the order
          // `Tel.buildIndex` folds against the AST. Columns are 1-indexed at the
          // keyword's first character (just past the leading spaces).
          //
          // The value extent covers the compound's inline atoms — the text a
          // decode error is about — and was measured by the line scan just now,
          // in `parseCompoundLineRest`, which the direct path shares. Reading it
          // here rather than recomputing it is what keeps the two paths from
          // drifting apart about where a value is.
          if tracking then
            positionRecords += compoundLine
            positionRecords += compoundLeadingSpaces + 1
            positionRecords += compoundKeyword.s.length
            positionRecords += lineValueColumn
            positionRecords += lineValueLength

          prevContentLeadingSpaces = compoundLeadingSpaces
          prevLineWasBoundary = false
          // §19.5 recovery needs this compound's keyword on the ancestor
          // stack BEFORE fillHead reads the next line — odd-indent recovery
          // there consults the ancestors. No-schema mode skips push/pop
          // entirely so the hot path is unaffected.
          val pushed = schema.present
          if pushed then pushAncestor(compoundKeyword)

          try
            fillHead()

            val extraAtom: Optional[Tel.Atom] =
              if tabulation.absent then parseSourceOrLiteralAtomIfPresent(compoundLeadingSpaces)
              else Unset

            if extraAtom.present then pushAtom(extraAtom.vouch)

            val children =
              if extraAtom.absent && tabulation.absent then parseChildren(indent) else EmptyBlocks

            val atoms = takeAtoms(atomScratchIx - atomsStart)
            pushCompound(Tel.Compound(compoundKeyword, atoms, compoundRemark, children))
          finally if pushed then popAncestor()

      val trailingBlankLines = consumeTrailingBlanksFor(indent)

      val comments  = takeComments(commentScratchIx - commentStart)
      val compounds = takeCompounds(compoundScratchIx - compoundStart)
      pushBlock(Tel.Block(comments, tabulation, compounds, trailingBlankLines))

    // After the comment-group, consume blank lines if the next non-blank line
    // is a content (not comment) line at the same `indent`. Otherwise leave
    // the blanks for the enclosing block. Uses mark+cue. The hold spans the
    // entire probe so the mark survives every nested fillHead.
    private update def skipInteriorBlanksIfFollowedByContentAtIndent(indent: Int)
    :   (Tactic[TelError]^) ?->{this} Unit =

      inHold:
        val mk = beginMark()

        val savedHeadSnapshot = (head.leadingSpaces, head.indentLevels, head.blank,
                                 head.eof, head.startLine)

        val savedBoundary = prevLineWasBoundary
        val savedLineNo = lineNo
        while !head.eof && head.blank do fillHead()
        val keep =
          !head.eof && !head.separator && head.indentLevels == indent && !isCommentBody()
        if !keep then
          // Rewind.
          syncTo()
          cursor.cue(mk)
          syncFrom()
          head.leadingSpaces = savedHeadSnapshot._1
          head.indentLevels  = savedHeadSnapshot._2
          head.blank         = savedHeadSnapshot._3
          head.eof           = savedHeadSnapshot._4
          head.startLine     = savedHeadSnapshot._5
          prevLineWasBoundary = savedBoundary
          lineNo = savedLineNo
          // Re-park: skip leading spaces of the line we just rewound to.
          var i = 0

          while i < savedHeadSnapshot._1 && more && peek == SP do
            advance()
            i += 1

    // ── Trailing blanks ──────────────────────────────────────────────────────

    // Consume blank lines that "belong" to the current block at `indent`. A
    // blank line belongs if the next non-blank line (if any) matches `indent`
    // or EOF.
    private update def consumeTrailingBlanksFor(indent: Int): (Tactic[TelError]^) ?->{this} Int =
      if head.eof then
        // The first parseBlock to reach EOF after a document that ended with
        // LF claims the virtual sentinel trailing line that Parser emits.
        // Cleared so outer parseBlock invocations don't double-count.
        if documentEndsWithLf then
          documentEndsWithLf = false
          1
        else
          0
      else if !head.blank then
        0
      else
        inHold:
          val mk = beginMark()

          val firstBlankSnapshot = (head.leadingSpaces, head.indentLevels, head.blank,
                                    head.eof, head.startLine)

          val savedBoundary = prevLineWasBoundary
          val savedLineNo = lineNo
          var count = 0

          while !head.eof && head.blank do
            count += 1
            fillHead()

          val keep =
            head.eof || head.indentLevels == indent

          if keep then count
          else
            // Rewind to the first blank.
            syncTo()
            cursor.cue(mk)
            syncFrom()
            head.leadingSpaces = firstBlankSnapshot._1
            head.indentLevels  = firstBlankSnapshot._2
            head.blank         = firstBlankSnapshot._3
            head.eof           = firstBlankSnapshot._4
            head.startLine     = firstBlankSnapshot._5
            prevLineWasBoundary = savedBoundary
            lineNo = savedLineNo
            var i = 0

            while i < firstBlankSnapshot._1 && more && peek == SP do
              advance()
              i += 1

            0

    // ── Predicates over the parked head ──────────────────────────────────────

    // True iff the parked head's first content byte is the sigil and the
    // line is a comment (sigil-only or sigil + soft space + text), as opposed
    // to a tabulation line or `#foo`-style keyword.
    private update def isCommentBody(): (Tactic[TelError]^) ?->{this} Boolean = inHold:
      if !more || peek != sigil then false
      else
        // Peek the rest of the line to decide between comment and tabulation.
        // A comment line has at most one space after the sigil; tabulation
        // lines have a hard space (2+) somewhere with another sigil.
        // Strategy: look one byte after the sigil.
        val nextByte = peekNext()

        if nextByte < 0 then true  // sigil at EOF — bare comment
        else if nextByte == LF.toInt || nextByte == CR.toInt then true
        else if nextByte != SP.toInt then false  // `#foo` — not a comment
        else
          // sigil + space + ... — could be comment or tabulation. Look ahead
          // to detect tabulation marker pattern. For now use mark+cue.
          !lineHasTabulationMarker()

    // Returns true iff the parked head's line, when read forward from current
    // position, contains a 2-space-or-more run followed by sigil somewhere
    // before the next LF. Uses mark+cue.
    private update def lineHasTabulationMarker(): (Tactic[TelError]^) ?->{this} Boolean = inHold:
      val mk = beginMark()
      var found = false
      var done = false

      while !done do
        if !more then done = true
        else
          val b = peek

          if b == LF || b == CR then done = true
          else if b == SP then
            // Count run.
            var run = 0
            while more && peek == SP do { advance(); run += 1 }

            if run >= 2 && more && peek == sigil then
              found = true
              done = true
          else
            advance()

      // Rewind.
      syncTo()
      cursor.cue(mk)
      syncFrom()
      found

    private update def isTabulationBody(): (Tactic[TelError]^) ?->{this} Boolean = inHold:
      if !more || peek != sigil then false
      else lineHasTabulationMarker()

    // ── Comment parsing ──────────────────────────────────────────────────────

    // Cursor is at the sigil. Returns the comment text, advancing past LF.
    private update def parseCommentLine(): (Tactic[TelError]^) ?->{this} Text = inHold:
      // Consume the sigil.
      advance()

      if !more || peek == LF || peek == CR then
        consumeLineEnding()
        t""
      else if peek == SP then
        // Skip the introducer space.
        advance()
        val mk = beginMark()
        while more && peek != LF && peek != CR do advance()
        val payload = sliceText(mk)
        consumeLineEnding()
        Text(payload)
      else
        // `#foo` — but we already classified this as a comment, so this
        // shouldn't happen. Treat as bare-content.
        val mk = beginMark()
        while more && peek != LF && peek != CR do advance()
        val payload = sliceText(mk)
        consumeLineEnding()
        Text(payload)

    // ── Tabulation line parsing ──────────────────────────────────────────────

    // Cursor is at the sigil. Reads marker offsets + headings, advances past
    // LF.
    private update def parseTabulationLine(): (Tactic[TelError]^) ?->{this} Tel.Tabulation = inHold:
      val lineStartCol = head.leadingSpaces  // first marker offset (column 0 = sigil)
      val markers = scala.collection.mutable.ArrayBuffer.empty[Int]
      val headings = scala.collection.mutable.ArrayBuffer.empty[Text]

      markers += lineStartCol  // first marker at first non-space position

      // Consume the first sigil.
      advance()

      // Repeating: heading text until next marker (hard-space + sigil) or
      // end of line.
      var lineCol = lineStartCol + 1  // column index just past the sigil
      var done = false
      while !done do
        // After a marker, an optional soft space introduces the heading.
        // §16 / E120: non-space immediately after marker is malformed; >1
        // leading space is malformed (unless empty heading).
        if !more || peek == LF || peek == CR then
          headings += t""
          done = true
        else if peek != SP then
          errorAt(Reason.BadTabulationHeading, head.startLine, lineCol + 1)
        else
          // peek == SP
          // Check if the next byte is also a space — if so, heading is empty
          // or malformed.
          val nextB = peekNext()
          if nextB == SP.toInt then
            // Two spaces after marker: empty heading? Look further.
            // Consume the two spaces; check if next is a sigil (start of next
            // column) or content (malformed).
            advance(); lineCol += 1  // first space
            advance(); lineCol += 1  // second space
            if more && peek == sigil then
              // Empty heading; new marker.
              markers += lineCol
              advance(); lineCol += 1
              headings += t""
              // continue outer loop for next column
            else if !more || peek == LF || peek == CR then
              // Empty heading at line end.
              headings += t""
              done = true
            else
              // E120: more spaces or non-sigil content after empty.
              errorAt(Reason.BadTabulationHeading, head.startLine, lineCol + 1)
          else
            // One soft space — heading text follows.
            advance(); lineCol += 1
            val mk = beginMark()
            var headingEnd = -1
            var stop = false

            while !stop do
              if !more || peek == LF || peek == CR then
                headingEnd = lineCol
                stop = true
              else if peek == sigil then
                errorAt(Reason.BadTabulationHeading, head.startLine, lineCol + 1)
              else if peek == SP then
                // Hard space check: two spaces in a row?
                val nb = peekNext()

                if nb == SP.toInt then
                  headingEnd = lineCol
                  stop = true
                else
                  advance(); lineCol += 1
              else
                advance(); lineCol += 1

            headings += Text(sliceText(mk))
            // Now we're either at LF/CR/EOF or at a hard-space run before a
            // marker.
            if !more || peek == LF || peek == CR then done = true
            else
              // Consume hard spaces.
              while more && peek == SP do { advance(); lineCol += 1 }

              if more && peek == sigil then
                markers += lineCol
                advance(); lineCol += 1
                // Loop continues.
              else if !more || peek == LF || peek == CR then
                done = true
              else
                errorAt(Reason.BadTabulationHeading, head.startLine, lineCol + 1)

      consumeLineEnding()
      Tel.Tabulation(Array.from(markers), Array.from(headings))

    // §16.2 column-rule validation. The cursor must be parked at the row's
    // first content byte (past leading spaces). Walks the bytes up to LF/CR
    // checking that every hard-space (2+) run ends exactly at one of the
    // tabulation header's marker positions (E117) and that no non-final
    // column value exceeds its declared width (E119). Uses mark + cue so the
    // cursor is restored to the same position for the subsequent
    // parseCompoundLine call.
    private update def validateTabulatedRowInline
      ( rowLeadingSpaces: Int, tabulation: Tel.Tabulation, lineNumber: Int )
    :   (Tactic[TelError]^) ?->{this} Unit =

      inHold:
        val markers = tabulation.markerOffsets
        val mk = beginMark()
        var col = rowLeadingSpaces
        var columnIdx = 0
        var phraseStart = rowLeadingSpaces
        var stopped = false

        while !stopped && more && peek != LF && peek != CR do
          val b = peek

          if b == SP then
            val runStart = col
            while more && peek == SP do { advance(); col += 1 }
            val runLen = col - runStart

            if runLen >= 2 then
              val sigilNext = more && peek == sigil

              val isRemark =
                if !sigilNext then false
                else
                  ensureLookahead(3)
                  val afterSigil = if pos + 1 < bufEnd then bytes(pos + 1) & 0xff else -1
                  afterSigil == SP.toInt && (pos + 2 >= bufEnd || bytes(pos + 2) != SP)
              if isRemark then
                // Remark terminates column validation.
                while more && peek != LF && peek != CR do { advance(); col += 1 }
                stopped = true
              else
                if columnIdx >= 1 && columnIdx < markers.length - 1 then
                  val phraseWidth = runStart - phraseStart
                  val colMax = markers.readUnchecked(columnIdx + 1) - markers.readUnchecked(columnIdx) - 2

                  // §19.5 SuppressColumnAlignment: record but keep scanning (the
                  // loop self-advances and the row is re-read by parseCompoundLine).
                  // The over-wide column value is the offending text.
                  if phraseWidth > colMax then
                    recoverAt(Reason.ColumnValueTooWide, lineNumber, phraseStart + 1, phraseWidth)
                      ( () )

                var foundIdx = -1
                var k = 1

                while k < markers.length && foundIdx < 0 do
                  if markers.readUnchecked(k) == col then foundIdx = k
                  k += 1

                // §19.5 SuppressColumnAlignment: record but keep scanning. The
                // misaligned hard-space run — from where it began to where it ended,
                // which is not a marker offset — is the offending text.
                if foundIdx < 0 then
                  recoverAt(Reason.HardSpaceWrongPosition, lineNumber, runStart + 1, runLen)(())

                columnIdx = foundIdx
                phraseStart = col
          else
            advance()
            col += 1

        // Cue back so parseCompoundLine reads the row from the start.
        syncTo()
        cursor.cue(mk)
        syncFrom()

    // ── Source / literal atom dispatch ───────────────────────────────────────

    private update def parseSourceOrLiteralAtomIfPresent(compoundLeadingSpaces: Int)
    :   (Tactic[TelError]^) ?->{this} Optional[Tel.Atom] =

      if head.eof || head.blank then Unset
      else
        val sourceIndent = compoundLeadingSpaces + 4
        val literalIndent = compoundLeadingSpaces + 6

        val first =
          if head.leadingSpaces == literalIndent then Optional(parseLiteralAtom(literalIndent))
          else if head.leadingSpaces == sourceIndent then Optional(parseSourceAtom(sourceIndent))
          else Unset

        // §10.4 / §11.1: at most one source / literal atom per compound. §19.5
        // IgnoreDuplicateAtom: record the duplicate but consume (and discard) it so
        // the cursor advances past it and parsing continues.
        // The duplicate's own indent is all that has been read of it when the error
        // is raised, so the span covers the line up to its first content byte.
        if first.present && !head.eof && !head.blank then
          if head.leadingSpaces == literalIndent then
            recoverAt(Reason.DuplicateLiteral, head.startLine, 1, literalIndent):
              parseLiteralAtom(literalIndent)
              ()
          else if head.leadingSpaces == sourceIndent then
            recoverAt(Reason.DuplicateSource, head.startLine, 1, sourceIndent):
              parseSourceAtom(sourceIndent)
              ()

        first

    // ── Source atom ──────────────────────────────────────────────────────────

    // Cursor is at the first content byte of a source line (past the
    // sourceIndent leading spaces). head reflects that. Reads lines while
    // they have leadingSpaces >= sourceIndent (or are interior blanks
    // followed by more source). Captured lines (first sourceIndent spaces
    // stripped, trailing spaces removed) are joined with LF as a separator
    // with no trailing LF; trailing blank lines are discarded (§14).
    private update def parseSourceAtom(sourceIndent: Int): (Tactic[TelError]^) ?->{this} Tel.Atom.Source =
      sb.setLength(0)

      // §14 "Convention A": the captured lines are joined with LF as a
      // separator with NO trailing LF, and trailing blank lines (those
      // between the last non-blank line and the dedent/EOF that ends the
      // atom) are discarded. `emitted` tracks whether a segment is already in
      // `sb` (so the separator is prepended, not appended); `pendingBlanks`
      // holds interior blank lines that only become LF separators once a
      // following non-blank line confirms they are not trailing.
      var done = false
      var emitted = false
      var pendingBlanks = 0

      while !done do
        if head.eof then
          // The final LF (if any) terminated this atom's last content line; it
          // is the line separator, not a trailing blank, so it is neither
          // captured here nor left for the enclosing block to claim (§14).
          documentEndsWithLf = false
          done = true
        else if head.blank then
          // Probe across blanks to see if more source follows. The mark must
          // survive several nested fillHead calls so the probe runs inside its
          // own hold; the cursor is free to compact again once the probe ends.
          done = inHold:
            val mk = beginMark()

            val firstBlankSnapshot = (head.leadingSpaces, head.indentLevels, head.blank,
                                      head.eof, head.startLine)

            val savedBoundary = prevLineWasBoundary
            val savedLineNo = lineNo
            var blanks = 0

            while !head.eof && head.blank do
              blanks += 1
              fillHead()

            val keep = !head.eof && head.leadingSpaces >= sourceIndent
            if keep then
              // Interior blanks: defer them until the following non-blank line
              // confirms they are not trailing (§14). Each yields one LF.
              pendingBlanks += blanks
              false
            else
              // Rewind.
              syncTo()
              cursor.cue(mk)
              syncFrom()
              head.leadingSpaces = firstBlankSnapshot._1
              head.indentLevels  = firstBlankSnapshot._2
              head.blank         = firstBlankSnapshot._3
              head.eof           = firstBlankSnapshot._4
              head.startLine     = firstBlankSnapshot._5
              prevLineWasBoundary = savedBoundary
              lineNo = savedLineNo
              var i = 0

              while i < firstBlankSnapshot._1 && more && peek == SP do advance(); i += 1

              true
        else if head.leadingSpaces >= sourceIndent then
          // Read content: skip the first sourceIndent spaces — but wait, the
          // fillHead already consumed all leading spaces. We need to back up
          // and consume only sourceIndent of them, leaving (leadingSpaces -
          // sourceIndent) for the content. Alternative: pad the appended
          // string with the extra leading spaces, since they ARE part of the
          // content (per §14).
          //
          // §14: source-atom payload is each line's content with the first
          // `sourceIndent` characters stripped. So if leadingSpaces > sourceIndent,
          // the excess is part of the content as leading spaces.
          // Since fillHead consumed ALL the leadingSpaces, we lost the
          // (leadingSpaces - sourceIndent) excess. Recompose it.
          // Emit the LF separator before this segment: one LF per deferred
          // interior blank, plus one to separate from the previous segment.
          // The first segment is emitted bare (§14 join semantics, no leading
          // or trailing LF).
          if emitted then
            while pendingBlanks > 0 do { sb.append('\n'); pendingBlanks -= 1 }
            sb.append('\n')
          else
            emitted = true

          val excess = head.leadingSpaces - sourceIndent
          var i = 0
          while i < excess do { sb.append(' '); i += 1 }

          // Now read the line content (until LF/CR), tracking trailing spaces
          // so we can strip them. One hold per payload line so the cursor can
          // compact between lines.
          inHold:
            val mk = beginMark()

            while
              scanUntil2(LF, CR, Parser.LfRepl, Parser.CrRepl)
              pos == bufEnd && more
            do ()
            val payload = sliceText(mk)
            // Strip trailing spaces.
            var endIdx = payload.length
            while endIdx > 0 && payload.charAt(endIdx - 1) == ' ' do endIdx -= 1
            sb.append(payload, 0, endIdx)

            consumeLineEnding()

          fillHead()
        else
          done = true

      Tel.Atom.Source(Text(sb.toString))

    // ── Literal atom ─────────────────────────────────────────────────────────

    // Cursor is at the first content byte of the opening line (the delimiter
    // text). The delimiter is the rest of the opening line. The payload is
    // everything between the next LF and the first line whose content is
    // byte-identical to the opening delimiter line (indentation included).
    private update def parseLiteralAtom(literalIndent: Int)
    :   (Tactic[TelError]^) ?->{this} Tel.Atom.Literal =

      val openingLine = head.startLine
      // Read the delimiter — opening line, terminated by LF or CR per the
      // document's line-ending mode. One hold for the opening line.
      val delimiter: String = inHold:
        val delimMk = beginMark()

        while
          scanUntil2(LF, CR, Parser.LfRepl, Parser.CrRepl)
          pos == bufEnd && more
        do ()
        val d = sliceText(delimMk)
        // The opening line is now consumed up to but not including the LF.
        consumeLineEnding()
        d

      val closingLine = (" "*literalIndent)+delimiter
      sb.setLength(0)
      var done = false

      while !done do
        // §19.5 PayloadToEof: an unterminated literal ends at EOF rather than
        // aborting, so the rest of the document can still be parsed and accrued.
        // The unclosed literal is reported at its opening delimiter, which is the
        // text a caller must fix (by matching it on a closing line).
        if !more
        then done = recoverAt
                     ( Reason.UnclosedLiteral,
                       openingLine,
                       literalIndent + 1,
                       delimiter.length )
                     ( true )
        // Read a line. Inside the literal payload, line endings are not
        // subject to the document's LF/CRLF mode (per §15 the payload is
        // verbatim, with CRLF normalised to LF). One hold per payload line so
        // the cursor can compact between lines.
        else done = inHold:
          val lineMk = beginMark()

          while
            scanUntil1(LF, Parser.LfRepl)
            pos == bufEnd && more
          do ()
          val raw = sliceText(lineMk)
          // The CR-stripped form is used only to recognise the closing
          // delimiter line; the payload itself preserves every byte between
          // structural LFs, including CR (§15).
          val line =
            if raw.length > 0 && raw.charAt(raw.length - 1) == '\r'
            then raw.substring(0, raw.length - 1)
            else raw
          if line == closingLine then
            // Closing delimiter — the LF *before* the closing delimiter is the
            // delimiter's leading separator, not part of the payload. Strip the
            // trailing '\n' we appended for the previous payload line (if any),
            // then consume the optional EOL after the delimiter.
            if sb.length > 0 && sb.charAt(sb.length - 1) == '\n' then sb.setLength(sb.length - 1)

            if more then
              advance()  // consume LF (may be absent at EOF)
              lineNo += 1
              if !more then documentEndsWithLf = true

            true
          else
            // Payload line — must have an LF terminator; EOF here is E115. §19.5
            // PayloadToEof: an unterminated final line ends the payload at EOF.
            if !more then
              sb.append(raw)
              recoverAt(Reason.UnclosedLiteral, openingLine, literalIndent + 1, delimiter.length)
                ( true )
            else
              advance()  // consume LF
              lineNo += 1
              if !more then documentEndsWithLf = true
              sb.append(raw)
              sb.append('\n')
              false

      fillHead()
      // §15: the payload is the verbatim bytes between structural LFs — CR is
      // preserved (only the LF before the closing delimiter is dropped, above).
      Tel.Atom.Literal(Text(delimiter), Text(sb.toString))

    // The length, in the UTF-16 units a `Text` counts, of an arena byte range.
    // Every lead byte contributes one unit, except a four-byte sequence, which
    // is a surrogate pair and contributes two; continuation bytes contribute
    // none. Measuring the arena directly avoids decoding an atom that the parse
    // would not otherwise have decoded. Runs only under `spanTracking`.
    private def arenaCharLength(offset: Int, length: Int): Int =
      var index = 0
      var count = 0

      while index < length do
        val byte = atomArena(offset + index)

        if (byte & 0xC0) != 0x80 then count += (if (byte & 0xF8) == 0xF0 then 2 else 1)
        index += 1

      count

    // ── Compound line parsing ────────────────────────────────────────────────

    // Cursor is at the first content byte (past leading spaces). Reads the
    // compound line's keyword, atoms, and remark, depositing the keyword and
    // remark into `compoundLineKeyword` / `compoundLineRemark`. Atoms are
    // pushed onto the `scratchAtoms` stack; the caller (parseBlock) is
    // responsible for taking them — typically together with an optional
    // source/literal extra atom — and constructing the final Tel.Compound.
    private update def parseCompoundLine(lineNumber: Int): (Tactic[TelError]^) ?->{this} Unit = inHold:
      val isAtColumnZero = head.leadingSpaces == 0
      val mayBeMisplacedPragma = isAtColumnZero && hasConsumedNonBlankLine

      // First phrase = keyword. Read until space or LF/CR.
      val keyword = readKeyword()

      // E102: a `tel` or `tel …` line at column 0 after the first non-blank
      // line is a misplaced pragma. The valid pragma was already consumed
      // earlier by parsePragma; anything matching here is a violation.
      // §19.5 RestartFromPragma: record the misplaced pragma but parse the line as
      // an ordinary compound (the keyword is already read; the rest follows).
      if mayBeMisplacedPragma && keyword == t"tel" then
        recoverAt(Reason.PragmaNotFirst, lineNumber, 1, keyword.s.length)(())

      hasConsumedNonBlankLine = true
      // The value run starts just past the keyword; the scan advances from here.
      if spanTracking then lineValueOrigin = head.leadingSpaces + 1 + keyword.s.length
      parseCompoundLineRest(lineNumber)
      compoundLineKeyword = keyword

    // The remainder of a compound line once the keyword has been consumed:
    // inline atoms (pushed onto `scratchAtoms`), the optional remark
    // (deposited in `compoundLineRemark`), the E108 trailing-space check and
    // the line ending. Factored out of `parseCompoundLine` so the direct
    // parsing rim, which reads the keyword itself (`directKeyword`), can
    // consume the rest of the line through the same scan.
    private update def parseCompoundLineRest(lineNumber: Int)(using Tactic[TelError]): Unit = inHold:
      var remark: Optional[Text] = Unset
      // The running column of the inline-atom run, advanced at each commit by
      // the space run that preceded the atom and then by the atom's own width.
      // Inline atoms are copied from the source verbatim, so this replays the
      // line's layout exactly.
      var valueCursor = lineValueOrigin
      lineValueColumn = 0
      lineValueLength = 0
      // Read atom bytes directly into the parser's atom-bytes arena. With
      // narrow holds, parseCompoundLine's hold has holdStart > 0, so refills
      // inside the line can compact and shift the cursor's `bytes` —
      // we therefore copy bytes out into our own buffer (the arena) as we
      // read them. Each Tel.Atom.Inline references its slice of the arena
      // (arenaArray, offset, length); no per-atom byte[] is allocated.
      var precedingSpaces = 0
      var hardSpaceMode = false
      var atomOpen = false

      inline def commit(): Unit =
        if atomOpen then
          val off = arenaInFlightOffset
          val len = endInFlightAtom()

          if spanTracking then
            valueCursor += precedingSpaces
            if lineValueColumn == 0 then lineValueColumn = valueCursor
            valueCursor += arenaCharLength(off, len)

          if directPrimaryOnly then
            // Consume only the first inline atom, in the reader's requested mode;
            // later atoms on the line are consumed but neither parsed nor allocated.
            if !directPrimaryPresent then
              directPrimaryPresent = true

              directPrimaryMode match
                case PrimaryLong =>
                  directPrimaryOk = parseArenaLong(off, len)
                  // Only a rejected value needs its text — for the `NotScalar`
                  // error, byte-for-byte with the AST path. The happy path
                  // allocates no `String`.
                  if !directPrimaryOk then
                    directPrimaryText = new String(atomArena, off, len, StandardCharsets.UTF_8)

                case PrimaryInt =>
                  // As `PrimaryLong`, but an out-of-`Int`-range (yet valid Long)
                  // value is also a `NotScalar`, exactly as `_.toInt` throws —
                  // so it too captures its text.
                  directPrimaryOk =
                    parseArenaLong(off, len)
                    && directPrimaryLongVal >= Int.MinValue && directPrimaryLongVal <= Int.MaxValue

                  if !directPrimaryOk then
                    directPrimaryText = new String(atomArena, off, len, StandardCharsets.UTF_8)

                case PrimaryBoolean =>
                  if len == 4 && atomArena(off) == 't'.toByte && atomArena(off + 1) == 'r'.toByte
                    && atomArena(off + 2) == 'u'.toByte && atomArena(off + 3) == 'e'.toByte
                  then { directPrimaryBoolVal = true; directPrimaryOk = true }
                  else if len == 5 && atomArena(off) == 'f'.toByte
                    && atomArena(off + 1) == 'a'.toByte && atomArena(off + 2) == 'l'.toByte
                    && atomArena(off + 3) == 's'.toByte && atomArena(off + 4) == 'e'.toByte
                  then { directPrimaryBoolVal = false; directPrimaryOk = true }
                  else
                    directPrimaryOk = false
                    directPrimaryText = new String(atomArena, off, len, StandardCharsets.UTF_8)

                case _ =>
                  directPrimaryText = new String(atomArena, off, len, StandardCharsets.UTF_8)
          else pushAtom(Tel.Atom.Inline.fromArena(atomArena, off, len, precedingSpaces))

          atomOpen = false

      var stopped = false

      while !stopped && remark.absent do
        if !more || peek == LF || peek == CR then stopped = true
        else
          val ch = peek

          if ch == SP then
            var run = 0
            while more && peek == SP do { advance(); run += 1 }

            if hardSpaceMode then
              if run >= 2 then
                commit()
                precedingSpaces = run
              else
                // Single space inside a hard-space-mode atom: the SP byte is
                // part of the atom's content. atomOpen is necessarily already
                // true here (hard-space-mode is only entered after a content
                // commit, and hard-space-mode + run==1 only fires while
                // reading content).
                appendToArena(SP)
                atomOpen = true
            else
              if run == 1 then
                commit()
                precedingSpaces = 1
              else
                commit()
                precedingSpaces = run
                hardSpaceMode = true
          else if ch == sigil && !atomOpen then
            // Could be remark introducer: sigil + soft space + non-space.
            ensureLookahead(3)
            val afterSigil = if pos + 1 < bufEnd then bytes(pos + 1) & 0xff else -1

            val softSpaceAfter =
              afterSigil == SP.toInt && (pos + 2 >= bufEnd || bytes(pos + 2) != SP)
            if softSpaceAfter then
              // Consume sigil + space, then read remark text until LF/CR.
              advance()  // sigil
              advance()  // space
              val mk = beginMark()
              while more && peek != LF && peek != CR do advance()
              remark = Text(sliceText(mk))
            else
              beginInFlightAtom()
              appendToArena(ch)
              advance()
              atomOpen = true
          else
            // Read a run of non-space, non-sigil, non-LF, non-CR bytes,
            // copying them into the parser's atom arena. After a refill
            // compacts the cursor buffer, the source content stays valid
            // because we read it out into our own arena before the next
            // refill can fire.
            val runStart = pos
            while pos < bufEnd &&
              bytes(pos) != SP &&
              bytes(pos) != LF &&
              bytes(pos) != CR &&
              (atomOpen || bytes(pos) != sigil)
            do pos += 1

            val runLen = pos - runStart

            if runLen > 0 then
              if !atomOpen then beginInFlightAtom()
              appendToArenaRange(bytes, runStart, runLen)
              atomOpen = true
            else
              // Defensive: only reachable if the outer guards were ever
              // relaxed. Treat the byte at `pos` as one atom byte.
              if !atomOpen then beginInFlightAtom()
              appendToArena(ch)
              advance()
              atomOpen = true

      commit()

      if spanTracking then
        if lineValueColumn != 0 then lineValueLength = valueCursor - lineValueColumn
        // Tag the measurement with the entry it belongs to, so the direct path
        // can tell whether a focused block read this entry or something deeper.
        directValueSeq = directEntrySeq

      // E108: a non-blank compound line must not end with a space character.
      // Inside the outer `hold`, the buffer byte just before the current pos
      // is still resident — peek it directly. (`pos > 0` because we have
      // consumed at least the keyword.)
      // §19.5 StripTrailing: the keyword/atoms already exclude the trailing space,
      // so recording the error and continuing yields the stripped line.
      // The trailing run itself is the offending text: walk back over it (still
      // inside the hold, so those bytes are resident) to span exactly the spaces.
      if remark.absent && more && (peek == LF || peek == CR) && pos > 0 && bytes(pos - 1) == SP
      then
        var back = pos
        while back > 0 && bytes(back - 1) == SP do back -= 1
        val trailing = pos - back

        recoverAt(Reason.TrailingSpaces, lineNumber, columnForCurrentBytePos() - trailing, trailing)
          ( () )

      consumeLineEnding()
      compoundLineRemark  = remark

    // Read a keyword from the current position. The keyword runs until SP, LF,
    // CR, or EOF. Returns the interned `Text`. Uses the 64-slot fingerprint
    // cache for ≤8-byte keywords (allocation-free on hit). The keyword's
    // start position is remembered via a `cursor.mark` (rather than a raw
    // buffer offset) because the per-leaf hold under which `readKeyword`
    // runs has `holdStart > 0`, so a refill inside the loop can compact the
    // buffer and shift live content toward index 0 — the absolute-position
    // mark survives that, a buffer-offset would not.
    private update def readKeyword()(using Tactic[TelError]): Text =
      val startMark = beginMark()
      var low:  Long = 0L
      var high: Long = 0L
      var len = 0

      while
        while pos < bufEnd && bytes(pos) != SP && bytes(pos) != LF && bytes(pos) != CR do
          val b = bytes(pos)

          if len < 4 then
            low |= (b & 0xff).toLong << (len * 8)
          else if len < 8 then
            high |= (b & 0xff).toLong << ((len - 4) * 8)

          len += 1
          pos += 1

        pos == bufEnd && more
      do ()

      directKeywordLow = low
      directKeywordHigh = high
      directKeywordLen = len

      if len == 0 then t""
      else if len > 8 then
        Text(sliceText(startMark))
      else
        val hash = ((low ^ (low >>> 32)) ^ (high ^ (high >>> 17))).toInt
        var slot = hash & 0x3F
        var probes = 0
        var result: String = null

        while result == null && probes < 4 do
          val existing = keyCache(slot)

          if existing == null then
            val s = sliceText(startMark)
            keyCacheTarget(slot) = s
            keyCacheLowTarget(slot) = low
            keyCacheHighTarget(slot) = high
            result = s
          else if keyCacheLow(slot) == low && keyCacheHigh(slot) == high then
            result = existing
          else
            slot = (slot + 1) & 0x3F
            probes += 1

        if result != null then Text(result) else Text(sliceText(startMark))

    // As `readKeyword`, but for the packed-dispatch step: the keyword is
    // *not* interned or sliced when it packs — the fingerprint the scan
    // computes anyway is the whole result (`directKeywordPacked`), and its
    // text materializes only if `directKeywordText` is later consulted (an
    // opaque general dispatch, an unknown sum variant, an error). A keyword
    // that cannot pack — empty, longer than eight bytes, or carrying bytes
    // outside printable ASCII, whose packed form could alias a shorter
    // keyword's or a sentinel — is sliced eagerly, exactly as `readKeyword`
    // would, and reported `KeywordOpaque`.
    private update def readKeywordFast()(using Tactic[TelError]): Unit =
      // Fused scan-and-pack: the word loaded to find the keyword's end *is*
      // its packed form, so a packable keyword costs one load. A keyword at
      // the window's edge, longer than eight bytes or carrying unpackable
      // bytes takes the per-byte slow path.
      var slow = true

      if pos + 8 <= bufEnd then
        val word: Long = Parser.longView(bytes, pos)
        val stops = Parser.contentStops(word)

        if stops != 0L then
          val len = java.lang.Long.numberOfTrailingZeros(stops) >> 3

          if len >= 1 then
            val packed = word & ((1L << (len*8)) - 1L)

            if Parser.printableWord(packed, len) then
              directKeywordLow = packed & 0xFFFFFFFFL
              directKeywordHigh = packed >>> 32
              directKeywordLen = len
              directKeywordPacked = packed
              directEntryKeywordLazy = true
              pos += len
              slow = false
        else if pos + 8 < bufEnd then
          // No stop in the first eight bytes: exactly eight, or oversized.
          val b8 = bytes(pos + 8)

          if (b8 == SP || b8 == LF || b8 == CR) && Parser.printableWord(word, 8) then
            directKeywordLow = word & 0xFFFFFFFFL
            directKeywordHigh = word >>> 32
            directKeywordLen = 8
            directKeywordPacked = word
            directEntryKeywordLazy = true
            pos += 8
            slow = false

      if slow then readKeywordSlow()

    private update def readKeywordSlow()(using Tactic[TelError]): Unit =
      val startMark = beginMark()
      var low:  Long = 0L
      var high: Long = 0L
      var len = 0

      while
        while pos < bufEnd && bytes(pos) != SP && bytes(pos) != LF && bytes(pos) != CR do
          val b = bytes(pos)

          if len < 4 then
            low |= (b & 0xff).toLong << (len * 8)
          else if len < 8 then
            high |= (b & 0xff).toLong << ((len - 4) * 8)

          len += 1
          pos += 1

        pos == bufEnd && more
      do ()

      directKeywordLow = low
      directKeywordHigh = high
      directKeywordLen = len

      val packed = (low & 0xFFFFFFFFL) | (high << 32)

      if len >= 1 && len <= 8 && Parser.printableWord(packed, len) then
        directKeywordPacked = packed
        directEntryKeywordLazy = true
      else
        directKeywordPacked = TelReader.KeywordOpaque
        directEntryKeywordLazy = false
        directEntryKeyword = if len == 0 then t"" else Text(sliceText(startMark))

    // The lazily-materialized text of a fast-stepped keyword: rebuilt from
    // the fingerprint (byte-exact — the packed bytes are printable ASCII)
    // through the intern cache, so repeated requests share one `String`.
    private update def internFingerprint(): Text =
      val low = directKeywordLow
      val high = directKeywordHigh
      val hash = ((low ^ (low >>> 32)) ^ (high ^ (high >>> 17))).toInt
      var slot = hash & 0x3F
      var probes = 0
      var result: String = null

      while result == null && probes < 4 do
        val existing = keyCache(slot)

        if existing == null then
          val s = fingerprintString()
          keyCacheTarget(slot) = s
          keyCacheLowTarget(slot) = low
          keyCacheHighTarget(slot) = high
          result = s
        else if keyCacheLow(slot) == low && keyCacheHigh(slot) == high then
          result = existing
        else
          slot = (slot + 1) & 0x3F
          probes += 1

      if result == null then result = fingerprintString()
      Text(result)

    private update def fingerprintString(): String =
      val len = directKeywordLen
      val chars = new scala.Array[Char](len)
      var index = 0

      while index < len do
        val byte =
          if index < 4 then (directKeywordLow >>> (index*8)) & 0xFF
          else (directKeywordHigh >>> ((index - 4)*8)) & 0xFF

        chars(index) = byte.toChar
        index += 1

      new String(chars)

    // ── Direct parsing rim ───────────────────────────────────────────────────
    //
    // The token-level surface behind `TelReader`, letting a `Tel.Parsable`
    // instance consume one compound entry at a time without materializing the
    // whole document's AST. The rim follows the AST parser's own recursive
    // structure: `directKeyword` plays the role of `parseChildren`'s dispatch
    // (blanks, comments, tabulation headers and separators are handled exactly
    // as `parseBlock` would), and the per-entry consumers (`directAtomText`,
    // `directValue`, `directSkipEntry`, `directFinishLine`) mirror the single-
    // compound path of `parseBlock`. No §19.5 recovery happens here beyond what
    // the reused helpers already do: an over-indented line fails fast with the
    // same `Reason` the AST path would record.

    // The entry most recently stepped to by `directKeyword`: its leading
    // spaces, indent level, source line and keyword. Consulted by the
    // per-entry consumers, which always run immediately after the
    // `directKeyword` that parked on the entry.
    var directEntrySpaces:  Int = 0
    var directEntryIndent:  Int = 0
    var directEntryLine:    Int = 1
    var directEntryKeyword: Text = t""

    // The stepped-to keyword's length in characters, recorded when it is read
    // (`directKeywordLen` is a byte length, and is only meaningful for the
    // packable keywords the fast step handles).
    var directEntryKeywordLen: Int = 0

    // Fingerprint of the keyword most recently read by `readKeyword` — the
    // packed words it computes anyway for the intern cache (low = bytes 0–3,
    // high = bytes 4–7, LSB-first), and the keyword's byte length.
    var directKeywordLow:  Long = 0L
    var directKeywordHigh: Long = 0L
    var directKeywordLen:  Int = 0

    // The fast step's result — the keyword's packed bytes, or
    // `TelReader.KeywordOpaque` — and whether `directEntryKeyword` is stale,
    // its text pending lazy materialization from the fingerprint.
    var directKeywordPacked:    Long = 0L
    var directEntryKeywordLazy: Boolean = false

    // The tabulation header governing subsequent rows at the same indent, and
    // the classification of the most recent group of consumed lines — the
    // direct-path stand-in for `parseChildren`'s "last block", from which the
    // over-indentation `Reason` is chosen exactly as the AST path chooses it.
    var directTabulation: Optional[Tel.Tabulation] = Unset
    private final val DirectFresh = 0
    private final val DirectComments = 1
    private final val DirectTabulationHeader = 2
    private final val DirectTabulationRows = 3
    private final val DirectCompound = 4
    var directGroup: Int = 0

    // Mirrors `parseChildren`'s reason selection for an over-indented line:
    // the shape of the preceding block determines whether the line is a
    // misplaced row, a child of a non-compound, or plain over-indentation.
    private update def directOverIndentReason: Reason = directGroup match
      case DirectComments         => Reason.ChildOfNonCompound
      case DirectTabulationHeader => Reason.RowWrongIndent
      case DirectTabulationRows   => Reason.ChildOfNonCompound
      case _                      => Reason.OverIndentation

    // The prologue of `parseOneDocument` without its body: BOM, interpreter
    // directive, pragma, and margin determination, leaving `head` parked on
    // the first content line (or EOF). The direct driver runs this before
    // handing the reader to a `Tel.Parsable`.
    private[stratiform] update def directProlog()(using Tactic[TelError]): Unit =
      syncFrom()
      checkBom()
      val directive = parseInterpreterDirective()
      val pragma = parsePragma()
      fillHead()

      if directive.absent && pragma.absent then determineMargin()
      else
        margin = 0
        while head.blank && !head.eof do fillHead()

      directTabulation = Unset
      directGroup = DirectFresh

    // Step to the next compound entry at exactly `indent`, transparently
    // consuming blank lines, comment lines and tabulation headers on the way
    // (with the same side effects and checks as `parseBlock`). Returns null
    // when the entry region ends: EOF, a document separator, a shallower
    // line, or — matching the AST path, which leaves such a tail unconsumed —
    // blank lines followed by a deeper line. A deeper line reached without an
    // intervening blank fails fast with the `Reason` the AST path would
    // record. On a compound line, consumes the keyword (leaving the parser
    // mid-line, right after it) and records the entry state for the
    // per-entry consumers.
    private[stratiform] update def directKeyword(indent: Int)(using Tactic[TelError]): Text | Null =
      if directKeywordAdvance(indent, textual = true) then directEntryKeyword else null

    // The step's shared line-classification loop. With `textual = true` the
    // keyword is read interned (`readKeyword`); otherwise through the
    // pack-only fast scan (`readKeywordFast`), whose text materializes only
    // on demand. Returns whether an entry was found (false once the entry
    // region ends).
    private update def directKeywordAdvance(indent: Int, textual: Boolean)(using Tactic[TelError])
    :   Boolean =
      var result = false
      var done = false

      while !done do
        if head.eof || head.separator then done = true
        else if head.blank then
          // A blank line ends the current block: any active tabulation stops
          // applying to what follows.
          directTabulation = Unset
          while !head.eof && head.blank do fillHead()

          // Blanks followed by a deeper line: the AST path leaves the blanks
          // unclaimed and every `parseChildren` level exits, silently ending
          // the document — mirrored by ending the entry region at every level.
          if !head.eof && !head.separator && head.indentLevels > indent then done = true
        else if head.indentLevels < indent then
          done = true
        else if head.indentLevels > indent then
          errorAt(directOverIndentReason, head.startLine, 1, head.leadingSpaces.max(1))
        // Both comment and tabulation lines open with the sigil, so a
        // compound line — the overwhelmingly common step — skips both
        // classifiers (and their hold ceremony) on one byte test.
        else if more && peek == sigil && isCommentBody() then
          // §9 E109 check, exactly as `parseBlock` performs it before
          // consuming a comment line.
          if !prevLineWasBoundary && prevContentLeadingSpaces >= 0 &&
            prevContentLeadingSpaces >= margin + indent*2
          then recoverAt(Reason.CommentNotPreceded, head.startLine, head.leadingSpaces + 1, 1)(())

          directTabulation = Unset
          directGroup = DirectComments
          parseCommentLine()
          prevLineWasBoundary = true
          hasConsumedNonBlankLine = true
          fillHead()
        else if more && peek == sigil && isTabulationBody() then
          val leadingSpaces = head.leadingSpaces
          directTabulation = Optional(parseTabulationLine())
          directGroup = DirectTabulationHeader
          prevContentLeadingSpaces = leadingSpaces
          prevLineWasBoundary = false
          hasConsumedNonBlankLine = true
          fillHead()
        else
          // A compound line. §16.2 row validation runs before any of the
          // row's bytes are consumed, exactly as in `parseBlock`.
          directTabulation.let: tabulation =>
            validateTabulatedRowInline(head.leadingSpaces, tabulation, head.startLine)

          directEntrySpaces = head.leadingSpaces
          directEntryIndent = head.indentLevels
          directEntryLine = head.startLine
          // A new entry: anything the previous one measured no longer describes
          // where we are.
          directEntrySeq += 1

          inHold:
            val mayBeMisplacedPragma = head.leadingSpaces == 0 && hasConsumedNonBlankLine

            if textual then
              val keyword = readKeyword()

              // E102 / §19.5 RestartFromPragma, as in `parseCompoundLine`.
              if mayBeMisplacedPragma && keyword == t"tel" then
                recoverAt(Reason.PragmaNotFirst, directEntryLine, 1, keyword.s.length)(())

              directEntryKeyword = keyword
              directEntryKeywordLazy = false
              directEntryKeywordLen = keyword.s.length
            else
              readKeywordFast()

              // The same E102 check against the packed form of `tel`.
              if mayBeMisplacedPragma && directKeywordLen == 3 && directKeywordLow == 0x6C6574L
              then recoverAt(Reason.PragmaNotFirst, directEntryLine, 1, directKeywordLen)(())

              // A packable keyword is at most eight printable-ASCII bytes, so
              // its byte length is also its character length.
              directEntryKeywordLen = directKeywordLen

            // The rest of this entry's line, whenever it is consumed, measures
            // its value run from just past the keyword.
            if spanTracking then lineValueOrigin = directEntrySpaces + 1 + directEntryKeywordLen

            hasConsumedNonBlankLine = true

          directGroup =
            if directGroup == DirectTabulationHeader || directGroup == DirectTabulationRows
            then DirectTabulationRows
            else DirectCompound

          result = true
          done = true

      result

    // ── Direct-path spans ────────────────────────────────────────────────────
    //
    // No AST survives a direct parse, so there is nothing to resolve a pointer
    // against afterwards: a focus must take its span while the parser is still
    // on the entry. These are the two regions `locate` / `locateKey` name on the
    // AST path, computed the same way from the same measurements.

    // The current entry's keyword.
    private[stratiform] def directKeywordSpan: Span =
      TelError.spanAt(directEntryLine, directEntrySpaces + 1, directEntryKeywordLen)

    // The current entry's inline-atom run — but only if the extent on record was
    // measured for entry `ordinal`, and not for something read since. Empty
    // otherwise, and empty for an entry with no inline atoms, so the caller
    // falls back to the keyword exactly as `walkIndex` does.
    private[stratiform] def directValueSpan(ordinal: Int): Span =
      if directValueSeq != ordinal || lineValueColumn == 0 then Span.empty
      else TelError.spanAt(directEntryLine, lineValueColumn, lineValueLength)

    // The value span of the entry being read, falling back to its keyword — the
    // span a decode error raised from a leaf instance should carry.
    private[stratiform] def directEntrySpan: Span =
      val value = directValueSpan(directEntrySeq)
      if value.exists then value else directKeywordSpan

    // The keyword most recently stepped to, as interned text — behind
    // `TelReader.keywordText`, for staged parsers whose packed step reported
    // the keyword opaque (and for errors and the AST bridge). A fast-stepped
    // keyword materializes here, lazily, from its fingerprint.
    private[stratiform] update def directKeywordText: Text =
      if directEntryKeywordLazy then
        directEntryKeyword = internFingerprint()
        directEntryKeywordLazy = false

      directEntryKeyword

    // As `directKeyword`, but returning the keyword in packed form for staged
    // parsers that compare keywords against literal constants: the keyword's
    // bytes 0–7, LSB-first (`TelReader.KeywordEnd` once the entry region
    // ends, or `TelReader.KeywordOpaque` for a keyword that cannot pack —
    // empty, longer than eight bytes, or carrying bytes outside printable
    // ASCII, whose packed form could alias a shorter keyword's or a
    // sentinel). An opaque keyword is still consumed: `directKeywordText`
    // identifies it for the caller's general dispatch.
    private[stratiform] update def directKeywordWord(indent: Int)(using Tactic[TelError]): Long =
      if !directKeywordAdvance(indent, textual = false) then TelReader.KeywordEnd
      else directKeywordPacked

    // Consume the current entry in full — line remainder, source/literal
    // continuation, and children — materializing it as the single compound
    // the AST path would have built. The direct seam for every consumer that
    // needs AST-identical semantics.
    private update def directCompound(indent: Int)(using Tactic[TelError]): Tel.Compound =
      val spaces = directEntrySpaces
      val keyword = directKeywordText
      val tabulated = directTabulation.present
      val atomsStart = atomScratchIx
      parseCompoundLineRest(directEntryLine)
      val remark = compoundLineRemark
      prevContentLeadingSpaces = spaces
      prevLineWasBoundary = false
      fillHead()

      // Tabulated rows never carry a source/literal atom or children,
      // exactly as in `parseBlock` (both are gated on `tabulation.absent`).
      val extraAtom: Optional[Tel.Atom] =
        if tabulated then Unset else parseSourceOrLiteralAtomIfPresent(spaces)

      if extraAtom.present then pushAtom(extraAtom.vouch)

      val children: Array[Tel.Block]^{} =
        if !tabulated && extraAtom.absent then parseChildren(indent)
        else EmptyBlocks

      // When children were not parsed (extra atom or tabulated row), a
      // following deeper line is over-indented — the enclosing AST loop
      // would fail on it, so fail here with the same classification.
      if (tabulated || extraAtom.present) && !head.eof && !head.separator && !head.blank &&
        head.indentLevels > indent
      then errorAt(directOverIndentReason, head.startLine, 1, head.leadingSpaces.max(1))

      val atoms = takeAtoms(atomScratchIx - atomsStart)
      Tel.Compound(keyword, atoms, remark, children)

    // Consume the whole entry in the given primary-capture `mode`, leaving the
    // captured value in the mode's slot (`directPrimaryText` /
    // `directPrimaryLongVal` / `directPrimaryBoolVal`, with `directPrimaryPresent`
    // and `directPrimaryOk`). Materializes no `Tel.Compound`, no `Array[Atom]^{}`
    // (`takeAtoms`) and no `Tel.Atom.Inline`: the first inline atom is consumed
    // straight into the slot, and the source/literal continuation and child
    // subtree are consumed (discarded) so the reader lands on the next sibling.
    private update def consumeDirectEntry(mode: Int)(using Tactic[TelError]): Unit =
      val spaces = directEntrySpaces
      val indent = directEntryIndent
      val tabulated = directTabulation.present

      directPrimaryMode = mode
      directPrimaryPresent = false
      directPrimaryOk = false
      directPrimaryText = null

      if !directLeafLine(mode) then
        directPrimaryOnly = true
        try parseCompoundLineRest(directEntryLine) finally directPrimaryOnly = false

      prevContentLeadingSpaces = spaces
      prevLineWasBoundary = false
      fillHead()

      val extraAtom: Optional[Tel.Atom] =
        if tabulated then Unset else parseSourceOrLiteralAtomIfPresent(spaces)

      // §14/§15 append the source/literal atom to the atom sequence: when the
      // line itself carried no inline atom, it is the entry's first atom and
      // supplies the primary.
      if !directPrimaryPresent then extraAtom match
        case Tel.Atom.Source(text)     => capturePrimaryFromText(text.s)
        case Tel.Atom.Literal(_, text) => capturePrimaryFromText(text.s)
        case _                         => ()

      // Mirror `directCompound`: children are parsed (and here discarded) only
      // when no source/literal atom and no tabulation intervened; otherwise a
      // following deeper line is over-indented and fails with the same reason.
      if !tabulated && extraAtom.absent then parseChildren(indent)
      else if !head.eof && !head.separator && !head.blank && head.indentLevels > indent
      then errorAt(directOverIndentReason, head.startLine, 1, head.leadingSpaces.max(1))

    // Parses an optional `-` and one to eighteen digits — exactly the
    // spellings `parseArenaLong` accepts — or Unset.
    private def parsedTextLong(text: String): Optional[Long] =
      val negative = text.startsWith("-")
      val start = if negative then 1 else 0
      val digits = text.length - start

      if digits < 1 || digits > 18 then Unset else
        var index = start
        var accumulator = 0L
        var ok = true

        while ok && index < text.length do
          val ch = text.charAt(index)

          if ch < '0' || ch > '9' then ok = false else
            accumulator = accumulator*10 + (ch - '0')
            index += 1

        if ok then Optional(if negative then -accumulator else accumulator) else Unset

    // Capture the primary slot from a source/literal atom's text, mirroring
    // `parseCompoundLineRest`'s per-mode capture of the first inline atom.
    private update def capturePrimaryFromText(value: String): Unit =
      directPrimaryPresent = true
      directPrimaryOk = false
      directPrimaryText = null

      directPrimaryMode match
        case PrimaryLong =>
          parsedTextLong(value) match
            case parsed: Long =>
              directPrimaryLongVal = parsed
              directPrimaryOk = true

            case _ => directPrimaryText = value

        case PrimaryInt =>
          parsedTextLong(value) match
            case parsed: Long
                 if parsed >= Int.MinValue && parsed <= Int.MaxValue =>
              directPrimaryLongVal = parsed
              directPrimaryOk = true

            case _ => directPrimaryText = value

        case PrimaryBoolean =>
          if value == "true" then
            directPrimaryBoolVal = true
            directPrimaryOk = true
          else if value == "false" then
            directPrimaryBoolVal = false
            directPrimaryOk = true
          else directPrimaryText = value

        case _ =>
          directPrimaryText = value

    // The leaf fast path, entered with the cursor right after the keyword:
    // handles the dominant data shapes — a bare `keyword` line, and
    // `keyword value` with a single inline atom, no remark, no hard spaces
    // and no trailing space — parsing (or slicing) the value straight from
    // the cursor buffer, with no arena copy and no `parseCompoundLineRest`
    // scan. The numeric capture accepts only spellings whose parse is
    // provably identical to `parseArenaLong`'s (an optional `-` and one to
    // eighteen digits); any other line shape rewinds to the line's remainder
    // and takes the general path, so the two routes agree by construction.
    // Returns whether the line was consumed.
    // Inline with an inline `mode`, so each primitive reader expands its own
    // mode-specialized copy — the dead mode arms fold away and the hot
    // methods stay small enough to inline predictably.
    private update def directLeafLine(mode: Int)(using Tactic[TelError]): Boolean = inHold:
      val entry = beginMark()

      if !more then false
      else if peek == LF || peek == CR then
        // A bare keyword line: no inline atoms.
        consumeLineEnding()
        compoundLineRemark = Unset
        true
      else if peek != SP then false
      else
        advance()

        if !more then { rewindTo(entry); false }
        else
          val b1 = peek

          // A second space (hard-space mode), a sigil (a remark, or an atom
          // opening with one) or a line ending (a trailing space) all take
          // the general path.
          if b1 == SP || b1 == LF || b1 == CR || b1 == sigil then
            rewindTo(entry)
            false
          else
            val atomStart = beginMark()
            val startPos = pos

            // Find the atom's end within the buffered window, a word at a
            // time; the window's edge (a refill would be needed) takes the
            // general path.
            var endPos = -1
            var scan = pos

            while endPos < 0 && scan + 8 <= bufEnd do
              val word: Long = Parser.longView(bytes, scan)
              val stops = Parser.contentStops(word)

              if stops != 0L
              then endPos = scan + (java.lang.Long.numberOfTrailingZeros(stops) >> 3)
              else scan += 8

            if endPos < 0 then
              rewindTo(entry)
              false
            else
              pos = endPos

              if peek == SP then
                // More atoms, a remark or a trailing space follow: general.
                rewindTo(entry)
                false
              else
                // A single-atom line, wholly in the window: capture the
                // primary in the requested mode, then consume the ending —
                // `parseCompoundLineRest`'s exit, with no trailing space
                // possible (the atom's last byte precedes the ending).
                val len = endPos - startPos

                def finish(): Boolean =
                  consumeLineEnding()
                  compoundLineRemark = Unset
                  true

                mode match
                  case PrimaryLong | PrimaryInt =>
                    var index = startPos
                    var neg = false

                    if bytes(index) == '-'.toByte then
                      neg = true
                      index += 1

                    // Eighteen digits can never overflow; anything else —
                    // `+`, nineteen digits, non-digits — is
                    // `parseArenaLong`'s to judge, on the general path.
                    val digits = endPos - index
                    var ok = digits >= 1 && digits <= 18
                    var value = 0L

                    while ok && index < endPos do
                      val digit = bytes(index) - '0'.toByte
                      if digit < 0 || digit > 9 then ok = false else
                        value = value*10L + digit
                        index += 1

                    if ok then
                      val signed = if neg then -value else value

                      directPrimaryPresent = true

                      if mode == PrimaryInt
                        && (signed < Int.MinValue.toLong || signed > Int.MaxValue.toLong)
                      then
                        // In `Long` range but not `Int`: `NotScalar`, with
                        // the atom's text for the error.
                        directPrimaryOk = false
                        directPrimaryText = sliceText(atomStart)
                      else
                        directPrimaryOk = true
                        directPrimaryLongVal = signed

                      finish()
                    else
                      rewindTo(entry)
                      false

                  case PrimaryBoolean =>
                    directPrimaryPresent = true

                    if len == 4 && bytes(startPos) == 't'.toByte
                      && bytes(startPos + 1) == 'r'.toByte
                      && bytes(startPos + 2) == 'u'.toByte
                      && bytes(startPos + 3) == 'e'.toByte
                    then
                      directPrimaryOk = true
                      directPrimaryBoolVal = true
                    else if len == 5 && bytes(startPos) == 'f'.toByte
                      && bytes(startPos + 1) == 'a'.toByte
                      && bytes(startPos + 2) == 'l'.toByte
                      && bytes(startPos + 3) == 's'.toByte
                      && bytes(startPos + 4) == 'e'.toByte
                    then
                      directPrimaryOk = true
                      directPrimaryBoolVal = false
                    else
                      directPrimaryOk = false
                      directPrimaryText = sliceText(atomStart)

                    finish()

                  case _ =>
                    directPrimaryPresent = true
                    directPrimaryText = sliceText(atomStart)
                    finish()

    // The entry's primary atom as text — the first atom's, of any presentation
    // form (a source/literal atom supplies it when the line itself carries no
    // inline atom), mirroring `Tel#primaryAtom`. Backs the `Text`/text-codec
    // primitive readers.
    private[stratiform] update def directAtomText()(using Tactic[TelError]): Optional[Text] =
      consumeDirectEntry(PrimaryText)
      val captured = directPrimaryText
      if captured == null then Unset else Optional(Text(captured))

    // The entry's primary atom parsed straight from its bytes as an integer, or
    // Unset for a missing / non-integer atom — the `Int`/`Long` readers, saving
    // the value `String` the `Text` path would materialize only to re-scan.
    private[stratiform] update def directAtomLong()(using Tactic[TelError]): Optional[Long] =
      consumeDirectEntry(PrimaryLong)
      if directPrimaryPresent && directPrimaryOk then Optional(directPrimaryLongVal) else Unset

    // As `directAtomLong`, additionally requiring the value to fit `Int`.
    private[stratiform] update def directAtomInt()(using Tactic[TelError]): Optional[Int] =
      consumeDirectEntry(PrimaryInt)
      if directPrimaryPresent && directPrimaryOk then Optional(directPrimaryLongVal.toInt) else Unset

    // The entry's primary atom parsed straight from its bytes as a boolean, or
    // Unset for a missing atom or anything but `true` / `false`.
    private[stratiform] update def directAtomBoolean()(using Tactic[TelError]): Optional[Boolean] =
      consumeDirectEntry(PrimaryBoolean)
      if directPrimaryPresent && directPrimaryOk then Optional(directPrimaryBoolVal) else Unset

    // Consume the rest of the entry's own line (atoms, remark) and any
    // source/literal continuation, but not its child lines — the step before
    // a nested record parses those children one level deeper.
    private[stratiform] update def directFinishLine()(using Tactic[TelError]): Unit =
      val spaces = directEntrySpaces
      val indent = directEntryIndent
      val tabulated = directTabulation.present

      // A record entry is usually a bare `keyword` line: nothing to scan.
      val bare = inHold:
        if more && (peek == LF || peek == CR) then
          consumeLineEnding()
          compoundLineRemark = Unset
          true
        else false

      if !bare then
        val atomsStart = atomScratchIx
        parseCompoundLineRest(directEntryLine)
        atomScratchIx = atomsStart
      prevContentLeadingSpaces = spaces
      prevLineWasBoundary = false
      fillHead()

      val extraAtom: Optional[Tel.Atom] =
        if tabulated then Unset else parseSourceOrLiteralAtomIfPresent(spaces)

      // After a source/literal atom (or on a tabulated row) the AST path
      // parses no children, so a deeper line is over-indented there; fail
      // the same way rather than letting the nested field loop consume it.
      if (tabulated || extraAtom.present) && !head.eof && !head.separator && !head.blank &&
        head.indentLevels > indent
      then errorAt(directOverIndentReason, head.startLine, 1, head.leadingSpaces.max(1))

    // As `directFinishLine`, but capturing the entry line's atoms — including
    // the source/literal continuation, which §14/§15 append to the atom
    // sequence — for the derived record parser's §19.2 positional pre-pass.
    private[stratiform] update def directFinishLineAtoms()(using Tactic[TelError])
    :   Array[Tel.Atom]^{} =

      val spaces = directEntrySpaces
      val indent = directEntryIndent
      val tabulated = directTabulation.present

      // A record entry is usually a bare `keyword` line: nothing to scan.
      val bare = inHold:
        if more && (peek == LF || peek == CR) then
          consumeLineEnding()
          compoundLineRemark = Unset
          true
        else false

      val atomsStart = atomScratchIx
      if !bare then parseCompoundLineRest(directEntryLine)
      prevContentLeadingSpaces = spaces
      prevLineWasBoundary = false
      fillHead()

      val extraAtom: Optional[Tel.Atom] =
        if tabulated then Unset else parseSourceOrLiteralAtomIfPresent(spaces)

      extraAtom match
        case atom: Tel.Atom => pushAtom(atom)
        case _              => ()

      val atoms = takeAtoms(atomScratchIx - atomsStart)

      // As in `directFinishLine`: after a source/literal atom or on a
      // tabulated row, a deeper line is over-indented.
      if (tabulated || extraAtom.present) && !head.eof && !head.separator && !head.blank &&
        head.indentLevels > indent
      then errorAt(directOverIndentReason, head.startLine, 1, head.leadingSpaces.max(1))

      atoms

    // Skip the whole entry — line remainder, continuation and subtree —
    // discarding it. Used for unknown keywords and duplicate non-repeatable
    // fields (where the AST's `field()` keeps the first match).
    private[stratiform] update def directSkipEntry(indent: Int)(using Tactic[TelError]): Unit =
      directCompound(indent)

    // Materialize this one entry as a `Tel` — the AST bridge for field types
    // that only carry a `Tel.Decodable`.
    private[stratiform] update def directValue(indent: Int)(using Tactic[TelError]): Tel =
      Tel.make(directCompound(indent))

    // Materialize everything that remains as a document-rooted `Tel` — the
    // AST bridge for a whole-input read of a `Decodable`-only type.
    private[stratiform] update def directDocument()(using Tactic[TelError]): Tel =
      Tel.make(Tel.Document(Unset, Unset, lineEndings, parseChildren(-1)))

    // Peek whether the current entry has substance — an atom of any
    // presentation form (§14/§15 append source/literal atoms to the atom
    // sequence), or a child compound beneath it — the exact test the AST's
    // `optionalDecodable` performs. The entry is parsed in full under a mark
    // and then rewound, restoring every piece of parser state the parse
    // touched, so the caller can still consume the entry either way.
    private[stratiform] update def directEntrySubstance()(using Tactic[TelError]): Boolean = inHold:
      val mk = beginMark()

      val savedHead = (head.leadingSpaces, head.indentLevels, head.blank, head.eof,
                       head.startLine, head.separator)

      val savedLineNo = lineNo
      val savedBoundary = prevLineWasBoundary
      val savedContentSpaces = prevContentLeadingSpaces
      val savedNonBlank = hasConsumedNonBlankLine
      val savedEndsLf = documentEndsWithLf
      val savedArenaPos = arenaPos
      val savedInFlight = inFlightStart
      val savedAtomIx = atomScratchIx
      val savedCommentIx = commentScratchIx
      val savedCompoundIx = compoundScratchIx
      val savedBlockIx = blockScratchIx

      val compound = directCompound(directEntryIndent)

      val substance =
        compound.atoms.length > 0 || compound.children.exists(_.compounds.length > 0)

      syncTo()
      cursor.cue(mk)
      syncFrom()
      head.leadingSpaces = savedHead._1
      head.indentLevels  = savedHead._2
      head.blank         = savedHead._3
      head.eof           = savedHead._4
      head.startLine     = savedHead._5
      head.separator     = savedHead._6
      lineNo = savedLineNo
      prevLineWasBoundary = savedBoundary
      prevContentLeadingSpaces = savedContentSpaces
      hasConsumedNonBlankLine = savedNonBlank
      documentEndsWithLf = savedEndsLf
      // The probe's atoms are discarded; only the write positions rewind
      // (atoms committed before the probe are self-contained slices).
      arenaPos = savedArenaPos
      inFlightStart = savedInFlight
      atomScratchIx = savedAtomIx
      commentScratchIx = savedCommentIx
      compoundScratchIx = savedCompoundIx
      blockScratchIx = savedBlockIx
      substance

class Tel private[stratiform]
  ( private[stratiform] val subtree:       Tel.Subtree,
    private[stratiform] val positionIndex: Optional[Tel.PositionIndex] = Unset )
extends scala.Dynamic, Documentary, Topical, Original:
  type Self = Tel
  type Metadata = Tel.Metadata

  // Decode, then locate every focus the decoders accrued against this root —
  // the per-field pointers are built by the derivation and by
  // `Parsable.focusing`; `supplementPositions` supplies their source positions
  // when this document was parsed under `parsing.trackPositions`. (jacinta
  // routes the same enrichment through `distillate.Decodable.position`, which
  // needs a `Locus`-fixing mixin and a lowest-priority adapter given; here the
  // focus type is already fixed by the `tracks Tel.Focus` clause, so the
  // enrichment is called directly.)
  inline def as[value: Decodable in Tel]: value raises TelError tracks Tel.Focus =
    val result = value.decoded(this)
    Tel.supplementPositions(this)
    result

  // Total field access used by the schema-typed navigation macros and by
  // internal optics: an empty `Tel` for a missing field, never raising.
  private[stratiform] def selectField(field: String): Tel =
    val match0 = childCompounds.find(_.keyword == Tel.camelToKebab(field))
    if match0.isEmpty then Tel.empty else Tel(match0.get)

  // Total child-by-index access under a field: an empty `Tel` when the index is
  // out of range. Used by the positional optics / plain dynamic access, where a
  // field compound contains the indexed children.
  private[stratiform] def selectFieldIndex(field: String, index: Int): Tel =
    val cs = selectField(field).childCompounds
    if index < 0 || index >= cs.length then Tel.empty else Tel(cs.readUnchecked(index))

  // Indexed access into a *repeated* (collection) field: a `List`/`Set` field is
  // encoded as repeated keyword compounds (per `#1291`), so index the n-th sibling
  // sharing the field's keyword. Used by schema-typed navigation for collection
  // positions. An empty `Tel` when the index is out of range.
  private[stratiform] def selectRepeatedField(field: String, index: Int): Tel =
    val cs = childCompounds.filter(_.keyword == Tel.camelToKebab(field))
    if index < 0 || index >= cs.length then Tel.empty else Tel(cs.readUnchecked(index))

  // Dynamic field access: `tel.firstName` looks up the kebab-case keyword
  // "first-name". For a schema-typed `Tel of P from R` the macro checks `P` has
  // the field and yields `Tel of <field-type> from R` (no `DynamicTelEnabler`
  // import needed); for a plain `Tel` it requires the enabler, as before.
  transparent inline def selectDynamic(field: String): Tel = ${Stratiform.select('this, 'field)}

  // Chained access: `tel.contacts(0)`. For a schema-typed `Tel of P from R`
  // where the field is a collection, the macro yields `Tel of <element> from R`;
  // for a plain `Tel` it returns the n-th child compound, as before.
  transparent inline def applyDynamic(field: String)(index: Int): Tel =
    ${Stratiform.applied('this, 'field, 'index)}

  // Keyword of this node — empty for the document root, otherwise the
  // compound's keyword text.
  def keyword: Text = subtree match
    case c: Tel.Compound  => c.keyword
    case _: Tel.Document  => t""

  // Flat list of atom texts attached to this node, in atom order: inline
  // atoms first, then the source or literal atom if one follows the line
  // (§14/§15 append it to the same atom sequence). For the document root
  // this is always empty since the root has no atoms.
  def atomTexts: Array[Text]^{} = subtree match
    case c: Tel.Compound =>
      Array.frozen:
        c.atoms.readable.map:
          case Tel.Atom.Inline(text, _)  => text
          case Tel.Atom.Source(text)     => text
          case Tel.Atom.Literal(_, text) => text

    case _: Tel.Document => Array.empty

  // The node's atoms in presentation order (all three forms); empty for the
  // document root.
  private[stratiform] def atoms: Array[Tel.Atom]^{} = subtree match
    case c: Tel.Compound => c.atoms
    case _: Tel.Document => Array.empty

  // First atom text (of any presentation form) or empty string if none.
  // Used by primitive Decodable instances which interpret a compound's
  // first atom as its scalar value.
  def primaryAtom: Text =
    atomTexts.at(denominative.Prim).or(t"")

  // All child compounds, flattened across the node's blocks (presentation-
  // level comments and tabulations are dropped from this view).
  def childCompounds: Array[Tel.Compound]^{} =
    subtree.children.bind(_.compounds)

  // First child compound whose keyword matches `target`, if any.
  def field(target: Text): Optional[Tel] =
    val matched = childCompounds.find(_.keyword == target)
    if matched.isEmpty then Unset else Tel(matched.get)

  // All child compounds whose keyword matches `target`. Useful for
  // schema-repeatable fields that produce multiple compounds with the
  // same keyword in the presentation tree.
  def fields(target: Text): Array[Tel]^{} =
    childCompounds.filter(_.keyword == target).map(Tel(_))

  // Document accessor for downstream operations (printing, mutation). Only
  // meaningful when this Tel wraps a Document.
  private[stratiform] def document: Optional[Tel.Document] = subtree match
    case d: Tel.Document => d
    case _               => Unset

  // Replace (or insert) the child compound with the given field name.
  // Used by the §22 `modify` mutation primitive and the Panopticon Lens
  // given. The new compound carries `value`'s atoms, remark, and
  // children; its keyword is taken from `fieldName` (translated to
  // kebab-case). When the field is missing it is appended to the last
  // block; when present it is replaced in place, preserving surrounding
  // formatting.
  def modify(fieldName: String, value: Tel)(using erased dynamicTelEnabler: DynamicTelEnabler): Tel =
    val name = Tel.camelToKebab(fieldName)

    val newCompound = value.subtree match
      case c: Tel.Compound => c.copy(keyword = name)

      case d: Tel.Document =>
        Tel.Compound(name, Array.empty[Tel.Atom], Unset, d.children)

    val newChildren = Tel.replaceOrAppendCompound(subtree.children, name, newCompound)

    val newSubtree = subtree match
      case d: Tel.Document => d.copy(children = newChildren)
      case c: Tel.Compound => c.copy(children = newChildren)

    Tel.make(newSubtree)
