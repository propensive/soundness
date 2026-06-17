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

import scala.language.dynamics

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import zephyrine.*

// Presentation model from §17 of the TEL specification. The Scala AST is
// structurally identical to the reference implementation's AST so that
// upstream `.check` fixtures round-trip through a cross-language CheckTree
// (see stratiform.CheckFormat).
//
// `Tel` itself is a thin wrapper around a `Subtree` (either the document
// root or a single compound). Both Subtree variants share a `children:
// IArray[Block]` field, so flat traversal logic can address either form
// without case analysis.

object Tel extends Tel2:

  // Consolidated names for the schema-related types. `TelError` lives
  // at the top level by design (the other types are defined inline in
  // `object Tel` below as their canonical location).
  type Error = TelError
  val Error: TelError.type = TelError

  // The focus carried by `Tel#as[T]` for multi-error accrual: a keyword path
  // identifying the field being decoded (and, for parser-phase errors, an
  // optional source position). Mirrors `Json.Focus` / `Yaml.Focus`.
  case class Focus(pointer: TelPath = TelPath.Root, position: Optional[TelError.Position] = Unset)
  derives CanEqual

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

  object Encodable:
    def apply[value](shape0: => Morphology)(lambda: value => Tel): value is Tel.Encodable =
      new Tel.Encodable:
        type Self = value
        def encoded(value: value): Tel = lambda(value)
        def shape(): Morphology = shape0

  // A TEL encoder/decoder that also carries the format-neutral `Morphology` of exactly
  // what it reads/writes, so a fused `Encodable & Schematic` / `Decodable &
  // Schematic` (built by `telSchematics`) is coherent by construction — the shape
  // travels with the codec rather than being resolved independently. The `Morphology` is
  // reified into a concrete `Tels.Type` downstream. Mirrors jacinta's
  // `Json.Encodable`/`Json.Decodable`. Not itself `Schematic`.
  trait Encodable extends anticipation.Encodable:
    type Form = Tel
    def shape(): Morphology

  object Decodable:
    def apply[value](shape0: => Morphology)(lambda: Tel => value): value is Tel.Decodable =
      new Tel.Decodable:
        type Self = value
        def decoded(tel: Tel): value = lambda(tel)
        def shape(): Morphology = shape0

  trait Decodable extends distillate.Decodable:
    type Form = Tel
    def shape(): Morphology

    // True for collection decoders (`List`/`Set`): a repeated field. The product
    // decoder gathers all same-keyword sibling compounds for such a field and hands
    // them here as a Document; other fields read a single matching compound.
    def repeatable: Boolean = false

  // Type assignment algorithm per §20.2 of the TEL specification.
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

    def assign(tel: Tel, schema: Tels): Tel.Element raises TelError tracks Tel.Focus =
      val compounds: IArray[Tel.Compound] = tel.subtree.children.flatMap(_.compounds)
      val rootChildren = assignChildren(compounds, schema.document, schema)

      val rootElements =
        applyConstraints(schema.document, IArray.empty[Tel.Element], rootChildren, schema)

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
      ( memberIndex: Int,
        entryType:   Type,
        member:      Member,
        variant:     Optional[Variant] = Unset )

    // Builds a map from keyword Text → KeywordEntry where `memberIndex`
    // is the **flat keyword index** per BinTEL §5: each Field
    // contributes 1 entry, each SelectRef contributes 1 entry per
    // variant in the referenced SelectDefinition. The flat index
    // identifies the unique keyword position in the parent's flat-
    // keyword sequence — used as the `keywordIndex` of every
    // Tel.Element produced from this parent.
    private def keywordMap(parent: Struct, schema: Tels)
    :   Map[Text, KeywordEntry] raises TelError =

      val builder = scala.collection.mutable.LinkedHashMap.empty[Text, KeywordEntry]
      var idx = 0
      var flatIdx = 0

      while idx < parent.members.length do
        parent.members(idx) match
          case f: Field =>
            builder(f.keyword) = KeywordEntry(flatIdx, f.fieldType, f)
            flatIdx += 1

          case s: SelectRef =>
            val selectDef = schema.selects.find(_.name == s.reference).getOrElse:
              abort(TelError(Reason.UnresolvedReference))

            var v = 0

            while v < selectDef.variants.length do
              val variant = selectDef.variants(v)

              builder(variant.keyword) =
                KeywordEntry(flatIdx + v, variant.variantType, s, Optional(variant))

              v += 1

            flatIdx += selectDef.variants.length

          case _: Exclude => ()

        idx += 1

      builder.toMap

    private def atomAssignable(member: Member, schema: Tels): Boolean raises TelError =
      member match
        case f: Field =>
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

    // Track both the member position (`pos` in parent.members) and the
    // running flat keyword index (`flatPos`) — Tel.Element.keywordIndex
    // uses flat positions per BinTEL §5.
    private def assignAtoms
      ( atoms:  IArray[Tel.Atom],
       parent: Struct,
       schema: Tels )
    :   IArray[Tel.Element] raises TelError =

      val results = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      var pos = 0
      var flatPos = 0
      var i = 0

      def flatWidthOf(member: Member): Int = member match
        case _: Field    => 1
        case _: Exclude  => 0

        case s: SelectRef =>
          schema.selects.find(_.name == s.reference) match
            case Some(selectDef) => selectDef.variants.length
            case None            => 0

      while i < atoms.length do
        val atomText = atoms(i) match
          case Tel.Atom.Inline(t, _)  => t
          case Tel.Atom.Source(t)     => t
          case Tel.Atom.Literal(_, t) => t

        var consumed = false

        while !consumed && pos < parent.members.length do
          if !atomAssignable(parent.members(pos), schema) then
            flatPos += flatWidthOf(parent.members(pos))
            pos += 1
          else
            parent.members(pos) match
              case f: Field =>
                resolveType(f.fieldType, schema) match
                  case s: Scalar =>
                    results += Tel.Element.Value(flatPos, s, atomText)

                    if f.repeatable != Polarity.Loose then
                      flatPos += 1
                      pos += 1

                    consumed = true

                  case Flag =>
                    if atomText == f.keyword then
                      results += Tel.Element.Node(flatPos, Flag, IArray.empty)
                      flatPos += 1
                      pos += 1
                      consumed = true
                    else
                      flatPos += 1
                      pos += 1

                  case _ => abort(TelError(Reason.AtomAtNonAssignablePos))

              case s: SelectRef =>
                val selectDef = schema.selects.find(_.name == s.reference).getOrElse:
                  abort(TelError(Reason.UnresolvedReference))

                selectDef.variants.zipWithIndex.find(_._1.keyword == atomText) match
                  case Some((_, variantOffset)) =>
                    results += Tel.Element.Node(flatPos + variantOffset, Flag, IArray.empty)

                    if s.repeatable != Polarity.Loose then
                      flatPos += selectDef.variants.length
                      pos += 1

                    consumed = true

                  case None =>
                    flatPos += selectDef.variants.length
                    pos += 1

              case _ =>
                flatPos += flatWidthOf(parent.members(pos))
                pos += 1

        if !consumed then abort(TelError(Reason.AtomFlagKeywordMismatch))
        i += 1

      IArray.from(results)

    private def assignChildren
      ( compounds: IArray[Tel.Compound],
       parent:    Struct,
       schema:    Tels )
    :   IArray[Tel.Element] raises TelError tracks Tel.Focus =

      val km = keywordMap(parent, schema)
      val results = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      var i = 0

      while i < compounds.length do
        val compound = compounds(i)

        // An unrecognised keyword is skipped (`IgnoreErroneousNode`): record it and
        // emit no element, so remaining siblings are still validated.
        km.get(compound.keyword) match
          case Some(entry) => results += assignCompound(compound, entry, schema)
          case None        => recoverNode(Reason.UnknownKeyword)(())

        i += 1

      IArray.from(results)

    private def applyConstraints
      ( parent:        Struct,
        atomElements:  IArray[Tel.Element],
        childElements: IArray[Tel.Element],
        schema:        Tels )
    :   IArray[Tel.Element] raises TelError tracks Tel.Focus =

      val results = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
      results ++= atomElements
      results ++= childElements

      def flatWidth(member: Member): Int = member match
        case _: Field   => 1
        case _: Exclude => 0

        case s: SelectRef =>
          schema.selects.find(_.name == s.reference) match
            case Some(sd) => sd.variants.length
            case None     => 0

      var memberIdx = 0
      var flatStart = 0

      while memberIdx < parent.members.length do
        val width = flatWidth(parent.members(memberIdx))

        parent.members(memberIdx) match
          case f: Field if f.required != Polarity.Loose =>
            val filled = results.exists:
              case Tel.Element.Node(idx, _, _)  => idx == Optional(flatStart)
              case Tel.Element.Value(idx, _, _) => idx == flatStart

            if !filled then resolveType(f.fieldType, schema) match
              case s: Scalar => f.default match
                case t: Text           => results += Tel.Element.Value(flatStart, s, t)
                case unset: Unset.type => recoverNode(Reason.RequiredMemberAbsent)(())

              case _ => recoverNode(Reason.RequiredMemberAbsent)(())

          case _ => ()

        flatStart += width
        memberIdx += 1

      IArray.from(results)

    private def assignCompound
      ( compound: Tel.Compound,
       entry:    KeywordEntry,
       schema:   Tels )
    :   Tel.Element raises TelError tracks Tel.Focus =

      val resolved = resolveType(entry.entryType, schema)

      resolved match
        case s: Struct =>
          val atomElements = assignAtoms(compound.atoms, s, schema)
          val childCompounds: IArray[Tel.Compound] = compound.children.flatMap(_.compounds)
          val childElements = assignChildren(childCompounds, s, schema)
          val allElements = applyConstraints(s, atomElements, childElements, schema)
          Tel.Element.Node(entry.memberIndex, s, allElements)

        case s: Scalar =>
          val text = compound.atoms.headOption.map:
            case Tel.Atom.Inline(t, _)  => t
            case Tel.Atom.Source(t)     => t
            case Tel.Atom.Literal(_, t) => t

          .getOrElse(t"")

          Tel.Element.Value(entry.memberIndex, s, text)

        case Flag =>
          if compound.atoms.nonEmpty || compound.children.nonEmpty then
            recoverNode(Reason.FlagWithContent)(())

          Tel.Element.Node(entry.memberIndex, Flag, IArray.empty)

        case _: Reference =>
          recoverNode(Reason.UnresolvedReference):
            Tel.Element.Node(entry.memberIndex, Flag, IArray.empty)

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
        children:     IArray[Element] )
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

    val Empty: Pointer = Pointer(IArray.empty)

    def of(keywords: Text*): Pointer =
      Pointer(IArray.from(keywords.map(Step(_, Unset))))

  // Logical address into a Tel document. A pointer is an ordered
  // sequence of `Step`s; each step selects a child compound from the
  // current node by keyword. When several siblings share a keyword
  // the optional `index` disambiguates them — `Unset` means "the
  // first" (index 0).
  case class Pointer(steps: IArray[Pointer.Step]):
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
    def children: IArray[Block]

  case class Document
    ( interpreterDirective: Optional[Text],
      pragma:               Optional[Pragma],
      lineEndings:          LineEndings,
      children:             IArray[Block] )
  extends Subtree

  case class Block
    ( comments:           IArray[Comment],
      tabulation:         Optional[Tabulation],
      compounds:          IArray[Compound],
      trailingBlankLines: Int )

  case class Comment(text: Text)

  case class Tabulation(markerOffsets: IArray[Int], headings: IArray[Text])

  case class Compound
    ( keyword:  Text,
      atoms:    IArray[Atom],
      remark:   Optional[Text],
      children: IArray[Block] )
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
        ( arena: Array[Byte], off: Int, len: Int, precedingSpaces: Int )
      :   Inline =

        new Inline(arena, off, len, null, precedingSpaces)

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
      ( private val bytes:           Array[Byte] | Null,
        private val byteOff:         Int,
        private val byteLen:         Int,
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
    Tel(TelParser.parse(bytes))

  // Schema-aware parse: the parser uses the §19.5 schema-aware E107
  // recovery rule when it encounters an odd-indented line. Useful
  // when the consumer wants tolerant parsing of indentation typos
  // against a known schema.
  private[stratiform] def parse(bytes: Data, schema: Tels): Tel raises TelError =
    Tel(TelParser.parse(bytes, schema))

  // Parse a multi-document source (§6.1) into its sequence of documents.
  // `parseAll` is eager; `parseStream` parses lazily on demand. Used internally
  // by the collection Aggregable typeclasses; user code should prefer
  // `bytes.read[List[Tel]]` or `bytes.read[Stream[Tel]]`.
  private[stratiform] def parseAll(bytes: Data): List[Tel] raises TelError =
    TelParser.parseDocuments(bytes).map(Tel(_))

  private[stratiform] def parseStream(bytes: Data): Stream[Tel] raises TelError =
    TelParser.parseStream(bytes).map(Tel(_))

  // Concatenate the chunks of a `Stream[Data]` source into a single byte array.
  private def concatenate(source: Stream[Data]): Data =
    import denominative.nil
    var acc    = IArray.empty[Byte]
    var stream = source

    while !stream.nil do
      acc = acc ++ stream.head
      stream = stream.tail

    acc

  // `bytes.read[Tel]` for any Stream[Data] source: concatenates the
  // chunks and parses the result. The metadata (interpreter directive,
  // pragma, line-endings) is *not* surfaced — use `.load[Tel]` to
  // recover those alongside the value. Per §6.1, single-document parsing
  // stops at the first document separator; content after it is ignored.
  given aggregable: Tactic[TelError] => Tel is Aggregable by Data =
    source => parse(concatenate(source))

  // `source.read[Foo in Tel]` shorthand for
  // `source.read[Tel].as[Foo]`. Mirrors `jacinta`'s `aggregableDirect`
  // for `value in Json`. The `Form` type-tag is added by an
  // `asInstanceOf` cast — `value in Tel` is just
  // `value { type Form = Tel }` so the cast is a no-op at runtime.
  given aggregableIn: [value: distillate.Decodable in Tel] => Tactic[TelError]
  =>  (value in Tel) is Aggregable by Data =
    source => parse(concatenate(source)).as[value].asInstanceOf[value in Tel]

  // `source.read[List[Tel]]` / `read[Stream[Tel]]` for a multi-document source
  // (§6.1). `List[Tel]` parses every document eagerly; `Stream[Tel]` parses
  // them lazily on demand (the more specific instance wins over turbulence's
  // generic `Stream` Aggregable, which would otherwise wrap the whole source as
  // a single element).
  given listAggregable: Tactic[TelError] => List[Tel] is Aggregable by Data =
    source => parseAll(concatenate(source))

  given streamAggregable: Tactic[TelError] => Stream[Tel] is Aggregable by Data =
    source => parseStream(concatenate(source))

  // `text.load[Tel]` for any Stream[Text] source: concatenates the
  // chunks, UTF-8 encodes, parses, and pairs the resulting Tel with a
  // `Tel.Metadata` carrying the document's prologue.
  given loadable: Tactic[TelError] => Tel is Loadable by Text = stream =>
    import denominative.nil
    val builder = new StringBuilder()
    var s = stream

    while !s.nil do
      builder.append(s.head.s)
      s = s.tail

    val text = builder.toString
    val bytes = text.getBytes("UTF-8").nn
    val doc = TelParser.parse(IArray.unsafeFromArray(bytes))
    val meta = Tel.Metadata(doc.interpreterDirective, doc.pragma, doc.lineEndings)
    turbulence.Document(Tel(doc): Tel, meta)

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
              val seg = sourceText.substring(start, end)
              out(if seg.isEmpty then "" else sourcePad + seg)
              if nl < 0 then start = sourceText.length + 1 else start = nl + 1

          case Tel.Atom.Literal(delimiter, text) =>
            out("  "*(indent + 3) + delimiter.s)
            val payload = text.s
            var start = 0

            while start <= payload.length do
              val nl = payload.indexOf('\n', start)
              val end = if nl < 0 then payload.length else nl
              out(payload.substring(start, end))
              if nl < 0 then start = payload.length + 1 else start = nl + 1

            out(delimiter.s)

          case _ => ()

        compound.children.each(emitBlock(_, indent + 1, sigil))

      def emitBlock(block: Tel.Block, indent: Int, sigil: Char): Unit =
        val pad = "  "*indent

        block.comments.each: comment =>
          val text = comment.text.s

          if text.isEmpty then out(s"$pad$sigil") else out(s"$pad$sigil $text")

        block.tabulation.let: tab =>
          val line = StringBuilder()
          var i = 0

          while i < tab.markerOffsets.length do
            val targetCol = tab.markerOffsets(i)
            while line.length < targetCol do line.append(' ')
            line.append(sigil)
            val heading = tab.headings(i).s

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

      document.interpreterDirective.let: payload =>
        out("#!" + payload.s)

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
    ( blocks: IArray[Block], keyword: Text, compound: Compound )
  :   IArray[Block] =

    var b = 0
    var found = false
    val out = scala.collection.mutable.ArrayBuffer.from(blocks.toList)

    while b < out.length && !found do
      val cs = out(b).compounds
      val idx = cs.indexWhere(_.keyword == keyword)

      if idx >= 0 then
        out(b) = out(b).copy(compounds = cs.updated(idx, compound))
        found = true

      b += 1

    if !found then
      if out.isEmpty then out += Block(IArray.empty, Unset, IArray(compound), 0)
      else
        val lastIdx = out.length - 1
        out(lastIdx) = out(lastIdx).copy(compounds = out(lastIdx).compounds :+ compound)

    IArray.from(out)

  // Replace the `index`-th child compound (flattened across blocks) by applying
  // `transform`, preserving block structure and surrounding formatting. An
  // out-of-range index leaves the children unchanged. Used by the panopticon
  // `Ordinal` optic.
  private[stratiform] def withChildCompound
    ( blocks: IArray[Block], index: Int, transform: Compound => Compound )
  :   IArray[Block] =

    var offset = 0

    blocks.map: block =>
      val compounds = block.compounds
      val base = offset
      offset += compounds.length

      if index >= base && index < base + compounds.length
      then
        block.copy(compounds = compounds.updated(index - base, transform(compounds(index - base))))
      else
        block

  // Apply `transform` to every child compound (flattened across blocks),
  // preserving block structure. Used by the panopticon `Each` optic.
  private[stratiform] def mapChildCompounds(blocks: IArray[Block], transform: Compound => Compound)
  :   IArray[Block] =

    blocks.map: block => block.copy(compounds = block.compounds.map(transform))

class Tel private[stratiform](private[stratiform] val subtree: Tel.Subtree)
extends scala.Dynamic, Documentary, Topical, Original:
  type Self = Tel
  type Metadata = Tel.Metadata

  inline def as[value: Decodable in Tel]: value raises TelError tracks Tel.Focus =
    value.decoded(this)

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
    if index < 0 || index >= cs.length then Tel.empty else Tel(cs(index))

  // Indexed access into a *repeated* (collection) field: a `List`/`Set` field is
  // encoded as repeated keyword compounds (per `#1291`), so index the n-th sibling
  // sharing the field's keyword. Used by schema-typed navigation for collection
  // positions. An empty `Tel` when the index is out of range.
  private[stratiform] def selectRepeatedField(field: String, index: Int): Tel =
    val cs = childCompounds.filter(_.keyword == Tel.camelToKebab(field))
    if index < 0 || index >= cs.length then Tel.empty else Tel(cs(index))

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

  // Flat list of inline atom texts attached to this node. For the document
  // root this is always empty since the root has no atoms.
  def atomTexts: IArray[Text] = subtree match
    case c: Tel.Compound => c.atoms.collect { case Tel.Atom.Inline(text, _) => text }
    case _: Tel.Document => IArray.empty

  // First inline atom text or empty string if none. Used by primitive
  // Decodable instances which interpret a compound's first atom as its
  // scalar value.
  def primaryAtom: Text =
    if atomTexts.isEmpty then t"" else atomTexts(0)

  // All child compounds, flattened across the node's blocks (presentation-
  // level comments and tabulations are dropped from this view).
  def childCompounds: IArray[Tel.Compound] =
    subtree.children.flatMap(_.compounds)

  // First child compound whose keyword matches `target`, if any.
  def field(target: Text): Optional[Tel] =
    val matched = childCompounds.find(_.keyword == target)
    if matched.isEmpty then Unset else Tel(matched.get)

  // All child compounds whose keyword matches `target`. Useful for
  // schema-repeatable fields that produce multiple compounds with the
  // same keyword in the presentation tree.
  def fields(target: Text): IArray[Tel] =
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
  def modify(fieldName: String, value: Tel)(using erased DynamicTelEnabler): Tel =
    val name = Tel.camelToKebab(fieldName)

    val newCompound = value.subtree match
      case c: Tel.Compound => c.copy(keyword = name)

      case d: Tel.Document =>
        Tel.Compound(name, IArray.empty[Tel.Atom], Unset, d.children)

    val newChildren = Tel.replaceOrAppendCompound(subtree.children, name, newCompound)

    val newSubtree = subtree match
      case d: Tel.Document => d.copy(children = newChildren)
      case c: Tel.Compound => c.copy(children = newChildren)

    Tel.make(newSubtree)
