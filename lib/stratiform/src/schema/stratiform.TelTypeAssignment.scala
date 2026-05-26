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

import anticipation.*
import contingency.*
import fulminate.*
import rudiments.*
import vacuous.*

import TelError.Reason
import TelSchema.*

// Type assignment algorithm per §20.2 of the TEL specification.
// Translates a Tel.Document presentation tree under a TelSchema into a
// Tel.Element semantic tree. Each compound becomes a Node (Struct- or
// Flag-typed) and each atom-position scalar becomes a Value.
//
// Phase-3 scope: covers Field members with Struct / Scalar / Flag
// types and Reference resolution to RecordDefinitions and
// ScalarDefinitions. SelectRef (sum types), schema layers, validator
// callbacks, and the constraint-violation recovery semantics are
// deferred. The implementation aborts on the first type-assignment
// error rather than accruing.

object TelTypeAssignment:

  // Type-assign without running scalar / struct validators (per §21.4,
  // when no Registry is supplied no E310 is raised).
  def assign(document: Tel.Document, schema: TelSchema)
  :     TelElement raises TelError =
    val compounds: IArray[Tel.Compound] = document.children.flatMap(_.compounds)
    val rootChildren = assignChildren(compounds, schema.document, schema)
    TelElement.Node(keywordIndex = Unset, elementType = schema.document, children = rootChildren)

  // Type-assign AND validate. The Registry is invoked per §21:
  // every Scalar value's validators list is applied to the assigned
  // text; every Struct's validators list is applied to the assigned
  // Node. An Invalid response raises E310.
  def assign
       (document:   Tel.Document,
        schema:     TelSchema,
        validators: TelValidator.Registry)
  :     TelElement raises TelError =
    val element = assign(document, schema)
    validateElement(element, validators)
    element

  // Walk the type-assigned tree post-order, invoking validators for
  // every Scalar value and Struct node.
  private def validateElement
       (element: TelElement, registry: TelValidator.Registry)
  :     Unit raises TelError =
    element match
      case TelElement.Value(_, scalarType, text) =>
        scalarType.validators.each: name =>
          registry(TelValidator.Request.Scalar(name, text)) match
            case TelValidator.Response.Valid      => ()
            case TelValidator.Response.Invalid(_) => abort(TelError(Reason.ValidatorRejected))

      case TelElement.Node(_, elementType, children) =>
        children.each(validateElement(_, registry))
        elementType match
          case s: Struct =>
            s.validators.each: name =>
              registry(TelValidator.Request.Struct
                       (name, element.asInstanceOf[TelElement.Node]))
              match
                case TelValidator.Response.Valid      => ()
                case TelValidator.Response.Invalid(_) => abort(TelError(Reason.ValidatorRejected))

          case _ => ()

  // Resolve a Reference to its concrete Type within the schema. Returns
  // the input type unchanged if not a Reference. Single-step (per
  // §20.2: "Reference and SelectRef resolution are single-step").
  private def resolveType(t: Type, schema: TelSchema): Type raises TelError =
    t match
      case Reference(name) =>
        schema.records.find(_.name == name) match
          case Some(record) => Struct(record.members, record.validators)
          case None         => schema.scalars.find(_.name == name) match
            case Some(scalarDef) => Scalar(scalarDef.validators)
            case None            => schema.selects.find(_.name == name) match
              case Some(_) => abort(TelError(Reason.ReferenceKindMismatch))
              case None    => abort(TelError(Reason.UnresolvedReference))

      case other => other

  // Walk the parent struct's members and produce a flat keyword order:
  // a list of (keyword, member-index, type). For Field members this is
  // a single entry; for SelectRef members one entry per variant (all
  // share the member index but carry the variant's Type and a back-
  // reference to the variant keyword for atom-phase matching).
  private case class KeywordEntry
     ( memberIndex: Int,
       entryType:   Type,
       member:      Member,
       variant:     Optional[Variant] = Unset )

  private def keywordMap(parent: Struct, schema: TelSchema)
  :     Map[Text, KeywordEntry] raises TelError =
    val builder = scala.collection.mutable.LinkedHashMap.empty[Text, KeywordEntry]
    var idx = 0
    while idx < parent.members.length do
      parent.members(idx) match
        case f: Field => builder(f.keyword) = KeywordEntry(idx, f.fieldType, f)
        case s: SelectRef =>
          // Resolve the referenced SelectDefinition and expand its
          // variants into one keyword entry per variant.
          val selectDef = schema.selects.find(_.name == s.reference).getOrElse:
            abort(TelError(Reason.UnresolvedReference))

          var v = 0
          while v < selectDef.variants.length do
            val variant = selectDef.variants(v)
            builder(variant.keyword) =
              KeywordEntry(idx, variant.variantType, s, Optional(variant))

            v += 1

        case _: Exclude => () // Exclude is layer-only

      idx += 1

    builder.toMap

  // Atom-assignable check (after Reference resolution).
  private def atomAssignable(member: Member, schema: TelSchema): Boolean raises TelError =
    member match
      case f: Field =>
        resolveType(f.fieldType, schema) match
          case _: Scalar => true
          case Flag      => true
          case _         => false

      case s: SelectRef =>
        // SelectRef is atom-assignable iff every variant of the
        // referenced SelectDefinition resolves to Flag (§20).
        val selectDef = schema.selects.find(_.name == s.reference).getOrElse:
          abort(TelError(Reason.UnresolvedReference))

        selectDef.variants.forall: v =>
          resolveType(v.variantType, schema) match
            case Flag => true
            case _    => false

      case _: Exclude => false

  // The atom phase of §20.2: walk the parent compound's atoms in order,
  // advancing through the member list and assigning each atom to the
  // next atom-assignable member.
  private def assignAtoms
       (atoms:  IArray[Tel.Atom],
        parent: Struct,
        schema: TelSchema)
  :     IArray[TelElement] raises TelError =
    val results = scala.collection.mutable.ArrayBuffer.empty[TelElement]
    var pos = 0
    var i = 0

    while i < atoms.length do
      // Advance past non-atom-assignable members.
      while pos < parent.members.length && !atomAssignable(parent.members(pos), schema)
      do pos += 1

      if pos >= parent.members.length then abort(TelError(Reason.TooManyAtoms))

      val member = parent.members(pos)
      val atomText = atoms(i) match
        case Tel.Atom.Inline(t, _)  => t
        case Tel.Atom.Source(t)     => t
        case Tel.Atom.Literal(_, t) => t

      member match
        case f: Field =>
          resolveType(f.fieldType, schema) match
            case s: Scalar =>
              results += TelElement.Value(pos, s, atomText)
              if f.repeatable != Polarity.Loose then pos += 1

            case Flag =>
              if atomText == f.keyword then
                results += TelElement.Node(pos, Flag, IArray.empty)
                pos += 1
              else abort(TelError(Reason.AtomFlagKeywordMismatch))

            case _ => abort(TelError(Reason.AtomAtNonAssignablePos))

        case s: SelectRef =>
          // All-Flag SelectRef: atom text must match one of the
          // referenced SelectDefinition's variant keywords.
          val selectDef = schema.selects.find(_.name == s.reference).getOrElse:
            abort(TelError(Reason.UnresolvedReference))

          selectDef.variants.find(_.keyword == atomText) match
            case Some(_) =>
              results += TelElement.Node(pos, Flag, IArray.empty)
              if s.repeatable != Polarity.Loose then pos += 1

            case None => abort(TelError(Reason.AtomVariantUnmatched))

        case _ => abort(TelError(Reason.AtomAtNonAssignablePos))

      i += 1

    IArray.from(results)

  // The compound-child phase of §20.2. Walks compound children, looks
  // up each keyword in the parent's keyword map, recurses according to
  // the child's resolved type, and emits an Element per child.
  private def assignChildren
       (compounds: IArray[Tel.Compound],
        parent:    Struct,
        schema:    TelSchema)
  :     IArray[TelElement] raises TelError =
    val km = keywordMap(parent, schema)
    val results = scala.collection.mutable.ArrayBuffer.empty[TelElement]
    var i = 0

    while i < compounds.length do
      val compound = compounds(i)
      val entry = km.get(compound.keyword).getOrElse:
        abort(TelError(Reason.UnknownKeyword))

      results += assignCompound(compound, entry, schema)
      i += 1

    // Constraint check: every required member must have at least one
    // assigned child (or a default value on a Scalar Field). Defaults
    // are realised as synthetic Value elements at the member's position.
    var memberIdx = 0
    while memberIdx < parent.members.length do
      parent.members(memberIdx) match
        case f: Field if f.required != Polarity.Loose =>
          val filled = results.exists:
            case TelElement.Node(idx, _, _)  => idx == Optional(memberIdx)
            case TelElement.Value(idx, _, _) => idx == memberIdx

          if !filled then resolveType(f.fieldType, schema) match
            case s: Scalar =>
              f.default match
                case t: Text             => results += TelElement.Value(memberIdx, s, t)
                case unset: Unset.type   => abort(TelError(Reason.RequiredMemberAbsent))

            case _ => abort(TelError(Reason.RequiredMemberAbsent))

        case _ => ()

      memberIdx += 1

    IArray.from(results)

  // Assign a type to a single compound child and recurse into its body
  // (atoms first, then sub-compounds).
  private def assignCompound
       (compound: Tel.Compound,
        entry:    KeywordEntry,
        schema:   TelSchema)
  :     TelElement raises TelError =
    val resolved = resolveType(entry.entryType, schema)

    resolved match
      case s: Struct =>
        val atomElements = assignAtoms(compound.atoms, s, schema)
        val childCompounds: IArray[Tel.Compound] = compound.children.flatMap(_.compounds)
        val childElements = assignChildren(childCompounds, s, schema)
        TelElement.Node(entry.memberIndex, s, atomElements ++ childElements)

      case s: Scalar =>
        val text = compound.atoms.headOption.map:
          case Tel.Atom.Inline(t, _)  => t
          case Tel.Atom.Source(t)     => t
          case Tel.Atom.Literal(_, t) => t

        .getOrElse(Text(""))

        TelElement.Value(entry.memberIndex, s, text)

      case Flag =>
        if compound.atoms.nonEmpty || compound.children.nonEmpty then
          abort(TelError(Reason.FlagWithContent))

        TelElement.Node(entry.memberIndex, Flag, IArray.empty)

      case _: Reference =>
        // resolveType is single-step; nested references shouldn't reach here.
        abort(TelError(Reason.UnresolvedReference))
