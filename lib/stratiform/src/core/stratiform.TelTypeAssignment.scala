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
import gossamer.*
import rudiments.*
import vacuous.*

import TelError.Reason
import Tels.*

// Type assignment algorithm per §20.2 of the TEL specification.
// Exposed as `Tel.Type` via singleton alias in object Tel.

object TelTypeAssignment:

  def assign(tel: Tel, schema: Tels): TelElement raises TelError =
    val compounds: IArray[Tel.Compound] = tel.subtree.children.flatMap(_.compounds)
    val rootChildren = assignChildren(compounds, schema.document, schema)
    val rootElements = applyConstraints
                        (schema.document, IArray.empty[TelElement], rootChildren, schema)

    TelElement.Node(keywordIndex = Unset, elementType = schema.document, children = rootElements)

  def assign(tel: Tel, schema: Tels, validators: TelValidator.Registry)
  :     TelElement raises TelError =
    val element = assign(tel, schema)
    validateElement(element, validators)
    element

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

  private def resolveType(t: Type, schema: Tels): Type raises TelError =
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

  private case class KeywordEntry
     ( memberIndex: Int,
       entryType:   Type,
       member:      Member,
       variant:     Optional[Variant] = Unset )

  // Builds a map from keyword Text → KeywordEntry where `memberIndex`
  // is the **flat keyword index** per BinTEL §5: each Field contributes
  // 1 entry, each SelectRef contributes 1 entry per variant in the
  // referenced SelectDefinition. The flat index identifies the unique
  // keyword position in the parent's flat-keyword sequence — used as
  // the `keywordIndex` of every TelElement produced from this parent.
  private def keywordMap(parent: Struct, schema: Tels)
  :     Map[Text, KeywordEntry] raises TelError =
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
  // running flat keyword index (`flatPos`) — TelElement.keywordIndex
  // uses flat positions per BinTEL §5.
  private def assignAtoms
       (atoms:  IArray[Tel.Atom],
        parent: Struct,
        schema: Tels)
  :     IArray[TelElement] raises TelError =
    val results = scala.collection.mutable.ArrayBuffer.empty[TelElement]
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
        else parent.members(pos) match
          case f: Field =>
            resolveType(f.fieldType, schema) match
              case s: Scalar =>
                results += TelElement.Value(flatPos, s, atomText)
                if f.repeatable != Polarity.Loose then
                  flatPos += 1
                  pos += 1
                consumed = true

              case Flag =>
                if atomText == f.keyword then
                  results += TelElement.Node(flatPos, Flag, IArray.empty)
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
                results += TelElement.Node(flatPos + variantOffset, Flag, IArray.empty)
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
       (compounds: IArray[Tel.Compound],
        parent:    Struct,
        schema:    Tels)
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

    IArray.from(results)

  private def applyConstraints
       ( parent:        Struct,
         atomElements:  IArray[TelElement],
         childElements: IArray[TelElement],
         schema:        Tels )
  :     IArray[TelElement] raises TelError =
    val results = scala.collection.mutable.ArrayBuffer.empty[TelElement]
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
            case TelElement.Node(idx, _, _)  => idx == Optional(flatStart)
            case TelElement.Value(idx, _, _) => idx == flatStart

          if !filled then resolveType(f.fieldType, schema) match
            case s: Scalar => f.default match
              case t: Text           => results += TelElement.Value(flatStart, s, t)
              case unset: Unset.type => abort(TelError(Reason.RequiredMemberAbsent))

            case _ => abort(TelError(Reason.RequiredMemberAbsent))

        case _ => ()

      flatStart += width
      memberIdx += 1

    IArray.from(results)

  private def assignCompound
       (compound: Tel.Compound,
        entry:    KeywordEntry,
        schema:   Tels)
  :     TelElement raises TelError =
    val resolved = resolveType(entry.entryType, schema)

    resolved match
      case s: Struct =>
        val atomElements = assignAtoms(compound.atoms, s, schema)
        val childCompounds: IArray[Tel.Compound] = compound.children.flatMap(_.compounds)
        val childElements = assignChildren(childCompounds, s, schema)
        val allElements = applyConstraints(s, atomElements, childElements, schema)
        TelElement.Node(entry.memberIndex, s, allElements)

      case s: Scalar =>
        val text = compound.atoms.headOption.map:
          case Tel.Atom.Inline(t, _)  => t
          case Tel.Atom.Source(t)     => t
          case Tel.Atom.Literal(_, t) => t

        .getOrElse(t"")

        TelElement.Value(entry.memberIndex, s, text)

      case Flag =>
        if compound.atoms.nonEmpty || compound.children.nonEmpty then
          abort(TelError(Reason.FlagWithContent))

        TelElement.Node(entry.memberIndex, Flag, IArray.empty)

      case _: Reference =>
        abort(TelError(Reason.UnresolvedReference))
