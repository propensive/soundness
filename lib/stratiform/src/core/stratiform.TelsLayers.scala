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

import TelError.Reason
import Tels.*

// Layer composition per §20.3. Takes a base schema and applies its
// ordered layer list, producing a flat composed Tels. The merge
// preserves subtyping: tightening polarity and excluding sum variants
// are allowed; loosening or widening is rejected (E215/E216/E214).

object TelsLayers:

  // Top-level entry: applies every layer in `schema.layers` to the
  // schema's base, returning a composed Schema with empty `layers`.
  def compose(schema: Tels): Tels raises TelError =
    if schema.layers.isEmpty then schema
    else
      val seenLayerNames = scala.collection.mutable.HashSet.empty[Text]
      var composed = schema.copy(layers = IArray.empty)
      var i = 0
      while i < schema.layers.length do
        val layer = schema.layers(i)
        if !seenLayerNames.add(layer.name) then abort(TelError(Reason.DuplicateLayerName))
        composed = applyLayer(composed, layer)
        i += 1

      composed

  // Apply a single layer to the (partially composed) schema. The order
  // of operations matters: Definitions are merged first so any
  // Reference resolved later sees the merged namespace.
  private def applyLayer(base: Tels, layer: Layer): Tels raises TelError =
    val mergedRecords = mergeRecordList(base.records, layer.records, base.scalars, base.selects)
    val mergedScalars = mergeScalarList(base.scalars, layer.scalars, mergedRecords, base.selects)
    val mergedSelects = mergeSelectList(base.selects, layer.selects, mergedRecords, mergedScalars)
    val mergedDocument = mergeStruct(base.document, layer.overlay)

    base.copy
     ( document = mergedDocument,
       records  = mergedRecords,
       scalars  = mergedScalars,
       selects  = mergedSelects )

  // MergePolarity per §20.3: tightening is allowed; loosening on an
  // already-set axis is rejected. `axis` is "required" or "repeatable"
  // and is only used for which error code to raise (E215 vs E216).
  private def mergePolarity(base: Polarity, layer: Polarity, axis: PolarityAxis)
  :     Polarity raises TelError =
    (base, layer) match
      case (b, Polarity.Implicit)              => b
      case (_, Polarity.Tight)                 => Polarity.Tight
      case (Polarity.Loose,    Polarity.Loose) => Polarity.Loose
      case (_,                 Polarity.Loose) => axis match
        case PolarityAxis.Required   => abort(TelError(Reason.LayerLoosenRequired))
        case PolarityAxis.Repeatable => abort(TelError(Reason.LayerLoosenRepeatable))

  private enum PolarityAxis:
    case Required, Repeatable

  // MergeStruct per §20.3: walk the layer's Members; for each, look up
  // by keyword in the base; if found, refine in place; otherwise
  // append. Exclude inside a Struct body is E217 (Exclude is select-
  // only).
  private def mergeStruct(base: Struct, layer: Struct): Struct raises TelError =
    val members = scala.collection.mutable.ArrayBuffer.from(base.members.toList)
    val keywordToIndex = scala.collection.mutable.HashMap.from(
      members.zipWithIndex.collect:
        case (f: Field, idx)     => f.keyword -> idx
        case (s: SelectRef, idx) => s.reference -> idx)

    var i = 0
    while i < layer.members.length do
      layer.members(i) match
        case f: Field =>
          keywordToIndex.get(f.keyword) match
            case Some(idx) =>
              members(idx) match
                case existing: Field =>
                  members(idx) = Field
                   ( required   = mergePolarity(existing.required, f.required,
                                    PolarityAxis.Required),
                     repeatable = mergePolarity(existing.repeatable, f.repeatable,
                                    PolarityAxis.Repeatable),
                     keyword    = f.keyword,
                     fieldType  = existing.fieldType,  // base's type wins
                     default    = existing.default )

                case _ => abort(TelError(Reason.LayerFieldTypeMismatch))

            case None =>
              keywordToIndex(f.keyword) = members.length
              members += f

        case s: SelectRef =>
          keywordToIndex.get(s.reference) match
            case Some(idx) =>
              members(idx) match
                case existing: SelectRef if existing.reference == s.reference =>
                  members(idx) = SelectRef
                   ( required   = mergePolarity(existing.required, s.required,
                                    PolarityAxis.Required),
                     repeatable = mergePolarity(existing.repeatable, s.repeatable,
                                    PolarityAxis.Repeatable),
                     reference  = s.reference )

                case _ => abort(TelError(Reason.LayerKeywordCollision))

            case None =>
              keywordToIndex(s.reference) = members.length
              members += s

        case _: Exclude => abort(TelError(Reason.ExcludeOutsideSelect))

      i += 1

    val mergedValidators = (base.validators ++ layer.validators).distinct
    Struct(IArray.from(members), IArray.from(mergedValidators))

  private def mergeRecordList
       (base:     IArray[RecordDefinition],
        layer:    IArray[RecordDefinition],
        scalars:  IArray[ScalarDefinition],
        selects:  IArray[SelectDefinition])
  :     IArray[RecordDefinition] raises TelError =
    val out = scala.collection.mutable.ArrayBuffer.from(base.toList)
    var i = 0
    while i < layer.length do
      val newDef = layer(i)
      val existing = out.indexWhere(_.name == newDef.name)
      if existing >= 0 then
        out(existing) = mergeRecord(out(existing), newDef)
      else
        if scalars.exists(_.name == newDef.name) || selects.exists(_.name == newDef.name)
        then abort(TelError(Reason.DuplicateDefinition))
        out += newDef

      i += 1

    IArray.from(out)

  private def mergeRecord(base: RecordDefinition, layer: RecordDefinition)
  :     RecordDefinition raises TelError =
    val baseStruct  = Struct(base.members, base.validators)
    val layerStruct = Struct(layer.members, layer.validators)
    val merged      = mergeStruct(baseStruct, layerStruct)
    RecordDefinition(base.name, merged.members, merged.validators)

  private def mergeScalarList
       (base:    IArray[ScalarDefinition],
        layer:   IArray[ScalarDefinition],
        records: IArray[RecordDefinition],
        selects: IArray[SelectDefinition])
  :     IArray[ScalarDefinition] raises TelError =
    val out = scala.collection.mutable.ArrayBuffer.from(base.toList)
    var i = 0
    while i < layer.length do
      val newDef = layer(i)
      val existing = out.indexWhere(_.name == newDef.name)
      if existing >= 0 then
        val mergedValidators = (out(existing).validators ++ newDef.validators).distinct
        out(existing) = ScalarDefinition(newDef.name, IArray.from(mergedValidators))
      else
        if records.exists(_.name == newDef.name) || selects.exists(_.name == newDef.name)
        then abort(TelError(Reason.DuplicateDefinition))
        out += newDef

      i += 1

    IArray.from(out)

  private def mergeSelectList
       (base:    IArray[SelectDefinition],
        layer:   IArray[SelectDefinition],
        records: IArray[RecordDefinition],
        scalars: IArray[ScalarDefinition])
  :     IArray[SelectDefinition] raises TelError =
    val out = scala.collection.mutable.ArrayBuffer.from(base.toList)
    var i = 0
    while i < layer.length do
      val newDef = layer(i)
      val existing = out.indexWhere(_.name == newDef.name)
      if existing >= 0 then
        out(existing) = mergeSelect(out(existing), newDef)
      else
        if records.exists(_.name == newDef.name) || scalars.exists(_.name == newDef.name)
        then abort(TelError(Reason.DuplicateDefinition))
        out += newDef

      i += 1

    IArray.from(out)

  // SelectDefinition merge: variant declarations in a layer must match
  // existing variant keywords (variant ADDITIONS are forbidden — E214).
  // Exclude(K) removes the variant with keyword K (E212 if missing).
  // Validators are appended.
  private def mergeSelect(base: SelectDefinition, layer: SelectDefinition)
  :     SelectDefinition raises TelError =
    val variants = scala.collection.mutable.ArrayBuffer.from(base.variants.toList)
    var i = 0
    // Note: layer's variant body lives in members; for now we accept
    // that the layer's SelectDefinition is constructed with the same
    // `variants` field shape (excludes encoded as variants with empty
    // type are not part of the data model — phase-3 leaves Exclude
    // handling to a follow-up that integrates the Member-level encoding
    // for layer-only operations).
    while i < layer.variants.length do
      val v = layer.variants(i)
      val existingIdx = variants.indexWhere(_.keyword == v.keyword)
      if existingIdx < 0 then abort(TelError(Reason.LayerVariantAddition))
      i += 1

    val mergedValidators = (base.validators ++ layer.validators).distinct
    SelectDefinition(base.name, IArray.from(variants), IArray.from(mergedValidators))
