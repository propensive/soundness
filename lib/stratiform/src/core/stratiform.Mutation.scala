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
import denominative.*
import gossamer.*
import vacuous.*

import MutationError.Reason

// Primitive presentation-preserving mutations per §22.2. Each op is a
// local rewrite addressed by a Tel.Pointer; surrounding atoms, comments,
// blank lines, and unrelated children are untouched by construction.
//
// Pointer conventions:
//   - Op.UpdateAtom, Delete, Replace, AttachRemark, RemoveRemark,
//     SetFlag, UnsetFlag, InsertBefore, InsertAfter — pointer addresses
//     the *target* compound.
//   - Op.Insert — pointer addresses the *parent* compound (the
//     compound whose children list receives the new entry). An empty
//     pointer means "insert at the document root".

object Mutation:
  enum Op:
    case UpdateAtom(pointer: Tel.Pointer, atomIndex: Int, text: Text)
    case Insert(pointer: Tel.Pointer, compound: Tel.Compound)
    case InsertBefore(pointer: Tel.Pointer, compound: Tel.Compound)
    case InsertAfter(pointer: Tel.Pointer, compound: Tel.Compound)
    case Delete(pointer: Tel.Pointer)
    case Replace(pointer: Tel.Pointer, compound: Tel.Compound)
    case AttachRemark(pointer: Tel.Pointer, text: Text)
    case RemoveRemark(pointer: Tel.Pointer)
    case SetFlag(pointer: Tel.Pointer, keyword: Text)
    case UnsetFlag(pointer: Tel.Pointer, keyword: Text)

    // §22.2 `reorder-within-group` — within the parent at
    // `parentPointer`, move the compound at occurrence `oldIndex` of
    // `keyword` to occurrence `newIndex` (zero-based within the
    // group). The reorder preserves block boundaries and surrounding
    // comments.
    case ReorderWithinGroup
      ( parentPointer: Tel.Pointer, keyword: Text, oldIndex: Int, newIndex: Int )

    // §22.2 `reorder-groups` — within the parent at `parentPointer`,
    // swap the relative order of all compounds with `firstKeyword`
    // and all compounds with `secondKeyword`. The two groups must
    // each be contiguous before and after (E309).
    case ReorderGroups(parentPointer: Tel.Pointer, firstKeyword: Text, secondKeyword: Text)

    // §22.2 `resize-tabulation` — recompute `markerOffsets` of the
    // tabulation in block `blockIndex` of the parent at `parentPointer`
    // using the §22.2 minimal-offsets algorithm, then re-pad every row.
    case ResizeTabulation(parentPointer: Tel.Pointer, blockIndex: Int)

  private def pointerOf(op: Op): Tel.Pointer = op match
    case Op.UpdateAtom(p, _, _)            => p
    case Op.Insert(p, _)                   => p
    case Op.InsertBefore(p, _)             => p
    case Op.InsertAfter(p, _)              => p
    case Op.Delete(p)                      => p
    case Op.Replace(p, _)                  => p
    case Op.AttachRemark(p, _)             => p
    case Op.RemoveRemark(p)                => p
    case Op.SetFlag(p, _)                  => p
    case Op.UnsetFlag(p, _)                => p
    case Op.ReorderWithinGroup(p, _, _, _) => p
    case Op.ReorderGroups(p, _, _)         => p
    case Op.ResizeTabulation(p, _)         => p

  def apply(tel: Tel, op: Op): Tel raises MutationError =
    // The document's resolved sigil (§8.3) feeds the inline-safe predicate
    // used by `update-value` form escalation; defaults to `#`.
    val sigil = tel.subtree match
      case d: Tel.Document => d.pragma.let(_.sigil.or('#')).or('#')
      case _               => '#'

    Tel.make(transform(tel.subtree, pointerOf(op).steps, 0, op, sigil))

  def apply(tel: Tel, ops: Seq[Op]): Tel raises MutationError =
    var current = tel
    var i = 0

    while i < ops.length do
      current = apply(current, ops(i))
      i += 1

    current

  // Descend through the pointer's steps. When `idx` equals `steps.length`
  // we have arrived at the pointer's destination — for `Insert` this is
  // the container into which the new compound is appended; for any other
  // op an empty pointer is invalid because the document root has no
  // keyword. When `idx == steps.length - 1` we are at the *parent* of a
  // sibling-targeted op (UpdateAtom, Delete, Replace, …) and apply the
  // op against the named child.
  private def transform
    ( subtree:  Tel.Subtree,
      steps:    IArray[Tel.Pointer.Step],
      idx:      Int,
      op:       Op,
      sigil:    Char )
  :   Tel.Subtree raises MutationError =

    if idx >= steps.length then op match
      case Op.Insert(_, compound) =>
        rewrap(subtree, appendCompound(subtree.children, compound))

      case Op.ReorderWithinGroup(_, keyword, oldI, newI) =>
        rewrap(subtree, reorderWithinGroup(subtree.children, keyword, oldI, newI))

      case Op.ReorderGroups(_, firstKw, secondKw) =>
        rewrap(subtree, reorderGroups(subtree.children, firstKw, secondKw))

      case Op.ResizeTabulation(_, blockIndex) =>
        rewrap(subtree, resizeTabulation(subtree.children, blockIndex))

      case _ => abort(MutationError(Reason.PointerNotFound))

    else
      val step = steps(idx)
      val (blockIdx, localIdx) = findTarget(subtree.children, step)

      val isTargetOp = op match
        case _: Op.Insert             => false
        case _: Op.ReorderWithinGroup => false
        case _: Op.ReorderGroups      => false
        case _: Op.ResizeTabulation   => false
        case _                        => true

      if isTargetOp && idx == steps.length - 1 then
        val updatedBlock = applyToTarget(subtree.children(blockIdx), localIdx, op, sigil)
        rewrap(subtree, subtree.children.updated(blockIdx, updatedBlock))
      else
        val targetBlock = subtree.children(blockIdx)
        val targetCompound = targetBlock.compounds(localIdx)
        val updatedSubtree = transform(targetCompound, steps, idx + 1, op, sigil)

        val updatedCompound = updatedSubtree match
          case c: Tel.Compound => c
          case _: Tel.Document => targetCompound // unreachable: child of a compound is a compound

        val updatedBlock =
          targetBlock.copy(compounds = targetBlock.compounds.updated(localIdx, updatedCompound))

        rewrap(subtree, subtree.children.updated(blockIdx, updatedBlock))

  private def rewrap(subtree: Tel.Subtree, children: IArray[Tel.Block]): Tel.Subtree =
    subtree match
      case d: Tel.Document => d.copy(children = children)
      case c: Tel.Compound => c.copy(children = children)

  // Locate the `(blockIndex, compoundIndex)` of the n-th compound matching
  // `step.keyword`. `step.index` defaults to 0 — i.e. "the first match".
  // Counting walks all blocks in order so siblings with the same keyword
  // spread across multiple blocks remain addressable.
  private def findTarget(blocks: IArray[Tel.Block], step: Tel.Pointer.Step)
  :   (Int, Int) raises MutationError =

    val want = step.index.or(0)
    var seen = 0
    var b = 0
    var foundBlock = -1
    var foundLocal = -1

    while b < blocks.length && foundBlock < 0 do
      val cs = blocks(b).compounds
      var c = 0

      while c < cs.length && foundBlock < 0 do
        if cs(c).keyword == step.keyword then
          if seen == want then
            foundBlock = b
            foundLocal = c
          else
            seen += 1

        c += 1

      b += 1

    if foundBlock < 0 then abort(MutationError(Reason.PointerNotFound))
    (foundBlock, foundLocal)

  // Apply a target-addressed op to the compound at `localIdx` within
  // `block`, returning the rewritten block. Ops may produce zero, one, or
  // two compounds in place of the target (e.g. Delete -> 0, InsertBefore
  // -> 2).
  private def applyToTarget(block: Tel.Block, localIdx: Int, op: Op, sigil: Char)
  :   Tel.Block raises MutationError =

    val target = block.compounds(localIdx)

    val replacement: IArray[Tel.Compound] = op match
      case Op.UpdateAtom(_, atomIndex, text) =>
        IArray(updateAtomAt(target, atomIndex, text, sigil))

      case Op.Delete(_) =>
        IArray.empty

      case Op.Replace(_, compound) =>
        IArray(compound)

      case Op.AttachRemark(_, text) =>
        IArray(target.copy(remark = text))

      case Op.RemoveRemark(_) =>
        if target.remark.absent then abort(MutationError(Reason.RemarkAbsent))
        IArray(target.copy(remark = Unset))

      case Op.InsertBefore(_, compound) =>
        IArray(compound, target)

      case Op.InsertAfter(_, compound) =>
        IArray(target, compound)

      case Op.SetFlag(_, keyword) =>
        val present = target.children.flatMap(_.compounds).exists(_.keyword == keyword)
        if present then abort(MutationError(Reason.FlagAlreadySet))
        val flag = Tel.Compound(keyword, IArray.empty, Unset, IArray.empty)
        IArray(target.copy(children = appendCompound(target.children, flag)))

      case Op.UnsetFlag(_, keyword) =>
        val present = target.children.flatMap(_.compounds).exists(_.keyword == keyword)
        if !present then abort(MutationError(Reason.FlagNotSet))

        val updated = target.children.map: b =>
          b.copy(compounds = b.compounds.filterNot(_.keyword == keyword))

        IArray(target.copy(children = updated))

      case Op.Insert(_, _) | Op.ReorderWithinGroup(_, _, _, _) | Op.ReorderGroups(_, _, _)
          | Op.ResizeTabulation(_, _) =>
        // unreachable: handled in transform's container-mode arm
        abort(MutationError(Reason.PointerNotFound))

    val before = block.compounds.take(localIdx)
    val after = block.compounds.drop(localIdx + 1)
    block.copy(compounds = before ++ replacement ++ after)

  // §22.3 `update-value` — replace the atomIndex-th atom's text (counting
  // every atom form). The atom's *form* is subject to the §22.2 Atom-form
  // safety invariant: the current form is kept while the new value stays
  // safe for it, otherwise the form escalates along inline -> source ->
  // literal to the first later form whose predicate the value satisfies,
  // never to an earlier one (so updating a literal atom to an inline-safe
  // value leaves it a literal atom). Preceding spaces follow the §22.3
  // hard-space rule; a kept literal atom reuses its delimiter when safe.
  private def updateAtomAt(compound: Tel.Compound, atomIndex: Int, text: Text, sigil: Char)
  :   Tel.Compound raises MutationError =

    if atomIndex < 0 || atomIndex >= compound.atoms.length
    then abort(MutationError(Reason.AtomIndexOutOfRange))

    val updated = escalateAtom(compound.atoms(atomIndex), text, sigil)
    compound.copy(atoms = compound.atoms.updated(atomIndex, updated))

  // The §22.3 form-escalation step: keep the current form if the new value
  // is still safe for it, else advance (never retreat) to the first later
  // form whose §22.2 predicate holds.
  private def escalateAtom(current: Tel.Atom, value: Text, sigil: Char): Tel.Atom =
    current match
      case Tel.Atom.Inline(_, _) =>
        if inlineSafe(value, sigil) then Tel.Atom.Inline(value, inlinePrecedingSpaces(value))
        else if sourceSafe(value) then Tel.Atom.Source(value)
        else Tel.Atom.Literal(literalDelimiter(value, t"---"), value)

      case Tel.Atom.Source(_) =>
        if sourceSafe(value) then Tel.Atom.Source(value)
        else Tel.Atom.Literal(literalDelimiter(value, t"---"), value)

      case Tel.Atom.Literal(delimiter, _) =>
        Tel.Atom.Literal(literalDelimiter(value, delimiter), value)

  // Append `compound` to the last block of `blocks`. If `blocks` is empty
  // a fresh block is created. Trailing blank lines on the existing last
  // block are preserved.
  private def appendCompound(blocks: IArray[Tel.Block], compound: Tel.Compound)
  :   IArray[Tel.Block] =

    if blocks.length == 0
    then IArray(Tel.Block(IArray.empty, Unset, IArray(compound), 0))
    else
      val lastIdx = blocks.length - 1
      val last = blocks(lastIdx)
      blocks.updated(lastIdx, last.copy(compounds = last.compounds :+ compound))

  // §22.2 `reorder-within-group`. Locate every occurrence of `keyword`
  // across `blocks`, build the member group as a flat sequence of
  // `(blockIndex, compoundIndex)` references, then move the element
  // at occurrence `oldIndex` to occurrence `newIndex` and rewrite the
  // affected blocks. Compounds with other keywords stay in place.
  private def reorderWithinGroup
    ( blocks: IArray[Tel.Block], keyword: Text, oldIndex: Int, newIndex: Int )
  :   IArray[Tel.Block] raises MutationError =

    val positions = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]
    var b = 0

    while b < blocks.length do
      val cs = blocks(b).compounds
      var c = 0

      while c < cs.length do
        if cs(c).keyword == keyword then positions += ((b, c))
        c += 1

      b += 1

    if oldIndex < 0 || oldIndex >= positions.length ||
      newIndex < 0 || newIndex >= positions.length
    then abort(MutationError(Reason.PointerNotFound))

    if oldIndex == newIndex then blocks
    else
      // Extract the moved compound, then write the group back in the
      // new order into the same (blockIndex, compoundIndex) slots.
      val movedCompound =
        val (bIdx, cIdx) = positions(oldIndex)
        blocks(bIdx).compounds(cIdx)

      val newGroup = scala.collection.mutable.ArrayBuffer.empty[Tel.Compound]
      var i = 0

      while i < positions.length do
        val (bIdx, cIdx) = positions(i)
        if i != oldIndex then newGroup += blocks(bIdx).compounds(cIdx)
        i += 1

      newGroup.insert(newIndex, movedCompound)

      // Write the reordered group back into the original slots.
      val out = scala.collection.mutable.ArrayBuffer.from(blocks.toList)
      var j = 0

      while j < positions.length do
        val (bIdx, cIdx) = positions(j)
        val block = out(bIdx)
        out(bIdx) = block.copy(compounds = block.compounds.updated(cIdx, newGroup(j)))
        j += 1

      IArray.from(out)

  // §22.2 `reorder-groups`. Verify both keyword groups are contiguous
  // in `blocks` (else E309 violation), then swap their relative
  // order. Each group's compounds and surrounding block boundaries
  // are preserved.
  private def reorderGroups
    ( blocks: IArray[Tel.Block], firstKeyword: Text, secondKeyword: Text )
  :   IArray[Tel.Block] raises MutationError =

    // A "group" here is the contiguous run of compounds with a given
    // keyword. We rebuild the children block list with the two groups
    // swapped in member position.
    val compoundsByBlock = blocks.map(_.compounds)

    // Walk all compounds in flat order, recording for each compound
    // its absolute index and whether it belongs to a tracked group.
    val flat = scala.collection.mutable.ArrayBuffer.empty[(Int, Int, Tel.Compound)]
    var b = 0

    while b < blocks.length do
      val cs = compoundsByBlock(b)
      var c = 0

      while c < cs.length do
        flat += ((b, c, cs(c)))
        c += 1

      b += 1

    // Verify contiguity for each group (or absent).
    def runRange(kw: Text): Optional[(Int, Int)] =
      val indices = flat.zipWithIndex.collect:
        case ((_, _, cmp), idx) if cmp.keyword == kw => idx

      if indices.nil then Unset
      else if indices.last - indices.head + 1 != indices.length
      then abort(MutationError(Reason.PointerNotFound))
      else (indices.head, indices.last): Optional[(Int, Int)]

    val firstRange = runRange(firstKeyword)
    val secondRange = runRange(secondKeyword)

    (firstRange, secondRange) match
      case (f: (Int, Int) @unchecked, s: (Int, Int) @unchecked) =>
        // Swap positions: the earlier-positioned group moves to where
        // the later one was, preserving counts.
        val (a, b) = if f._1 < s._1 then (f, s) else (s, f)
        val before = flat.toList.take(a._1)
        val groupA = flat.toList.slice(a._1, a._2 + 1)
        val between = flat.toList.slice(a._2 + 1, b._1)
        val groupB = flat.toList.slice(b._1, b._2 + 1)
        val after  = flat.toList.drop(b._2 + 1)

        val reordered = (before ++ groupB ++ between ++ groupA ++ after).map(_._3)

        // Rewrite each block, preserving the original block sizes.
        val out = scala.collection.mutable.ArrayBuffer.empty[Tel.Block]
        var p = 0
        var bi = 0

        while bi < blocks.length do
          val sz = compoundsByBlock(bi).length
          val cs = IArray.from(reordered.slice(p, p + sz))
          out += blocks(bi).copy(compounds = cs)
          p += sz
          bi += 1

        IArray.from(out)

      case _ =>
        // Either group not present — nothing to reorder. Spec says
        // both groups should exist for this op to be meaningful; we
        // treat a missing group as PointerNotFound.
        abort(MutationError(Reason.PointerNotFound))

  // §22.2 `resize-tabulation`. Recompute the `markerOffsets` of the
  // tabulation in `blocks(blockIndex)` using the minimal-offsets
  // algorithm: column i's marker is positioned to leave the previous
  // column its full width plus exactly two spaces of inter-column gap.
  // All existing row content is re-padded with spaces so atom
  // positions align with the new column starts.
  private def resizeTabulation(blocks: IArray[Tel.Block], blockIndex: Int)
  :   IArray[Tel.Block] raises MutationError =

    if blockIndex < 0 || blockIndex >= blocks.length
    then abort(MutationError(Reason.PointerNotFound))

    val block = blocks(blockIndex)

    block.tabulation.let: tab =>
      // Compute widths column-by-column: per spec, w_0 is the keyword
      // column's max width (keywords of all rows, plus heading 0); w_i
      // (i ≥ 1) is the i-th atom's max width across all rows, plus
      // heading_i.
      val n = tab.markerOffsets.length

      def textWidth(text: Text): Int = text.s.length

      val widths = new Array[Int](n)
      var col = 0

      while col < n do
        widths(col) = textWidth(tab.headings(col))
        col += 1

      // Keyword column = column 0; subsequent atoms map to columns 1..n-1.
      block.compounds.foreach: c =>
        val kwWidth = textWidth(c.keyword)
        if kwWidth > widths(0) then widths(0) = kwWidth
        var ai = 0
        var colIdx = 1

        while ai < c.atoms.length && colIdx < n do
          c.atoms(ai) match
            case Tel.Atom.Inline(text, _) =>
              val w = textWidth(text)
              if w > widths(colIdx) then widths(colIdx) = w
              colIdx += 1

            case _ => ()

          ai += 1

      // Minimal-offsets algorithm:
      //   markerOffsets[0] = w_0 + 2
      //   markerOffsets[i] = markerOffsets[i-1] + 1 + w_i + 2
      val newOffsets = new Array[Int](n)
      newOffsets(0) = widths(0) + 2
      var i = 1

      while i < n do
        newOffsets(i) = newOffsets(i - 1) + 1 + widths(i) + 2
        i += 1

      // Re-pad each row's atoms so the i-th inline atom starts at
      // markerOffsets[i]. The keyword stays at column 0; precedingSpaces
      // on each inline atom is set to the gap between the previous
      // atom's end and the new column start.
      val newCompounds = block.compounds.map: c =>
        var cursor = textWidth(c.keyword)
        var colIdx = 1

        val newAtoms = c.atoms.map: a =>
          a match
            case Tel.Atom.Inline(text, _) if colIdx < n =>
              val targetStart = newOffsets(colIdx)
              val gap = math.max(2, targetStart - cursor)
              val padded = Tel.Atom.Inline(text, gap)
              cursor = targetStart + textWidth(text)
              colIdx += 1
              padded

            case other => other

        c.copy(atoms = newAtoms)

      blocks.updated(blockIndex,
        block.copy
         ( tabulation = Tel.Tabulation(newOffsets.asInstanceOf[IArray[Int]], tab.headings),
           compounds  = newCompounds ))

    .or(blocks)

  // §22.3 `construct` — produce a fresh compound from a keyword and a
  // sequence of scalar atom texts, choosing each atom's form by the §22.2
  // atom-form safety predicates: inline if inline-safe, else source if
  // source-safe, else literal. An empty value yields *no atom* (a leading
  // space would be an E108 trailing space on the keyword line, §22.3), so
  // empty values are dropped. The canonical sigil is `#`.
  def construct(keyword: Text, atoms: Text*): Tel.Compound =
    val atomNodes =
      IArray.from(atoms.collect { case value if value.s.nonEmpty => chooseAtomForm(value, '#') })

    Tel.Compound(keyword, atomNodes, Unset, IArray.empty)

  // §22.3 atom-form escalation: the first form in inline -> source ->
  // literal whose §22.2 safety predicate the value satisfies.
  private def chooseAtomForm(value: Text, sigil: Char): Tel.Atom =
    if inlineSafe(value, sigil) then Tel.Atom.Inline(value, inlinePrecedingSpaces(value))
    else if sourceSafe(value) then Tel.Atom.Source(value)
    else Tel.Atom.Literal(literalDelimiter(value, t"---"), value)

  // §22.3: an inline atom whose value contains a space uses a two-space
  // (hard-space) separator so the parser keeps the soft spaces as content
  // (§10.3); a space-free value uses a single space.
  private def inlinePrecedingSpaces(value: Text): Int =
    if value.s.indexOf(' ') >= 0 then 2 else 1

  // §22.2 inline-safe: no LF; no leading/trailing space; no run of two or
  // more spaces; and the value does not begin with the sigil immediately
  // followed by a space (which would start a remark, §11.2). An internal
  // space-then-sigil is safe — a spaced value is emitted in hard-space
  // mode, so the sigil is not at a phrase boundary. An empty value is
  // inline-safe (callers emit it as no atom, not an empty inline atom).
  private def inlineSafe(value: Text, sigil: Char): Boolean =
    val s = value.s

    if s.isEmpty then true
    else if s.charAt(0) == ' ' || s.charAt(s.length - 1) == ' ' then false
    else if s.length >= 2 && s.charAt(0) == sigil && s.charAt(1) == ' ' then false
    else
      var i = 0
      var ok = true

      while ok && i < s.length do
        val c = s.charAt(i)

        if c == '\n' then ok = false
        else if c == ' ' && i + 1 < s.length && s.charAt(i + 1) == ' ' then ok = false

        i += 1

      ok

  // §22.2 source-safe: non-empty; no empty line (hence no leading/trailing
  // LF and no run of two or more LFs); no line ending in a space (source
  // atoms strip trailing spaces, §14); and the first line does not begin
  // with a space (the first line's indentation is stripped, §14).
  private def sourceSafe(value: Text): Boolean =
    val s = value.s

    if s.isEmpty then false
    else if s.charAt(0) == '\n' || s.charAt(s.length - 1) == '\n' then false
    else if s.charAt(0) == ' ' || s.charAt(s.length - 1) == ' ' then false
    else
      var i = 0
      var ok = true

      while ok && i < s.length do
        if s.charAt(i) == '\n' then
          if s.charAt(i - 1) == ' ' then ok = false        // trailing space on a line
          else if s.charAt(i + 1) == '\n' then ok = false  // empty interior line

        i += 1

      ok

  // §22.3 literal delimiter: the shortest run of `-`, starting from
  // `initial`, that does not appear as a line of the value — so the value
  // is literal-safe (§22.2) with respect to it.
  private def literalDelimiter(value: Text, initial: Text): Text =
    var delimiter = initial.s
    while appearsAsLine(value.s, delimiter) do delimiter = delimiter+"-"
    Text(delimiter)

  // True if `line` occurs as a whole LF-delimited line of `s`.
  private def appearsAsLine(s: String, line: String): Boolean =
    var start = 0
    var found = false

    while !found && start <= s.length do
      val nl = s.indexOf('\n', start)
      val end = if nl < 0 then s.length else nl
      if s.substring(start, end).nn == line then found = true
      start = if nl < 0 then s.length + 1 else nl + 1

    found
