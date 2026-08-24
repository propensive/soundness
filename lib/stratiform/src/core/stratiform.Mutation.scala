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

import murmuration.*

import anticipation.*
import contingency.*
import gossamer.*
import vacuous.*

import Mutation.Error.Reason
import fulminate.*
import rudiments.`:+`
import denominative.dysasymptotics.linearSize

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
//   - Op.InsertIntoBlock, ReorderWithinGroup, ReorderGroups,
//     ResizeTabulation — pointer addresses the *parent* compound; the
//     operation's own indices select among that parent's blocks or
//     member groups.
//
// Addressing. The spec's compound paths are `(block_index,
// compound_index)` pairs; a `Tel.Pointer` step is a keyword plus an
// occurrence index, which identifies the same compound (every compound
// has a keyword, and flat occurrence order is bijective with block and
// compound indices) while staying meaningful across unrelated edits. A
// spec block path is expressed as a parent pointer plus `blockIndex`,
// and an atom path as a target pointer plus `atomIndex` in presentation
// order (atoms precede compound children in semantic order, §18.3).
// Pointers are resolved against the document an op is applied to:
// callers must not reuse occurrence-indexed pointers across mutations
// that move, add or remove compounds of the same keyword.
//
// Schema-dependent preconditions are out of scope here, exactly as in
// the reference implementation: the `required` checks of `delete` and
// `unset-flag`, the `repeatable` check of `set-flag` beyond simple
// presence, `update-value`'s §21 helper-method validation, `replace`'s
// member-equivalence and well-typedness, and schema-level E309 for
// `reorder-groups`. A schema-aware layer can perform them before
// delegating to these primitives.

object Mutation:
  // §22.2 `reorder-groups` is a directed move: the addressed group's
  // blocks are placed immediately before or immediately after the other
  // group's blocks.
  enum Placement:
    case Before, After

  // A schema-free description of a struct member for the §22.3 canonical
  // `construct` algorithm: a scalar member with its present occurrences
  // (one occurrence models a non-repeatable member, several a repeatable
  // one, none an absent member that terminates the inline run), a flag
  // member, a ready-made compound child, or `Break` — an absent member
  // that contributes nothing but terminates the inline run (§22.2: an
  // absent Scalar member ends the run, because the atom phase never skips
  // a Scalar position).
  enum Member:
    case Value(keyword: Text, occurrences: List[Text])
    case Flag(keyword: Text)
    case Child(compound: Tel.Compound)
    case Break

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

    // §22.2 `insert-into-block` — append a compound to the `compounds`
    // list of block `blockIndex` of the parent at `parentPointer`: the
    // natural way to add a row to a tabulated block. Each inline atom of
    // the new compound is one column value; the tabulation must have
    // sufficient column capacity (apply `resize-tabulation` first if
    // not), and the row is re-padded to the block's marker offsets.
    case InsertIntoBlock(parentPointer: Tel.Pointer, blockIndex: Int, compound: Tel.Compound)

    // §22.2 `reorder-within-group` — within the parent at
    // `parentPointer`, move the compound at occurrence `oldIndex` of
    // `keyword` to occurrence `newIndex` (zero-based within the
    // group). The reorder preserves block boundaries and surrounding
    // comments.
    case ReorderWithinGroup
      ( parentPointer: Tel.Pointer, keyword: Text, oldIndex: Int, newIndex: Int )

    // §22.2 `reorder-groups` — within the parent at `parentPointer`,
    // move the member group of `keyword` to immediately before or after
    // the member group of `otherKeyword`. Whole blocks move where the
    // groups occupy disjoint blocks, so attached comments, tabulations
    // and blank lines travel with their group. Both groups must be
    // contiguous before and after (E309). The spec addresses the groups
    // by member index; keywords are the schema-free equivalent, as in
    // the reference implementation.
    case ReorderGroups
      ( parentPointer: Tel.Pointer,
        keyword:       Text,
        otherKeyword:  Text,
        placement:     Placement = Placement.Before )

    // §22.2 `resize-tabulation` — recompute `markerOffsets` of the
    // tabulation in block `blockIndex` of the parent at `parentPointer`
    // using the normative minimal-offsets algorithm, then re-pad every
    // row. `plannedRows` are rows about to be added (via
    // `insert-into-block`), whose column widths the new offsets must
    // also accommodate.
    case ResizeTabulation
      ( parentPointer: Tel.Pointer,
        blockIndex:    Int,
        plannedRows:   Array[Tel.Compound]^{} = Array.empty[Tel.Compound] )

  private def pointerOf(op: Op): Tel.Pointer = op match
    case Op.UpdateAtom(p, _, _)            => p
    case Op.Insert(p, _)                   => p
    case Op.InsertBefore(p, _)             => p
    case Op.InsertAfter(p, _)              => p
    case Op.InsertIntoBlock(p, _, _)       => p
    case Op.Delete(p)                      => p
    case Op.Replace(p, _)                  => p
    case Op.AttachRemark(p, _)             => p
    case Op.RemoveRemark(p)                => p
    case Op.SetFlag(p, _)                  => p
    case Op.UnsetFlag(p, _)                => p
    case Op.ReorderWithinGroup(p, _, _, _) => p
    case Op.ReorderGroups(p, _, _, _)      => p
    case Op.ResizeTabulation(p, _, _)      => p

  def apply(tel: Tel, op: Op): Tel raises Mutation.Error =
    // The document's resolved sigil (§8.3) feeds the inline-safe predicate
    // used by `update-value` form escalation; defaults to `#`.
    val sigil = tel.subtree match
      case d: Tel.Document => d.pragma.let(_.sigil.or('#')).or('#')
      case _               => '#'

    Tel.make(transform(tel.subtree, pointerOf(op).steps, 0, op, sigil))

  def apply(tel: Tel, ops: List[Op]): Tel raises Mutation.Error =
    var current = tel
    val ops2 = ops.stdlib
    var i = 0

    while i < ops2.length do
      current = apply(current, ops2(i))
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
      steps:    Array[Tel.Pointer.Step]^{},
      idx:      Int,
      op:       Op,
      sigil:    Char )
  :   Tel.Subtree raises Mutation.Error =

    if idx >= steps.length then op match
      case Op.Insert(_, compound) =>
        rewrap(subtree, insertNatural(subtree.children, compound))

      case Op.InsertIntoBlock(_, blockIndex, compound) =>
        rewrap(subtree, insertIntoBlock(subtree.children, blockIndex, compound, idx))

      case Op.ReorderWithinGroup(_, keyword, oldI, newI) =>
        rewrap(subtree, reorderWithinGroup(subtree.children, keyword, oldI, newI))

      case Op.ReorderGroups(_, keyword, otherKeyword, placement) =>
        rewrap(subtree, reorderGroups(subtree.children, keyword, otherKeyword, placement))

      case Op.ResizeTabulation(_, blockIndex, plannedRows) =>
        rewrap(subtree, resizeTabulation(subtree.children, blockIndex, plannedRows, idx))

      case _ => abort(Mutation.Error(Reason.PointerNotFound))

    else
      val step = steps.readable(idx)
      val (blockIdx, localIdx) = findTarget(subtree.children, step)

      val isTargetOp = op match
        case _: Op.Insert             => false
        case _: Op.InsertIntoBlock    => false
        case _: Op.ReorderWithinGroup => false
        case _: Op.ReorderGroups      => false
        case _: Op.ResizeTabulation   => false
        case _                        => true

      if isTargetOp && idx == steps.length - 1 then
        val replacement = applyToTarget(subtree.children.readable(blockIdx), localIdx, op, sigil)
        val children = subtree.children

        val spliced =
          if replacement.length > 0
          then Array.frozen
                ( children.readable.take(blockIdx) ++ replacement.readable
                  ++ children.readable.drop(blockIdx + 1) )
          else
            // The op emptied the block (§22.2 `delete`): the block and its
            // comments go, but its trailing blank lines — which include a
            // nested subtree's final line ending — are absorbed by the
            // preceding block, or kept as bare blank lines when the block
            // was a compound's only child block.
            removeBlock(children, blockIdx, subtree.isInstanceOf[Tel.Compound])

        rewrap(subtree, spliced)
      else
        val targetBlock = subtree.children.readable(blockIdx)
        val targetCompound = targetBlock.compounds.readable(localIdx)
        val updatedSubtree = transform(targetCompound, steps, idx + 1, op, sigil)

        val updatedCompound = updatedSubtree match
          case c: Tel.Compound => c
          case _: Tel.Document => targetCompound // unreachable: child of a compound is a compound

        val updatedBlock =
          targetBlock.copy
           (compounds = Array.frozen(targetBlock.compounds.readable.updated(localIdx, updatedCompound)))

        rewrap(subtree, Array.frozen(subtree.children.readable.updated(blockIdx, updatedBlock)))

  private def rewrap(subtree: Tel.Subtree, children: Array[Tel.Block]^{}): Tel.Subtree =
    subtree match
      case d: Tel.Document => d.copy(children = children)
      case c: Tel.Compound => c.copy(children = children)

  // Locate the `(blockIndex, compoundIndex)` of the n-th compound matching
  // `step.keyword`. `step.index` defaults to 0 — i.e. "the first match".
  // Counting walks all blocks in order so siblings with the same keyword
  // spread across multiple blocks remain addressable.
  private def findTarget(blocks: Array[Tel.Block]^{}, step: Tel.Pointer.Step)
  :   (Int, Int) raises Mutation.Error =

    val want = step.index.or(0)
    var seen = 0
    var b = 0
    var foundBlock = -1
    var foundLocal = -1

    while b < blocks.length && foundBlock < 0 do
      val cs = blocks.readable(b).compounds
      var c = 0

      while c < cs.length && foundBlock < 0 do
        if cs.readable(c).keyword == step.keyword then
          if seen == want then
            foundBlock = b
            foundLocal = c
          else
            seen += 1

        c += 1

      b += 1

    if foundBlock < 0 then abort(Mutation.Error(Reason.PointerNotFound))
    (foundBlock, foundLocal)

  // Apply a target-addressed op to the compound at `localIdx` within
  // `block`, returning the blocks that replace the target's block: none
  // when the op empties it (§22.2 `delete` removes an emptied block with
  // its attached comments), one for an in-place rewrite, or two when a
  // sibling is inserted next to a tabulated block.
  private def applyToTarget(block: Tel.Block, localIdx: Int, op: Op, sigil: Char)
  :   Array[Tel.Block]^{} raises Mutation.Error =

    val target = block.compounds.readable(localIdx)

    def splice(replacement: Array[Tel.Compound]^{}): Array[Tel.Block]^{} =
      val compounds =
        Array.frozen
         ( block.compounds.readable.take(localIdx) ++ replacement.readable
           ++ block.compounds.readable.drop(localIdx + 1) )

      if compounds.length == 0 then Array.empty
      else Array(block.copy(compounds = compounds))

    op match
      case Op.UpdateAtom(_, atomIndex, text) =>
        splice(Array(updateAtomAt(target, atomIndex, text, sigil)))

      case Op.Delete(_) =>
        splice(Array.empty)

      case Op.Replace(_, compound) =>
        // §22.2: the replacement retains the original compound's remark. A
        // replacement carrying its own remark keeps it — `construct` never
        // produces one, so the two readings coincide for constructed
        // payloads.
        splice(Array(compound.copy(remark = compound.remark.or(target.remark))))

      case Op.AttachRemark(_, text) =>
        splice(Array(target.copy(remark = text)))

      case Op.RemoveRemark(_) =>
        // Removing an absent remark produces an identical document, so it
        // succeeds as the identity (§22.2).
        if target.remark.absent then Array(block)
        else splice(Array(target.copy(remark = Unset)))

      case Op.InsertBefore(_, compound) =>
        // §22.2: the same block as the sibling, unless that block is
        // tabulated — then a new block immediately before it, whose blank
        // line keeps the new compound from being read as a row.
        if block.tabulation.present
        then Array(Tel.Block(Array.empty, Unset, Array(compound), 1), block)
        else splice(Array(compound, target))

      case Op.InsertAfter(_, compound) =>
        // As `insert-before`; the tabulated block's blank line stops it
        // absorbing the new compound as a row (§16.2), and the new block
        // takes over the original separation from following content.
        if block.tabulation.present
        then Array
              ( block.copy(trailingBlankLines = 1),
                Tel.Block(Array.empty, Unset, Array(compound), block.trailingBlankLines) )
        else splice(Array(target, compound))

      case Op.SetFlag(_, keyword) =>
        val inlinePresent = target.atoms.exists:
          case Tel.Atom.Inline(text, _) => text == keyword
          case _                        => false

        val childPresent = target.children.flatMap(_.compounds).exists(_.keyword == keyword)
        if inlinePresent || childPresent then abort(Mutation.Error(Reason.FlagAlreadySet))

        // §22.2 placement conditions (a) and (b) reference schema member
        // order; the schema-free sufficient condition is that the target
        // has no compound children (and no source/literal atom, which
        // would end the inline line). The atom is hard-space-preceded
        // when the line is already in hard-space mode (§10.3), else
        // soft-space-preceded.
        val inlinePlaceable =
          target.children.length == 0 && !target.atoms.exists:
            case Tel.Atom.Inline(_, _) => false
            case _                     => true

        if inlinePlaceable then
          val hard = target.atoms.exists:
            case Tel.Atom.Inline(text, spaces) => spaces >= 2 || text.s.indexOf(' ') >= 0
            case _                             => false

          val flagAtom = Tel.Atom.Inline(keyword, if hard then 2 else 1)
          splice(Array(target.copy(atoms = target.atoms :+ flagAtom)))
        else
          val flag = Tel.Compound(keyword, Array.empty, Unset, Array.empty)
          splice(Array(target.copy(children = insertNatural(target.children, flag))))

      case Op.UnsetFlag(_, keyword) =>
        // An inline-atom flag is removed from the atom list, preserving
        // every other atom's preceding spaces (§22.2); a compound-child
        // flag is removed by the `delete` rules, so an emptied child
        // block disappears. Only flag-shaped compounds (no atoms, no
        // children) qualify. An absent flag is the identity.
        var atomIdx = -1
        var i = 0

        while atomIdx < 0 && i < target.atoms.length do
          target.atoms.readable(i) match
            case Tel.Atom.Inline(text, _) => if text == keyword then atomIdx = i
            case _                        => ()

          i += 1

        if atomIdx >= 0 then
          val atoms =
            Array.frozen(target.atoms.readable.take(atomIdx) ++ target.atoms.readable.drop(atomIdx + 1))
          splice(Array(target.copy(atoms = atoms)))
        else
          var foundBlock = -1
          var foundLocal = -1
          var b = 0

          while foundBlock < 0 && b < target.children.length do
            val cs = target.children.readable(b).compounds
            var c = 0

            while foundBlock < 0 && c < cs.length do
              val candidate = cs.readable(c)

              if candidate.keyword == keyword && candidate.atoms.length == 0
                && candidate.children.length == 0
              then
                foundBlock = b
                foundLocal = c

              c += 1

            b += 1

          if foundBlock < 0 then Array(block)
          else
            val childBlock = target.children.readable(foundBlock)

            val remaining =
              Array.frozen
               ( childBlock.compounds.readable.take(foundLocal)
                 ++ childBlock.compounds.readable.drop(foundLocal + 1) )

            val children =
              if remaining.length == 0 then removeBlock(target.children, foundBlock, true)
              else Array.frozen
                    (target.children.readable.updated(foundBlock, childBlock.copy(compounds = remaining)))

            splice(Array(target.copy(children = children)))

      case Op.Insert(_, _) | Op.InsertIntoBlock(_, _, _) | Op.ReorderWithinGroup(_, _, _, _)
        | Op.ReorderGroups(_, _, _, _) | Op.ResizeTabulation(_, _, _) =>
        // unreachable: handled in transform's container-mode arm
        abort(Mutation.Error(Reason.PointerNotFound))

  // §22.3 `update-value` — replace the atomIndex-th atom's text (counting
  // every atom form). The atom's *form* is subject to the §22.2 Atom-form
  // safety invariant: the current form is kept while the new value stays
  // safe for it, otherwise the form escalates along inline -> source ->
  // literal to the first later form whose predicate the value satisfies,
  // never to an earlier one (so updating a literal atom to an inline-safe
  // value leaves it a literal atom). Preceding spaces follow the §22.3
  // hard-space rule; a kept literal atom reuses its delimiter when safe.
  private def updateAtomAt(compound: Tel.Compound, atomIndex: Int, text: Text, sigil: Char)
  :   Tel.Compound raises Mutation.Error =

    if atomIndex < 0 || atomIndex >= compound.atoms.length
    then abort(Mutation.Error(Reason.AtomIndexOutOfRange))

    val updated = escalateAtom(compound.atoms.readable(atomIndex), text, sigil)
    compound.copy(atoms = Array.frozen(compound.atoms.readable.updated(atomIndex, updated)))

  // The §22.3 form-escalation step: keep the current form if the new value
  // is still safe for it, else advance (never retreat) to the first later
  // form whose §22.2 predicate holds.
  private def escalateAtom(current: Tel.Atom, value: Text, sigil: Char): Tel.Atom =
    current match
      case Tel.Atom.Inline(_, spaces) =>
        if inlineSafe(value, sigil) then
          // §22.2: all presentation details not targeted by the operation
          // are retained — including the atom's preceding spaces, so an
          // identity update leaves tabulation padding intact. The count
          // only escalates to a hard space when the new value introduces
          // an internal space.
          val kept = if value.s.indexOf(' ') >= 0 && spaces < 2 then 2 else spaces
          Tel.Atom.Inline(value, kept)
        else if sourceSafe(value) then Tel.Atom.Source(value)
        else Tel.Atom.Literal(literalDelimiter(value, t"---"), value)

      case Tel.Atom.Source(_) =>
        if sourceSafe(value) then Tel.Atom.Source(value)
        else Tel.Atom.Literal(literalDelimiter(value, t"---"), value)

      case Tel.Atom.Literal(delimiter, _) =>
        Tel.Atom.Literal(literalDelimiter(value, delimiter), value)

  // §22.2 `insert` — the natural position for a compound's member:
  // immediately after the last existing compound with the same keyword,
  // within the same block when that block is untabulated; in a new block
  // immediately after it when it is tabulated (rows are column-padded, so
  // splicing a plain compound in would break the geometry); or in a fresh
  // trailing block when no member group exists yet.
  private def insertNatural(blocks: Array[Tel.Block]^{}, compound: Tel.Compound)
  :   Array[Tel.Block]^{} =

    var lastB = -1
    var lastC = -1
    var b = 0

    while b < blocks.length do
      val cs = blocks.readable(b).compounds
      var c = 0

      while c < cs.length do
        if cs.readable(c).keyword == compound.keyword then
          lastB = b
          lastC = c

        c += 1

      b += 1

    if lastB < 0 then
      // No member group yet: a fresh trailing block. The previous last
      // block's trailing blank lines (which include the document's final
      // line ending) transfer to the new block, so the document's end is
      // unchanged; a tabulated last block additionally keeps one blank
      // line so the new compound isn't read as a row (§16.2).
      if blocks.length == 0
      then Array(Tel.Block(Array.empty, Unset, Array(compound), 1))
      else
        val lastIdx = blocks.length - 1
        val last = blocks.readable(lastIdx)
        val fresh = Tel.Block(Array.empty, Unset, Array(compound), last.trailingBlankLines)
        val separation = if last.tabulation.present then 1 else 0
        Array.frozen
         ( blocks.readable.updated(lastIdx, last.copy(trailingBlankLines = separation))
           :+ fresh )
    else
      val block = blocks.readable(lastB)

      if block.tabulation.present then
        val separated = block.copy(trailingBlankLines = 1)
        val fresh = Tel.Block(Array.empty, Unset, Array(compound), block.trailingBlankLines)
        Array.frozen
         ( blocks.readable.take(lastB) ++ scala.IArray(separated, fresh)
           ++ blocks.readable.drop(lastB + 1) )
      else
        val cs = block.compounds
        val compounds =
          Array.frozen
           ( cs.readable.take(lastC + 1) ++ scala.IArray(compound)
             ++ cs.readable.drop(lastC + 1) )
        Array.frozen(blocks.readable.updated(lastB, block.copy(compounds = compounds)))

  // Remove the emptied block at `blockIdx`, discarding its comments
  // (§22.2 `delete`) but not its trailing blank lines: they merge into
  // the preceding block, or — when the removed block was a compound's
  // only child block — remain as a bare run of blank lines, since a
  // nested subtree's last block carries the following line ending.
  private def removeBlock(blocks: Array[Tel.Block]^{}, blockIdx: Int, nested: Boolean)
  :   Array[Tel.Block]^{} =

    val removed = blocks.readable(blockIdx)

    if blockIdx > 0 then
      val previous = blocks.readable(blockIdx - 1)

      val trailing =
        if removed.trailingBlankLines > previous.trailingBlankLines
        then removed.trailingBlankLines else previous.trailingBlankLines

      val absorbed = previous.copy(trailingBlankLines = trailing)
      Array.frozen
       ( blocks.readable.take(blockIdx - 1) ++ scala.IArray(absorbed)
         ++ blocks.readable.drop(blockIdx + 1) )
    else if blocks.length == 1 && nested && removed.trailingBlankLines > 0
    then Array(Tel.Block(Array.empty, Unset, Array.empty, removed.trailingBlankLines))
    else Array.frozen(blocks.readable.drop(1))

  // §22.2 `insert-into-block` — append a compound to an existing block's
  // `compounds` list. For a tabulated block, every column value (the
  // keyword, then one column per inline atom) must fit its column span
  // with the two-space gap intact — otherwise the caller must
  // `resize-tabulation` first — and the row is re-padded so each column
  // value starts exactly at its marker offset (§16.2 E117).
  private def insertIntoBlock
    ( blocks: Array[Tel.Block]^{}, blockIndex: Int, compound: Tel.Compound, indent: Int )
  :   Array[Tel.Block]^{} raises Mutation.Error =

    if blockIndex < 0 || blockIndex >= blocks.length
    then abort(Mutation.Error(Reason.PointerNotFound))

    val block = blocks.readable(blockIndex)

    val padded = block.tabulation.let: tab =>
      val offsets = tab.markerOffsets
      val vs = incomingRowWidths(compound, offsets.length)
      var col = 0

      while col < offsets.length - 1 do
        if vs(col) > offsets.readable(col + 1) - offsets.readable(col) - 2
        then abort(Mutation.Error(Reason.TabulationOverflow))

        col += 1

      repadIncoming(compound, offsets, indent)

    . or(compound)

    Array.frozen
     ( blocks.readable.updated
        (blockIndex, block.copy(compounds = Array.frozen(block.compounds.readable :+ padded))) )

  // Width of `text` in code points: the spec measures column geometry in
  // code points. (The parser records marker offsets in bytes and the
  // serializer pads in UTF-16 units, so the three agree only within
  // ASCII; the spec's unit is the most defensible of the three here.)
  private def codePoints(text: Text): Int = text.s.codePointCount(0, text.s.length)

  // Column widths of an *existing* (parsed, column-aligned) row: column 0
  // is the keyword-and-pre-column portion, extended by soft-space atoms;
  // each hard-space atom starts the next column, extended by any
  // following soft-space atoms of its phrase. A source or literal atom
  // ends the inline row.
  private def existingRowWidths(compound: Tel.Compound, columns: Int): scala.Array[Int] =
    val vs = new scala.Array[Int](columns)
    var col = 0
    var width = codePoints(compound.keyword)
    var i = 0
    var stop = false

    while i < compound.atoms.length && !stop do
      compound.atoms.readable(i) match
        case Tel.Atom.Inline(text, spaces) =>
          if spaces >= 2 && col + 1 < columns then
            if width > vs(col) then vs(col) = width
            col += 1
            width = codePoints(text)
          else width += spaces + codePoints(text)

        case _ => stop = true

      i += 1

    if width > vs(col) then vs(col) = width
    vs

  // Column widths of an *incoming* row (as built by `construct` or
  // `Revision.compound`): each inline atom is one successive column
  // value, regardless of its preceding spaces.
  private def incomingRowWidths(compound: Tel.Compound, columns: Int): scala.Array[Int] =
    val vs = new scala.Array[Int](columns)
    vs(0) = codePoints(compound.keyword)
    var col = 0
    var i = 0
    var stop = false

    while i < compound.atoms.length && !stop do
      compound.atoms.readable(i) match
        case Tel.Atom.Inline(text, _) =>
          if col + 1 < columns then
            col += 1
            vs(col) = codePoints(text)
          else vs(col) += 2 + codePoints(text)

        case _ => stop = true

      i += 1

    vs

  // Re-pad an existing row to fresh marker offsets: each hard-space atom
  // (a column start) receives exactly the gap that lands it on its
  // marker; soft-space atoms keep their spacing.
  private def repadExisting
    ( compound: Tel.Compound, offsets: Array[Int]^{}, indent: Int )
  :   Tel.Compound =

    var cursor = 2*indent + codePoints(compound.keyword)
    var col = 0

    val atoms = compound.atoms.remap:
      case Tel.Atom.Inline(text, spaces) =>
        if spaces >= 2 && col + 1 < offsets.length then
          col += 1
          val gap = offsets.readable(col) - cursor
          cursor = offsets.readable(col) + codePoints(text)
          Tel.Atom.Inline(text, gap)
        else
          cursor += spaces + codePoints(text)
          Tel.Atom.Inline(text, spaces)

      case other => other

    compound.copy(atoms = atoms)

  // Re-pad an incoming row (one column value per inline atom) to the
  // block's marker offsets.
  private def repadIncoming
    ( compound: Tel.Compound, offsets: Array[Int]^{}, indent: Int )
  :   Tel.Compound =

    var cursor = 2*indent + codePoints(compound.keyword)
    var col = 0

    val atoms = compound.atoms.remap:
      case Tel.Atom.Inline(text, _) =>
        if col + 1 < offsets.length then
          col += 1
          val gap = offsets.readable(col) - cursor
          cursor = offsets.readable(col) + codePoints(text)
          Tel.Atom.Inline(text, gap)
        else
          cursor += 2 + codePoints(text)
          Tel.Atom.Inline(text, 2)

      case other => other

    compound.copy(atoms = atoms)

  // §22.2 `reorder-within-group`. Locate every occurrence of `keyword`
  // across `blocks`, build the member group as a flat sequence of
  // `(blockIndex, compoundIndex)` references, then move the element
  // at occurrence `oldIndex` to occurrence `newIndex` and rewrite the
  // affected blocks. Compounds with other keywords stay in place.
  private def reorderWithinGroup
    ( blocks: Array[Tel.Block]^{}, keyword: Text, oldIndex: Int, newIndex: Int )
  :   Array[Tel.Block]^{} raises Mutation.Error =

    val positions = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]
    var b = 0

    while b < blocks.length do
      val cs = blocks.readable(b).compounds
      var c = 0

      while c < cs.length do
        if cs.readable(c).keyword == keyword then positions += ((b, c))
        c += 1

      b += 1

    if oldIndex < 0 || oldIndex >= positions.length || newIndex < 0 || newIndex >= positions.length
    then abort(Mutation.Error(Reason.PointerNotFound))

    if oldIndex == newIndex then blocks
    else
      // Extract the moved compound, then write the group back in the
      // new order into the same (blockIndex, compoundIndex) slots.
      val movedCompound =
        val (bIdx, cIdx) = positions(oldIndex)
        blocks.readable(bIdx).compounds.readable(cIdx)

      val newGroup = scala.collection.mutable.ArrayBuffer.empty[Tel.Compound]
      var i = 0

      while i < positions.length do
        val (bIdx, cIdx) = positions(i)
        if i != oldIndex then newGroup += blocks.readable(bIdx).compounds.readable(cIdx)
        i += 1

      newGroup.insert(newIndex, movedCompound)

      // Write the reordered group back into the original slots.
      val out = scala.collection.mutable.ArrayBuffer.from(blocks.readable)
      var j = 0

      while j < positions.length do
        val (bIdx, cIdx) = positions(j)
        val block = out(bIdx)
        out(bIdx) =
          block.copy(compounds = Array.frozen(block.compounds.readable.updated(cIdx, newGroup(j))))
        j += 1

      Array.from(out)

  // §22.2 `reorder-groups`. Verify both keyword groups exist and are
  // contiguous in flat order (E309, before and after), then move the
  // `keyword` group to immediately before or after the `otherKeyword`
  // group. When the two groups occupy disjoint block sets, whole blocks
  // move, so attached comments, tabulations and blank-line counts travel
  // with their group; when both live in a single shared block, the
  // compounds are reordered within it. Any other arrangement is
  // block-level interleaving and is rejected.
  private def reorderGroups
    ( blocks: Array[Tel.Block]^{}, keyword: Text, otherKeyword: Text, placement: Placement )
  :   Array[Tel.Block]^{} raises Mutation.Error =

    // Verify flat contiguity of a group and report the block indices it
    // touches and whether every touched block holds only this group.
    def survey(kw: Text): (scala.List[Int], Boolean) =
      var present = false
      var finished = false
      var interleaved = false
      var homogeneous = true
      val touched = scala.collection.mutable.ArrayBuffer.empty[Int]
      var b = 0

      while b < blocks.length do
        val cs = blocks.readable(b).compounds
        var c = 0
        var touches = false

        while c < cs.length do
          if cs.readable(c).keyword == kw then
            if finished then interleaved = true
            present = true
            touches = true
          else if present then finished = true

          c += 1

        if touches then
          touched += b
          if cs.exists(_.keyword != kw) then homogeneous = false

        b += 1

      if !present || interleaved then abort(Mutation.Error(Reason.PointerNotFound))
      (touched.toList, homogeneous)

    val (movingBlocks, movingHomogeneous) = survey(keyword)
    val (otherBlocks, _) = survey(otherKeyword)

    if movingBlocks == otherBlocks && movingBlocks.length == 1 then
      // Both groups share one block: reorder its compounds.
      val blockIdx = movingBlocks.head
      val block = blocks.readable(blockIdx)
      val cs = block.compounds

      def run(kw: Text): (Int, Int) =
        var first = -1
        var last = -1
        var c = 0

        while c < cs.length do
          if cs.readable(c).keyword == kw then
            if first < 0 then first = c
            last = c

          c += 1

        (first, last)

      val (ms, me) = run(keyword)
      val (os, oe) = run(otherKeyword)
      val moving = cs.readable.slice(ms, me + 1)
      val removed = cs.readable.take(ms) ++ cs.readable.drop(me + 1)

      val insertAt = placement match
        case Placement.Before => if ms < os then os - moving.length else os
        case Placement.After  => if ms < os then oe + 1 - moving.length else oe + 1

      val compounds =
        Array.frozen(removed.take(insertAt) ++ moving ++ removed.drop(insertAt))

      Array.frozen(blocks.readable.updated(blockIdx, block.copy(compounds = compounds)))

    else if movingBlocks.exists(otherBlocks.contains) || !movingHomogeneous
    then abort(Mutation.Error(Reason.PointerNotFound))
    else
      // Disjoint block sets: move the whole blocks of the `keyword`
      // group. Each seam created by the move gets at least one blank
      // line of separation, so adjacent blocks don't merge (and a
      // following tabulation header can't absorb a preceding compound
      // line) on re-parse.
      def separated(block: Tel.Block): Tel.Block =
        if block.trailingBlankLines == 0 then block.copy(trailingBlankLines = 1) else block

      val moving = scala.collection.mutable.ArrayBuffer.empty[Tel.Block]
      val pruned = scala.collection.mutable.ArrayBuffer.empty[(Int, Tel.Block)]
      var b = 0

      while b < blocks.length do
        if movingBlocks.contains(b) then moving += blocks.readable(b) else pruned += ((b, blocks.readable(b)))
        b += 1

      val anchor = placement match
        case Placement.Before => pruned.indexWhere { (idx, _) => otherBlocks.contains(idx) }
        case Placement.After  => pruned.lastIndexWhere { (idx, _) => otherBlocks.contains(idx) } + 1

      val out = scala.collection.mutable.ArrayBuffer.empty[Tel.Block]
      pruned.take(anchor).foreach { (_, block) => out += block }
      moving.foreach { block => out += block }
      pruned.drop(anchor).foreach { (_, block) => out += block }

      // Identity: the groups were already in the requested arrangement.
      val unchanged =
        out.length == blocks.length && {
          var same = true
          var i = 0

          while same && i < blocks.length do
            if out(i) ne blocks.readable(i) then same = false
            i += 1

          same
        }

      if unchanged then blocks
      else
        // Blank-line separation at the three new seams: before and after
        // the moved run, and at the vacated position (the block that
        // preceded the first moved block, when it now has a successor).
        val movedFirst = anchor
        val movedLast = anchor + moving.length - 1
        if movedFirst > 0 then out(movedFirst - 1) = separated(out(movedFirst - 1))
        if movedLast < out.length - 1 then out(movedLast) = separated(out(movedLast))

        if movingBlocks.head > 0 then
          val vacated = blocks.readable(movingBlocks.head - 1)
          var v = 0

          while v < out.length - 1 do
            if out(v) eq vacated then out(v) = separated(out(v))
            v += 1

        // The final block keeps the document's original end separation: a
        // block moved to the end would otherwise turn its former
        // inter-group blank lines into stray trailing blanks.
        if out(out.length - 1) ne blocks.readable(blocks.length - 1) then
          val end = blocks.readable(blocks.length - 1).trailingBlankLines
          out(out.length - 1) = out(out.length - 1).copy(trailingBlankLines = end)

        Array.from(out)

  // §22.2 `resize-tabulation`, by the normative minimal-offsets
  // algorithm: with v_i the widest column-i value (column 0 being the
  // keyword-and-pre-column portion, and `plannedRows` counted alongside
  // the existing rows) and h_i the heading text width,
  //
  //   w_i = max(v_i, h_i + 2)          (a heading occupies sigil, space,
  //                                     heading — h_i + 2 — from its marker)
  //   markerOffsets(0) = 2 × indent
  //   markerOffsets(i) = markerOffsets(i-1) + w_(i-1) + 2
  //
  // then every existing row is re-padded so its column values start
  // exactly at the new offsets. Headings need no model change: the
  // serializer re-pads them from the offsets. A block without a
  // tabulation is rejected.
  private def resizeTabulation
    ( blocks:      Array[Tel.Block]^{},
      blockIndex:  Int,
      plannedRows: Array[Tel.Compound]^{},
      indent:      Int )
  :   Array[Tel.Block]^{} raises Mutation.Error =

    if blockIndex < 0 || blockIndex >= blocks.length
    then abort(Mutation.Error(Reason.PointerNotFound))

    val block = blocks.readable(blockIndex)
    val tab = block.tabulation.or(abort(Mutation.Error(Reason.PointerNotFound)))
    val n = tab.markerOffsets.length
    val widths = new scala.Array[Int](n)
    var col = 0

    while col < n do
      widths(col) = codePoints(tab.headings.readable(col)) + 2
      col += 1

    def fold(vs: scala.Array[Int]): Unit =
      var i = 0

      while i < n do
        if vs(i) > widths(i) then widths(i) = vs(i)
        i += 1

    block.compounds.foreach { compound => fold(existingRowWidths(compound, n)) }
    plannedRows.foreach { compound => fold(incomingRowWidths(compound, n)) }

    val newOffsets = new scala.Array[Int](n)
    newOffsets(0) = 2*indent
    var i = 1

    while i < n do
      newOffsets(i) = newOffsets(i - 1) + widths(i - 1) + 2
      i += 1

    val offsets = newOffsets.asInstanceOf[Array[Int]^{}]
    val compounds = block.compounds.remap(repadExisting(_, offsets, indent))

    Array.frozen:
      blocks.readable.updated
       ( blockIndex,
         block.copy(tabulation = Tel.Tabulation(offsets, tab.headings), compounds = compounds) )

  // §22.3 `construct` — produce a fresh compound from a keyword and a
  // sequence of scalar atom texts, choosing each atom's form by the §22.2
  // atom-form safety predicates: inline if inline-safe, else source if
  // source-safe, else literal. An empty value yields *no atom* (a leading
  // space would be an E108 trailing space on the keyword line, §22.3), so
  // empty values are dropped. The canonical sigil is `#`.
  def construct(keyword: Text, atoms: Text*): Tel.Compound =
    val atomNodes =
      Array.from(atoms.collect { case value if value.s.nonEmpty => chooseAtomForm(value, '#') })

    Tel.Compound(keyword, atomNodes, Unset, Array.empty)

  def construct(keyword: Text, members: List[Member]): Tel.Compound =
    construct(keyword, members, '#')

  // §22.2 `construct` — the full canonical-presentation algorithm over a
  // member description, iterated in member order (§22.3):
  //
  //  1. The inline run: leading single-occurrence scalar members with
  //     non-empty, inline-safe values become inline atoms, and flag
  //     members contribute their keyword as an inline atom; both
  //     continue the run. Any other member — absent, repeatable, not
  //     inline-safe, or a ready-made child — terminates it.
  //  2. A repeatable scalar member that terminates the run goes all
  //     inline when *every* occurrence is inline-safe; otherwise every
  //     occurrence becomes a compound child (occurrences are never
  //     split).
  //  3. All remaining members serialize as compound children with
  //     explicit keywords, atom forms chosen by §22.3 escalation, in a
  //     single block with no comments, tabulation, or blank lines. An
  //     empty scalar value has no faithful atom form and becomes a
  //     bare-keyword child.
  //
  // Inline atoms are soft-space-preceded up to the first value that
  // contains a space; that atom and every later one are hard-space-
  // preceded, since a single hard space puts the whole rest of the line
  // into hard-space mode (§10.3) — the spec's per-atom rule, extended so
  // the construction re-parses to the same atoms. The document's sigil
  // feeds the inline-safety check, so a custom-sigil document never
  // receives a value that would re-parse as a remark.
  def construct(keyword: Text, members: List[Member], sigil: Char): Tel.Compound =
    val inlineTexts = scala.collection.mutable.ArrayBuffer.empty[Text]
    val children = scala.collection.mutable.ArrayBuffer.empty[Tel.Compound]
    var inRun = true

    def scalarChild(kw: Text, value: Text): Tel.Compound =
      if value.s.isEmpty then Tel.Compound(kw, Array.empty, Unset, Array.empty)
      else Tel.Compound(kw, Array(chooseAtomForm(value, sigil)), Unset, Array.empty)

    members.stdlib.foreach:
      case Member.Flag(kw) =>
        if inRun then inlineTexts += kw
        else children += Tel.Compound(kw, Array.empty, Unset, Array.empty)

      case Member.Child(compound) =>
        inRun = false
        children += compound

      case Member.Break =>
        inRun = false

      case Member.Value(kw, occurrences) =>
        val os = occurrences.stdlib

        if inRun && os.length == 1 && os.head.s.nonEmpty && inlineSafe(os.head, sigil)
        then inlineTexts += os.head
        else if inRun && os.length > 1
          && os.forall { o => o.s.nonEmpty && inlineSafe(o, sigil) }
        then
          os.foreach { o => inlineTexts += o }
          inRun = false
        else
          inRun = false
          os.foreach { o => children += scalarChild(kw, o) }

    var hard = false

    val atoms = Array.from[Tel.Atom]:
      inlineTexts.map: text =>
        if text.s.indexOf(' ') >= 0 then hard = true
        Tel.Atom.Inline(text, if hard then 2 else 1)

    val childBlocks: Array[Tel.Block]^{} =
      if children.length == 0 then Array.empty
      else Array(Tel.Block(Array.empty, Unset, Array.from(children), 0))

    Tel.Compound(keyword, atoms, Unset, childBlocks)

  // §22.3 atom-form escalation: the first form in inline -> source ->
  // literal whose §22.2 safety predicate the value satisfies. Also used by
  // the derived encoders (`Tel2.scalar`), so an encoded multi-line value
  // reparses.
  private[stratiform] def chooseAtomForm(value: Text, sigil: Char): Tel.Atom =
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
  // LF and no run of two or more LFs); no line ending in a White_Space
  // character (source atoms strip trailing spaces, §14, and any other
  // trailing whitespace is visually indistinguishable); and the first
  // line does not begin with a space (the first line's indentation is
  // stripped, §14).
  private def sourceSafe(value: Text): Boolean =
    val s = value.s

    if s.isEmpty then false
    else if s.charAt(0) == '\n' || s.charAt(s.length - 1) == '\n' then false
    else if s.charAt(0) == ' ' || whitespace(s.charAt(s.length - 1)) then false
    else
      var i = 0
      var ok = true

      while ok && i < s.length do
        if s.charAt(i) == '\n' then
          if whitespace(s.charAt(i - 1)) then ok = false   // trailing whitespace on a line
          else if s.charAt(i + 1) == '\n' then ok = false  // empty interior line

        i += 1

      ok

  // The Unicode White_Space property. `Character.isWhitespace` omits the
  // no-break spaces (U+00A0, U+2007, U+202F) and NEL (U+0085); the union
  // covers White_Space in full (plus the U+001C–1F separators, which are
  // safe to over-reject).
  private def whitespace(c: Char): Boolean =
    Character.isWhitespace(c) || c == '\u0085' || c == '\u00A0' || c == '\u2007' || c == '\u202F'

  // §22.3 literal delimiter: the shortest run of `-`, starting from
  // `initial`, that does not collide with a line of the value at any
  // indentation — a position-independent sufficient check for §22.2
  // literal-safety, since the actual serialization indent isn't known here.
  private def literalDelimiter(value: Text, initial: Text): Text =
    var delimiter = initial.s
    while collidesWithDelimiterLine(value.s, delimiter) do delimiter = delimiter+"-"
    Text(delimiter)

  // True if `s` contains a line consisting of zero-or-more spaces followed
  // exactly by `delimiter`. A trailing CR is stripped before the comparison
  // because the parser strips one too when recognising the closing delimiter
  // line (§15); without that, a payload line of `<spaces><delimiter>CR` would
  // pass this check yet close the atom early on re-parse.
  private def collidesWithDelimiterLine(s: String, delimiter: String): Boolean =
    var start = 0
    var found = false

    while !found && start <= s.length do
      val nl = s.indexOf('\n', start)
      val lineEnd = if nl < 0 then s.length else nl
      var i = start
      while i < lineEnd && s.charAt(i) == ' ' do i += 1
      val end = if lineEnd > i && s.charAt(lineEnd - 1) == '\r' then lineEnd - 1 else lineEnd
      if s.substring(i, end).nn == delimiter then found = true
      start = if nl < 0 then s.length + 1 else nl + 1

    found

  // MutationError → Mutation.Error
  // Errors raised by the Mutation interpreter (§22). These describe failed
  // pointer resolutions and invariant violations of primitive operations.

  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case PointerNotFound       => m"the pointer does not resolve to a compound"
        case AtomIndexOutOfRange   => m"the atom index is out of range for the target compound"
        case FlagAlreadySet        => m"the flag is already set on the target compound"
        case TabulationOverflow    => m"the tabulation has insufficient column capacity for the row"
        case WriteUnsupported      => m"the source of the document does not support writing"

    enum Reason(val number: Int) extends Clarification:
      case PointerNotFound     extends Reason(1)
      case AtomIndexOutOfRange extends Reason(2)
      case FlagAlreadySet      extends Reason(3)
      case TabulationOverflow  extends Reason(4)
      case WriteUnsupported    extends Reason(5)

  case class Error(reason: Mutation.Error.Reason)(using Diagnostics)
  extends fulminate.Error(606, reason.ordinal)(m"the mutation failed because $reason")

