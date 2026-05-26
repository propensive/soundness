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
import vacuous.*

import MutationError.Reason

// Primitive presentation-preserving mutations per §22.2. Each op is a
// local rewrite addressed by a TelPointer; surrounding atoms, comments,
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
    case UpdateAtom(pointer: TelPointer, atomIndex: Int, text: Text)
    case Insert(pointer: TelPointer, compound: Tel.Compound)
    case InsertBefore(pointer: TelPointer, compound: Tel.Compound)
    case InsertAfter(pointer: TelPointer, compound: Tel.Compound)
    case Delete(pointer: TelPointer)
    case Replace(pointer: TelPointer, compound: Tel.Compound)
    case AttachRemark(pointer: TelPointer, text: Text)
    case RemoveRemark(pointer: TelPointer)
    case SetFlag(pointer: TelPointer, keyword: Text)
    case UnsetFlag(pointer: TelPointer, keyword: Text)

  private def pointerOf(op: Op): TelPointer = op match
    case Op.UpdateAtom(p, _, _)   => p
    case Op.Insert(p, _)          => p
    case Op.InsertBefore(p, _)    => p
    case Op.InsertAfter(p, _)     => p
    case Op.Delete(p)             => p
    case Op.Replace(p, _)         => p
    case Op.AttachRemark(p, _)    => p
    case Op.RemoveRemark(p)       => p
    case Op.SetFlag(p, _)         => p
    case Op.UnsetFlag(p, _)       => p

  def apply(tel: Tel, op: Op): Tel raises MutationError =
    Tel.make(transform(tel.subtree, pointerOf(op).steps, 0, op))

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
         steps:    IArray[TelPointer.Step],
         idx:      Int,
         op:       Op )
  :     Tel.Subtree raises MutationError =

    if idx >= steps.length then op match
      case Op.Insert(_, compound) =>
        rewrap(subtree, appendCompound(subtree.children, compound))

      case _ => abort(MutationError(Reason.PointerNotFound))

    else
      val step = steps(idx)
      val (blockIdx, localIdx) = findTarget(subtree.children, step)

      val isTargetOp = op match
        case Op.Insert(_, _) => false
        case _               => true

      if isTargetOp && idx == steps.length - 1 then
        val updatedBlock = applyToTarget(subtree.children(blockIdx), localIdx, op)
        rewrap(subtree, subtree.children.updated(blockIdx, updatedBlock))
      else
        val targetBlock = subtree.children(blockIdx)
        val targetCompound = targetBlock.compounds(localIdx)
        val updatedSubtree = transform(targetCompound, steps, idx + 1, op)
        val updatedCompound = updatedSubtree match
          case c: Tel.Compound => c
          case _: Tel.Document => targetCompound // unreachable: child of a compound is a compound

        val updatedBlock = targetBlock.copy
                            (compounds = targetBlock.compounds.updated(localIdx, updatedCompound))

        rewrap(subtree, subtree.children.updated(blockIdx, updatedBlock))

  private def rewrap(subtree: Tel.Subtree, children: IArray[Tel.Block]): Tel.Subtree =
    subtree match
      case d: Tel.Document => d.copy(children = children)
      case c: Tel.Compound => c.copy(children = children)

  // Locate the `(blockIndex, compoundIndex)` of the n-th compound matching
  // `step.keyword`. `step.index` defaults to 0 — i.e. "the first match".
  // Counting walks all blocks in order so siblings with the same keyword
  // spread across multiple blocks remain addressable.
  private def findTarget(blocks: IArray[Tel.Block], step: TelPointer.Step)
  :     (Int, Int) raises MutationError =
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
          else seen += 1

        c += 1

      b += 1

    if foundBlock < 0 then abort(MutationError(Reason.PointerNotFound))
    (foundBlock, foundLocal)

  // Apply a target-addressed op to the compound at `localIdx` within
  // `block`, returning the rewritten block. Ops may produce zero, one, or
  // two compounds in place of the target (e.g. Delete -> 0, InsertBefore
  // -> 2).
  private def applyToTarget(block: Tel.Block, localIdx: Int, op: Op)
  :     Tel.Block raises MutationError =
    val target = block.compounds(localIdx)
    val replacement: IArray[Tel.Compound] = op match
      case Op.UpdateAtom(_, atomIndex, text) =>
        IArray(updateAtomAt(target, atomIndex, text))

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

      case Op.Insert(_, _) =>
        // unreachable: handled in transform's container-mode arm
        abort(MutationError(Reason.PointerNotFound))

    val before = block.compounds.take(localIdx)
    val after = block.compounds.drop(localIdx + 1)
    block.copy(compounds = before ++ replacement ++ after)

  // Modify the n-th inline atom of a compound (counting only Inline
  // atoms; Source and Literal atoms are skipped from the index, since
  // those have a different mutation surface).
  private def updateAtomAt(compound: Tel.Compound, atomIndex: Int, text: Text)
  :     Tel.Compound raises MutationError =
    var seen = 0
    var found = -1
    var i = 0
    while i < compound.atoms.length && found < 0 do
      compound.atoms(i) match
        case _: Tel.Atom.Inline =>
          if seen == atomIndex then found = i else seen += 1

        case _ => ()

      i += 1

    if found < 0 then abort(MutationError(Reason.AtomIndexOutOfRange))
    compound.atoms(found) match
      case Tel.Atom.Inline(_, sp) =>
        compound.copy(atoms = compound.atoms.updated(found, Tel.Atom.Inline(text, sp)))

      case _ => abort(MutationError(Reason.AtomIndexOutOfRange))

  // Append `compound` to the last block of `blocks`. If `blocks` is empty
  // a fresh block is created. Trailing blank lines on the existing last
  // block are preserved.
  private def appendCompound(blocks: IArray[Tel.Block], compound: Tel.Compound)
  :     IArray[Tel.Block] =
    if blocks.length == 0
    then IArray(Tel.Block(IArray.empty, Unset, IArray(compound), 0))
    else
      val lastIdx = blocks.length - 1
      val last = blocks(lastIdx)
      blocks.updated(lastIdx, last.copy(compounds = last.compounds :+ compound))
