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

import anticipation.*
import rudiments.*
import contingency.*
import vacuous.*

// Composable edit DSL built atop the primitive Mutation.Op interpreter
// (§22.5). A `Revision` is an ordered op-log; revisions compose with `++`
// to form longer sequences and apply to a `Tel` value with `revision(tel)`
// or the `Tel#edited` extension. Pointer resolution happens lazily at apply
// time, so each operation in the sequence addresses the *intermediate*
// document produced by the preceding operations.

object Revision:

  val noop: Revision = new Revision(Array.empty)

  // Begin a new edit anchored at `pointer`. The returned cursor exposes
  // the per-operation builders; each call produces a fresh `Revision` value
  // that the caller may compose further with `++`.
  def at(pointer: Tel.Pointer): Cursor = Cursor(pointer)

  // Construct a fresh compound from a keyword and a flat list of inline-
  // atom texts. Convenience for assembling Insert / Replace payloads
  // without explicit `Tel.Compound(...)` boilerplate.
  def compound(keyword: Text, atomTexts: Text*): Tel.Compound =
    // Explicit element types: the frozen `Array` is invariant, so the atoms must be
    // built at `Tel.Atom` rather than upcast from `Tel.Atom.Inline`.
    val atoms = Array.from[Tel.Atom](atomTexts.map(Tel.Atom.Inline(_, 1)))
    Tel.Compound(keyword, atoms, Unset, Array.empty[Tel.Block])

  // A cursor binds a pointer to the upcoming operation. Each operation
  // method returns a `Revision` (singleton op-log) that can be `++`-chained
  // with further revisions.
  case class Cursor(pointer: Tel.Pointer):
    def update(text: Text): Revision = update(0, text)

    def update(atomIndex: Int, text: Text): Revision =
      Revision.single(Mutation.Op.UpdateAtom(pointer, atomIndex, text))

    def delete: Revision = Revision.single(Mutation.Op.Delete(pointer))

    def replace(compound: Tel.Compound): Revision =
      Revision.single(Mutation.Op.Replace(pointer, compound))

    def insert(compound: Tel.Compound): Revision =
      Revision.single(Mutation.Op.Insert(pointer, compound))

    def insertBefore(compound: Tel.Compound): Revision =
      Revision.single(Mutation.Op.InsertBefore(pointer, compound))

    def insertAfter(compound: Tel.Compound): Revision =
      Revision.single(Mutation.Op.InsertAfter(pointer, compound))

    // §22.2 `insert-into-block` — pointer addresses the parent; append
    // `compound` as a row of the `blockIndex`-th child block, re-padded
    // to the block's tabulation if it has one.
    def insertIntoBlock(blockIndex: Int, compound: Tel.Compound): Revision =
      Revision.single(Mutation.Op.InsertIntoBlock(pointer, blockIndex, compound))

    def attachRemark(text: Text): Revision =
      Revision.single(Mutation.Op.AttachRemark(pointer, text))

    def removeRemark: Revision = Revision.single(Mutation.Op.RemoveRemark(pointer))

    def setFlag(keyword: Text): Revision =
      Revision.single(Mutation.Op.SetFlag(pointer, keyword))

    def unsetFlag(keyword: Text): Revision =
      Revision.single(Mutation.Op.UnsetFlag(pointer, keyword))

    // §22.2 `reorder-within-group` — pointer addresses the parent;
    // move the `oldIndex`-th occurrence of `keyword` to `newIndex`.
    def reorderWithinGroup(keyword: Text, oldIndex: Int, newIndex: Int): Revision =
      Revision.single(Mutation.Op.ReorderWithinGroup(pointer, keyword, oldIndex, newIndex))

    // §22.2 `reorder-groups` — pointer addresses the parent; move the
    // member group of `keyword` immediately before (default) or after
    // the member group of `otherKeyword`.
    def reorderGroups(keyword: Text, otherKeyword: Text): Revision =
      reorderGroups(keyword, otherKeyword, Mutation.Placement.Before)

    def reorderGroups(keyword: Text, otherKeyword: Text, placement: Mutation.Placement)
    :   Revision =
      Revision.single(Mutation.Op.ReorderGroups(pointer, keyword, otherKeyword, placement))

    // §22.2 `resize-tabulation` — pointer addresses the parent;
    // recompute marker offsets for the tabulation in `blockIndex`-th
    // child block via the minimal-offsets algorithm, accommodating any
    // `plannedRows` about to be inserted.
    def resizeTabulation(blockIndex: Int, plannedRows: Tel.Compound*): Revision =
      Revision.single
        ( Mutation.Op.ResizeTabulation(pointer, blockIndex, Array.from(plannedRows)) )

  // §22.2 `construct` — build a fresh compound from a keyword and a
  // sequence of scalar atom texts, picking inline / source / literal
  // atom forms via the §22.3 escalation algorithm.
  def construct(keyword: Text, atoms: Text*): Tel.Compound =
    Mutation.construct(keyword, atoms*)

  // §22.2 `construct` over a full member description: the §22.3
  // canonical-presentation algorithm, producing inline atoms for the
  // leading run and compound children for the rest.
  def construct(keyword: Text, members: List[Mutation.Member]): Tel.Compound =
    Mutation.construct(keyword, members)

  private def single(op: Mutation.Op): Revision = new Revision(Array.of(op))


case class Revision private[stratiform] (ops: Array[Mutation.Op]^{}):
  def ++ (next: Revision): Revision =
    new Revision(Array.frozen(ops.readable ++ next.ops.readable))

  def apply(tel: Tel): Tel raises Mutation.Error = Mutation(tel, ops.to[List])
