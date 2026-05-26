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

// Composable edit DSL built atop the primitive Mutation.Op interpreter
// (§22.5). An `Edit` is an ordered op-log; edits compose with `++` to
// form longer sequences and apply to a `Tel` value with `edit(tel)` or
// the `Tel#edited` extension. Pointer resolution happens lazily at apply
// time, so each operation in the sequence addresses the *intermediate*
// document produced by the preceding operations.

object Edit:

  val noop: Edit = new Edit(IArray.empty)

  // Begin a new edit anchored at `pointer`. The returned cursor exposes
  // the per-operation builders; each call produces a fresh `Edit` value
  // that the caller may compose further with `++`.
  def at(pointer: TelPointer): Cursor = Cursor(pointer)

  // Construct a fresh compound from a keyword and a flat list of inline-
  // atom texts. Convenience for assembling Insert / Replace payloads
  // without explicit `Tel.Compound(...)` boilerplate.
  def compound(keyword: Text, atomTexts: Text*): Tel.Compound =
    val atoms = IArray.from(atomTexts.map(t => Tel.Atom.Inline(t, 1)))
    Tel.Compound(keyword, atoms, Unset, IArray.empty)

  // A cursor binds a pointer to the upcoming operation. Each operation
  // method returns an `Edit` (singleton op-log) that can be `++`-chained
  // with further edits.
  case class Cursor(pointer: TelPointer):
    def update(text: Text): Edit = update(0, text)
    def update(atomIndex: Int, text: Text): Edit =
      Edit.single(Mutation.Op.UpdateAtom(pointer, atomIndex, text))

    def delete: Edit = Edit.single(Mutation.Op.Delete(pointer))

    def replace(compound: Tel.Compound): Edit =
      Edit.single(Mutation.Op.Replace(pointer, compound))

    def insert(compound: Tel.Compound): Edit =
      Edit.single(Mutation.Op.Insert(pointer, compound))

    def insertBefore(compound: Tel.Compound): Edit =
      Edit.single(Mutation.Op.InsertBefore(pointer, compound))

    def insertAfter(compound: Tel.Compound): Edit =
      Edit.single(Mutation.Op.InsertAfter(pointer, compound))

    def attachRemark(text: Text): Edit =
      Edit.single(Mutation.Op.AttachRemark(pointer, text))

    def removeRemark: Edit = Edit.single(Mutation.Op.RemoveRemark(pointer))

    def setFlag(keyword: Text): Edit =
      Edit.single(Mutation.Op.SetFlag(pointer, keyword))

    def unsetFlag(keyword: Text): Edit =
      Edit.single(Mutation.Op.UnsetFlag(pointer, keyword))

  private def single(op: Mutation.Op): Edit = new Edit(IArray(op))

case class Edit private[stratiform] (ops: IArray[Mutation.Op]):
  def ++ (next: Edit): Edit = new Edit(ops ++ next.ops)

  def apply(tel: Tel): Tel raises MutationError = Mutation(tel, ops.toSeq)

extension (tel: Tel)
  def edited(edit: Edit): Tel raises MutationError = edit(tel)
