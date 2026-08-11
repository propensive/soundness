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

import java.nio.charset.StandardCharsets

import scala.caps

import anticipation.*
import aperture.*
import contingency.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import zephyrine.*

// Scoped machine editing of a TEL source through aperture's `Openable`:
// `source.open[Tel](Read & Write) { handle ?=> ... }` parses the source on
// entry and provides a `TelHandle` for the duration of the block. The
// grant-gated operations below expose the §22.2 machine-operation set: a
// handle with the Read grant offers the current document snapshot, and one
// with the Write grant offers the mutation operations, each applied
// eagerly — a rejected operation aborts `Mutation.Error` at its call site
// and provably leaves the document unchanged. On normal completion of a
// writable open, the document is serialized (preserving the interpreter
// directive, pragma, and line endings) and written back to the source; an
// abort or exception skips the write-back entirely.

// Flags for `open[Tel]`.
enum TelFlag:
  // Serialize and write back on scope close even when no mutation was
  // applied: a canonicalizing rewrite of the source.
  case Force

object TelHandle:
  extension (handle: (TelHandle & Granting[Grant.Read])^)
    // The current document: an immutable `Tel` snapshot, so the whole
    // navigation and decoding surface applies to it, and it may safely
    // outlive the block — only the handle itself is confined. Named
    // `current` because `tel` is claimed by the generic encoding
    // extension and `snapshot` by rudiments.
    transparent inline def current: Tel = handle.current0

    transparent inline def metadata: Optional[Tel.Metadata] = handle.metadata0

  extension (handle: (TelHandle & Granting[Grant.Write])^)
    // The general seam: apply any §22.2 machine operation, or a composed
    // `Revision` op-log, to the document.
    transparent inline def mutate(op: Mutation.Op)(using Tactic[Mutation.Error]): Unit =
      handle.mutate0(op)

    transparent inline def revise(revision: Revision)(using Tactic[Mutation.Error]): Unit =
      handle.revise0(revision)

    // §22.2 `update-value` — rewrite the primary (or `atomIndex`-th) atom.
    transparent inline def update(pointer: Tel.Pointer, text: Text)
      ( using Tactic[Mutation.Error] )
    :   Unit =
      handle.mutate0(Mutation.Op.UpdateAtom(pointer, 0, text))

    transparent inline def update(pointer: Tel.Pointer, atomIndex: Int, text: Text)
      ( using Tactic[Mutation.Error] )
    :   Unit =
      handle.mutate0(Mutation.Op.UpdateAtom(pointer, atomIndex, text))

    // §22.2 `insert` — natural-position insertion under the parent at `pointer`.
    transparent inline def insert(pointer: Tel.Pointer, compound: Tel.Compound)
      ( using Tactic[Mutation.Error] )
    :   Unit =
      handle.mutate0(Mutation.Op.Insert(pointer, compound))

    transparent inline def insertBefore(pointer: Tel.Pointer, compound: Tel.Compound)
      ( using Tactic[Mutation.Error] )
    :   Unit =
      handle.mutate0(Mutation.Op.InsertBefore(pointer, compound))

    transparent inline def insertAfter(pointer: Tel.Pointer, compound: Tel.Compound)
      ( using Tactic[Mutation.Error] )
    :   Unit =
      handle.mutate0(Mutation.Op.InsertAfter(pointer, compound))

    // §22.2 `insert-into-block` — append a row to the `blockIndex`-th child
    // block of the parent at `pointer`.
    transparent inline def insertIntoBlock
      ( pointer: Tel.Pointer, blockIndex: Int, compound: Tel.Compound )
      ( using Tactic[Mutation.Error] )
    :   Unit =
      handle.mutate0(Mutation.Op.InsertIntoBlock(pointer, blockIndex, compound))

    // §22.2 `delete` — named `remove` because galilei claims `delete`.
    transparent inline def remove(pointer: Tel.Pointer)(using Tactic[Mutation.Error]): Unit =
      handle.mutate0(Mutation.Op.Delete(pointer))

    transparent inline def replace(pointer: Tel.Pointer, compound: Tel.Compound)
      ( using Tactic[Mutation.Error] )
    :   Unit =
      handle.mutate0(Mutation.Op.Replace(pointer, compound))

    transparent inline def attachRemark(pointer: Tel.Pointer, text: Text)
      ( using Tactic[Mutation.Error] )
    :   Unit =
      handle.mutate0(Mutation.Op.AttachRemark(pointer, text))

    transparent inline def removeRemark(pointer: Tel.Pointer)(using Tactic[Mutation.Error]): Unit =
      handle.mutate0(Mutation.Op.RemoveRemark(pointer))

    transparent inline def setFlag(pointer: Tel.Pointer, keyword: Text)
      ( using Tactic[Mutation.Error] )
    :   Unit =
      handle.mutate0(Mutation.Op.SetFlag(pointer, keyword))

    transparent inline def unsetFlag(pointer: Tel.Pointer, keyword: Text)
      ( using Tactic[Mutation.Error] )
    :   Unit =
      handle.mutate0(Mutation.Op.UnsetFlag(pointer, keyword))

    transparent inline def reorderWithinGroup
      ( pointer: Tel.Pointer, keyword: Text, oldIndex: Int, newIndex: Int )
      ( using Tactic[Mutation.Error] )
    :   Unit =
      handle.mutate0(Mutation.Op.ReorderWithinGroup(pointer, keyword, oldIndex, newIndex))

    transparent inline def reorderGroups
      ( pointer:      Tel.Pointer,
        keyword:      Text,
        otherKeyword: Text,
        placement:    Mutation.Placement )
      ( using Tactic[Mutation.Error] )
    :   Unit =
      handle.mutate0(Mutation.Op.ReorderGroups(pointer, keyword, otherKeyword, placement))

    transparent inline def resizeTabulation
      ( pointer: Tel.Pointer, blockIndex: Int, plannedRows: Tel.Compound* )
      ( using Tactic[Mutation.Error] )
    :   Unit =
      handle.mutate0
        (Mutation.Op.ResizeTabulation(pointer, blockIndex, Array.from(plannedRows)))

// The scoped handle to a TEL document under edit. Public `*0` members
// rather than private: the grant-gated operations are transparent-inline
// extensions, and a private member's inline accessor bridge fails capture
// checking. The vars hold pure data, so they are untracked, exactly as
// aperture's own handle fixtures.
class TelHandle private[stratiform] (initial: Tel) extends caps.ExclusiveCapability:
  @scala.caps.unsafe.untrackedCaptures var current0: Tel = initial
  @scala.caps.unsafe.untrackedCaptures var dirty0: Boolean = false

  def metadata0: Optional[Tel.Metadata] = current0.subtree match
    case document: Tel.Document =>
      Tel.Metadata(document.interpreterDirective, document.pragma, document.lineEndings)

    case _ =>
      Unset

  // `Mutation` is pure and assignment happens only on success, so a
  // rejected operation cannot leave a partially-applied document.
  def mutate0(op: Mutation.Op)(using Tactic[Mutation.Error]): Unit =
    current0 = Mutation(current0, op)
    dirty0 = true

  def revise0(revision: Revision)(using Tactic[Mutation.Error]): Unit =
    current0 = revision(current0)
    dirty0 = true

// Named classes rather than anonymous given instances: instantiating an
// anonymous subclass freshens the handle's capability type in the
// inferred `Result` member (see the equivalent comment on galilei's
// `FileOpenable`).
class TelOpenable[source]
  ( using readable: (source is Readable to Tel)^,
          writable: (source is Writable by Data)^ )
extends Openable:
  type Self = source
  type Form = Tel
  type Operand = TelFlag
  type Result = TelHandle

  def open[grants <: Grant, result]
    ( value: source, mode: Mode granting grants, flags: List[TelFlag] )
    ( block: ((TelHandle & Granting[grants])^) ?=> result )
  :   result =

    val handle = new TelHandle(readable.read(value)) with Granting[grants] {}
    val outcome = block(using handle)

    // Write-back happens only on normal completion — an abort or an
    // exception skips it — and only when the document was mutated or the
    // `Force` flag asks for a canonicalizing rewrite. TEL text is
    // UTF-8-encoded by specification, independent of any ambient encoder.
    if mode.atoms.has(Write) && (handle.dirty0 || flags.has(TelFlag.Force)) then
      val bytes: Data =
        Array.unsafeFrozen(handle.current0.show.s.getBytes(StandardCharsets.UTF_8).nn)

      writable.write(value, Stream(bytes))

    outcome

// The read-only counterpart, for sources that can be parsed but offer no
// write-back: a `Write` mode is refused at open time.
class TelViewOpenable[source]
  ( using readable: (source is Readable to Tel)^, mutationError: Tactic[Mutation.Error] )
extends Openable:
  type Self = source
  type Form = Tel
  type Operand = TelFlag
  type Result = TelHandle

  def open[grants <: Grant, result]
    ( value: source, mode: Mode granting grants, flags: List[TelFlag] )
    ( block: ((TelHandle & Granting[grants])^) ?=> result )
  :   result =

    if mode.atoms.has(Write)
    then abort(Mutation.Error(Mutation.Error.Reason.WriteUnsupported))

    block(using new TelHandle(readable.read(value)) with Granting[grants] {})
