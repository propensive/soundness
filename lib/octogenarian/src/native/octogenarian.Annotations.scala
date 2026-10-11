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
package octogenarian

import anticipation.*
import contingency.*
import distillate.*
import galilei.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import symbolism.*
import vacuous.*

// Git notes, natively: a notes ref names a commit whose tree maps annotated objects' hashes to
// blobs, flat or fanned out into two-character directories. An edit builds a new tree and a new
// commit on no ref; the handle moves the ref by compare-and-swap.
object Annotations:
  import Git.Error.Reason as Reason

  private def notesTree(gitDir: Path on Linux, commonDir: Path on Linux, objects: Path on Linux, ref: Text)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Optional[Git.Hash] =

    Refstore.resolve(gitDir, commonDir, ref).let(Revwalk.commit(objects, _).tree)

  // Every note, as target → note blob, whichever fanout the tree uses.
  def list(gitDir: Path on Linux, commonDir: Path on Linux, objects: Path on Linux, ref: Text)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   List[(Git.Hash, Git.Hash)] =

    notesTree(gitDir, commonDir, objects, ref).lay(Nil): tree =>
      Trees.flatten(objects, tree).bind(noted(_))

  // A notes tree entry read back to the hash it annotates, whichever fanout it was stored under.
  private def flat(path: Text): Optional[Git.Hash] =
    val joined = path.s.replace("/", "").nn
    if joined.length == 40 then Git.Hash.unsafe(joined.tt) else Unset

  private def noted(entry: (Text, Git.Hash)): List[(Git.Hash, Git.Hash)] =
    flat(entry(0)).lay(Nil) { target => List((entry(1), target)) }

  def read(gitDir: Path on Linux, commonDir: Path on Linux, objects: Path on Linux, ref: Text, target: Git.Hash)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Optional[Text] =

    // Stored notes end with a newline which is not part of the note.
    def noteText(note: Git.Hash): Optional[Text] =
      Store.read(objects, note).let { (_, data) => Objects.text(data) }.let: text =>
        if text.ends(t"\n") then text.skip(1, Rtl) else text

    list(gitDir, commonDir, objects, ref).reap { case (note, `target`) => note }.let(noteText(_))

  def edit
    ( gitDir: Path on Linux, commonDir: Path on Linux, objects: Path on Linux,
      base: Optional[Git.Hash], edit: Git.Backend.NoteEdit, identity: Git.Identity )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Hash =

    val existing: List[(Git.Hash, Git.Hash)] = base.lay(Nil): base =>
      Trees.flatten(objects, Revwalk.commit(objects, base).tree).bind(noted(_)).map { (note, target) => (target, note) }

    def noteOf(target: Git.Hash): Optional[Git.Hash] =
      existing.reap { case (`target`, note) => note }

    def blob(text: Text): Git.Hash =
      val body = if text.ends(t"\n") then text else t"$text\n"
      Store.write(objects, Git.Object.Kind.Blob, Objects.bytes(body))

    val (updated, summary): (List[(Git.Hash, Git.Hash)], Text) = edit match
      case Git.Backend.NoteEdit.Add(target, body, force) =>
        if noteOf(target).present && !force then abort(Git.Error(Reason.NoteExists(target)))
        (existing.filter(_(0) != target) + List((target, blob(body))), t"Notes added by 'git notes add'")

      case Git.Backend.NoteEdit.Append(target, body) =>
        val combined = noteOf(target).lay(body): note =>
          Store.read(objects, note).lay(body) { (_, data) => t"${Objects.text(data)}\n$body" }

        (existing.filter(_(0) != target) + List((target, blob(combined))), t"Notes added by 'git notes append'")

      case Git.Backend.NoteEdit.Remove(target, ignoreMissing) =>
        if noteOf(target).absent && !ignoreMissing then abort(Git.Error(Reason.NoteNotFound))
        (existing.filter(_(0) != target), t"Notes removed by 'git notes remove'")

      case Git.Backend.NoteEdit.Copy(source, target, force) =>
        if noteOf(target).present && !force then abort(Git.Error(Reason.NoteExists(target)))
        val note = noteOf(source).or(abort(Git.Error(Reason.NoteNotFound)))
        (existing.filter(_(0) != target) + List((target, note)), t"Notes added by 'git notes copy'")

    val entries = updated.map: (target, note) =>
      Git.Object.Tree.Entry(Git.Object.Tree.Mode.Regular, target.text, note)

    val tree = Trees.write(objects, entries)
    val signature = Git.Signature(identity, Git.Timestamp.now())
    val payload = Objects.serializeCommit(tree, base.let(List(_)).or(Nil), signature, signature, summary)
    Store.write(objects, Git.Object.Kind.Commit, payload)
