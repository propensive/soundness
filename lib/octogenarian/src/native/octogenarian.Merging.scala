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
import aperture.*
import contingency.*
import dissonance.*
import galilei.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import symbolism.*
import turbulence.*
import vacuous.*

import sortingAlgorithms.timsort
import filesystemBackends.javaBaseFilesystem
import filesystemOptions.createNonexistentParents
import filesystemOptions.overwritePreexisting
import filesystemOptions.preserveSymlinks
import filesystemOptions.deleteRecursively

// Three-way merging: of trees, path by path, and of text, line by line (diff3). A merge either
// settles every path or leaves some in conflict, with the stages git records for them and the
// marked-up content git would leave in the working file.
object Merging:
  import Git.Error.Reason as Reason

  // A path on which the two sides disagree irreconcilably: the entry each side has for it (base,
  // ours, theirs; absent where the side has no such path) and, for a text file, the content with
  // conflict markers to leave in the working tree.
  case class Conflict
    ( path:    Text,
      base:    Optional[(Int, Git.Hash)],
      ours:    Optional[(Int, Git.Hash)],
      theirs:  Optional[(Int, Git.Hash)],
      content: Optional[Data] )

  case class Result(merged: Workspace.Snapshot, conflicts: List[Conflict]):
    def clean: Boolean = conflicts == Nil

  // Lines with their terminators kept, so that a file without a final newline survives the merge.
  def lines(data: Data): List[Text] =
    val text = Objects.text(data)

    def recur(from: Int, acc: List[Text]): List[Text] =
      if from >= text.length then acc.reverse else
        val newline = text.s.indexOf('\n', from)
        if newline < 0 then (text.skip(from) :: acc).reverse
        else recur(newline + 1, text.s.substring(from, newline + 1).nn.tt :: acc)

    recur(0, Nil)

  private def terminated(line: Text): Text = if line.ends(t"\n") then line else t"$line\n"

  private def binary(data: Data): Boolean =
    val array = data.asInstanceOf[scala.Array[Byte]]
    def recur(index: Int): Boolean = index < array.length.min(8000) && (array(index) == 0 || recur(index + 1))
    recur(0)

  // Line-based three-way merge. Base lines matched on both sides in step are kept; between
  // them, a chunk changed on one side only takes that side, and a chunk changed differently on
  // both sides is a conflict, written out between git's markers.
  def diff3(base: List[Text], ours: List[Text], theirs: List[Text], oursLabel: Text, theirsLabel: Text)
  :   (List[Text], Boolean) =

    val baseList = java.util.ArrayList[Text]()
    val oursList = java.util.ArrayList[Text]()
    val theirsList = java.util.ArrayList[Text]()
    base.each(baseList.add(_))
    ours.each(oursList.add(_))
    theirs.each(theirsList.add(_))
    val baseSize = baseList.size
    val oursSize = oursList.size
    val theirsSize = theirsList.size

    // For each base line, the line it matches on each side, or -1.
    val oursMatch = java.nio.IntBuffer.allocate(baseSize.max(1)).nn
    val theirsMatch = java.nio.IntBuffer.allocate(baseSize.max(1)).nn

    def fill(buffer: java.nio.IntBuffer, edits: List[Edit[Text]]): Unit =
      def clear(index: Int): Unit = if index < baseSize then
        buffer.put(index, -1)
        clear(index + 1)

      def record(remaining: List[Edit[Text]]): Unit = remaining match
        case Par(left, right, _) :: rest => buffer.put(left, right); record(rest)
        case _ :: rest                   => record(rest)
        case _                           => ()

      clear(0)
      record(edits)

    fill(oursMatch, diff(Sequence(base*), Sequence(ours*)).edits.to(List))
    fill(theirsMatch, diff(Sequence(base*), Sequence(theirs*)).edits.to(List))

    def slice(list: java.util.ArrayList[Text], from: Int, until: Int): List[Text] =
      def recur(index: Int, acc: List[Text]): List[Text] =
        if index < from then acc else recur(index - 1, list.get(index).nn :: acc)
      recur(until - 1, Nil)

    // The next base line matched on both sides at or after `index`, or the end.
    def anchor(index: Int): Int =
      if index >= baseSize then baseSize
      else if oursMatch.get(index) >= 0 && theirsMatch.get(index) >= 0 then index
      else anchor(index + 1)

    def resolve(baseChunk: List[Text], oursChunk: List[Text], theirsChunk: List[Text]): (List[Text], Boolean) =
      if oursChunk == theirsChunk then (oursChunk, false)
      else if oursChunk == baseChunk then (theirsChunk, false)
      else if theirsChunk == baseChunk then (oursChunk, false)
      else
        val marked =
          List(t"<<<<<<< $oursLabel\n") + oursChunk.map(terminated(_)) + List(t"=======\n") +
            theirsChunk.map(terminated(_)) + List(t">>>>>>> $theirsLabel\n")

        (marked, true)

    def recur(index: Int, oursIndex: Int, theirsIndex: Int, acc: List[Text], conflicted: Boolean)
    :   (List[Text], Boolean) =

      if index >= baseSize && oursIndex >= oursSize && theirsIndex >= theirsSize then (acc.reverse, conflicted)
      else if index < baseSize && oursMatch.get(index) == oursIndex && theirsMatch.get(index) == theirsIndex then
        recur(index + 1, oursIndex + 1, theirsIndex + 1, baseList.get(index).nn :: acc, conflicted)
      else
        val next = anchor(index)
        val oursNext = if next >= baseSize then oursSize else oursMatch.get(next)
        val theirsNext = if next >= baseSize then theirsSize else theirsMatch.get(next)

        val (resolved, conflict) =
          resolve
            ( slice(baseList, index, next),
              slice(oursList, oursIndex, oursNext),
              slice(theirsList, theirsIndex, theirsNext) )

        recur(next, oursNext, theirsNext, resolved.reverse + acc, conflicted || conflict)

    recur(0, 0, 0, Nil, false)

  // Merges the content of one path changed on both sides: the merged blob if the changes do not
  // overlap, or the marked-up content if they do (binary content always conflicts, ours kept).
  def content
    ( base: Optional[Data], ours: Data, theirs: Data, oursLabel: Text, theirsLabel: Text )
  :   (Data, Boolean) =

    if binary(ours) || binary(theirs) || base.lay(false)(binary(_)) then (ours, true)
    else
      val (merged, conflicted) =
        diff3(base.lay(Nil)(lines(_)), lines(ours), lines(theirs), oursLabel, theirsLabel)

      (Objects.bytes(merged.join(t"")), conflicted)

  private def blob(objects: Path on Linux, entry: (Int, Git.Hash))(using Tactic[Git.Error], Tactic[Io.Error]): Data =
    Store.readOr(objects, entry(1))(1)

  private def textual(entry: Optional[(Int, Git.Hash)]): Boolean = entry match
    case (mode: Int, _: Git.Hash) => mode != Index.symlink && mode != Index.gitlink
    case _                        => false

  // Merges three snapshots path by path. A path changed on one side only takes that side's
  // entry; one changed identically on both takes either; a text file changed differently on both
  // is merged line by line; anything else is a conflict.
  def trees
    ( objects: Path on Linux, base: Workspace.Snapshot, ours: Workspace.Snapshot,
      theirs: Workspace.Snapshot, oursLabel: Text, theirsLabel: Text )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Result =

    val paths = (base.keys.to[List] + ours.keys.to[List] + theirs.keys.to[List]).distinct

    def settle(path: Text): Either[Conflict, Optional[(Int, Git.Hash)]] =
      val baseEntry = base.at(path)
      val oursEntry = ours.at(path)
      val theirsEntry = theirs.at(path)

      if oursEntry == theirsEntry then Right(oursEntry)
      else if oursEntry == baseEntry then Right(theirsEntry)
      else if theirsEntry == baseEntry then Right(oursEntry)
      else if textual(oursEntry) && textual(theirsEntry) then
        val oursPair = oursEntry.or(abort(Git.Error(Reason.Unexpected)))
        val theirsPair = theirsEntry.or(abort(Git.Error(Reason.Unexpected)))
        val mode = if oursPair(0) == baseEntry.let(_(0)) then theirsPair(0) else oursPair(0)
        val baseData = if textual(baseEntry) then baseEntry.let(blob(objects, _)) else Unset

        val (merged, conflicted) =
          content(baseData, blob(objects, oursPair), blob(objects, theirsPair), oursLabel, theirsLabel)

        if conflicted then Left(Conflict(path, baseEntry, oursEntry, theirsEntry, merged))
        else Right((mode, Store.write(objects, Git.Object.Kind.Blob, merged)))
      else
        // A modify/delete or type conflict: the working file keeps whichever side has content,
        // ours first.
        val kept = oursEntry.or(theirsEntry)
        val data = if textual(kept) then kept.let(blob(objects, _)) else Unset
        Left(Conflict(path, baseEntry, oursEntry, theirsEntry, data))

    def recur(remaining: List[Text], merged: List[(Text, (Int, Git.Hash))], conflicts: List[Conflict]): Result =
      remaining match
        case path :: rest => settle(path) match
          case Left(conflict) => recur(rest, merged, conflict :: conflicts)
          case Right(entry)   => entry match
            case (mode: Int, hash: Git.Hash) => recur(rest, (path, (mode, hash)) :: merged, conflicts)
            case _                           => recur(rest, merged, conflicts)

        case _ => Result(merged.to[Map], conflicts.reverse)

    recur(paths, Nil, Nil)

  private def stageEntry(path: Text, stage: Int, entry: (Int, Git.Hash)): Index.Entry =
    Index.Entry(path, entry(0), entry(1), stage, 0, 0, 0, 0, 0, 0, 0, 0, 0)

  private def sortKey(entry: Index.Entry): String = t"${entry.path}\u0000${entry.stage}".s

  // Brings the index and working files to a merge result: settled paths are checked out and
  // staged; conflicted paths get their marked-up content and stages 1 to 3. Returns the
  // conflicted paths.
  def apply
    ( gitDir: Path on Linux, objects: Path on Linux, worktree: Path on Linux, result: Result )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   List[Text] =

    val contents = Index.read(gitDir)
    val current = Workspace.indexSnapshot(contents)
    val settled = Workspace.checkoutFiles(objects, worktree, current, result.merged)

    def write(conflict: Conflict): List[Index.Entry] =
      conflict.content.let: data =>
        val target = Workspace.file(worktree, conflict.path)
        target.parent.let(Refstore.ensureDirectory(_))
        if target.existent() then target.delete()
        target.write(data)
        val mode = conflict.ours.or(conflict.theirs).let(_(0)).or(Index.regular)
        target.executable() = mode == Index.executable

      conflict.base.let(stageEntry(conflict.path, 1, _)).let(List(_)).or(Nil) +
        conflict.ours.let(stageEntry(conflict.path, 2, _)).let(List(_)).or(Nil) +
        conflict.theirs.let(stageEntry(conflict.path, 3, _)).let(List(_)).or(Nil)

    val staged = result.conflicts.bind(write(_))
    val entries = (settled + staged).order(sortKey(_))
    Index.write(gitDir, Index.Contents(contents.version, entries, contents.extensions))
    result.conflicts.map(_.path)
