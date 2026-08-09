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
import rudiments.*
import dissonance.*
import gossamer.*
import kaleidoscope.*
import vacuous.*

object Patch:
  // Streams a `git diff -p --no-color` output as one FileDiff per file.
  // Common cases (modify, add, delete, rename, copy) are handled. Binary
  // diffs and mode-only changes produce a FileDiff with no hunks.
  def parse(lines: Iterator[Text]^): List[FileDiff] =
    val cursor = lines.buffered
    while cursor.hasNext && !cursor.head.starts(t"diff --git ") do cursor.next()
    val files = scala.collection.mutable.ListBuffer[FileDiff]()

    while cursor.hasNext do
      val header = cursor.next()
      val body = scala.collection.mutable.ListBuffer[Text]()
      while cursor.hasNext && !cursor.head.starts(t"diff --git ") do body += cursor.next()
      files += parseFile(header, body.to(List))

    files.to(List)

  // Flattens every hunk's edits into a single Dissonance Diff[Text]. Assembled exclusively from
  // `Retained` edits, so the `Diff` carries the proof.
  def asDiff(file: FileDiff): Diff[Text] & Retained =
    Diff(file.hunks.stdlib.flatMap(_.edits.stdlib)*).retained

  private def parseHunkRange(text: Text): (Int, Int) = text.cut(t",") match
    case List(start)        => (start.s.toInt, 1)
    case List(start, count) => (start.s.toInt, count.s.toInt)
    case _                  => (0, 0)

  private def parseFile(header: Text, body: List[Text]): FileDiff =
    case class State
      ( oldPath:    Optional[Text]               = Unset,
        newPath:    Optional[Text]               = Unset,
        changeKind: ChangeKind                   = ChangeKind.Modified,
        hunks:      List[Hunk]                   = Nil,
        // Current hunk being read (with running line counters), if any.
        hunk:       Optional[Hunk]               = Unset,
        edits:      List[Edit[Text] & Retained]  = Nil,
        oldLine:    Int                          = 0,
        newLine:    Int                          = 0 ):

      def closeHunk(): State =
        hunk.lay(this): h =>
          copy(hunks = h.copy(edits = edits.reverse) :: hunks, hunk = Unset, edits = Nil)

      def push(edit: Edit[Text] & Retained): State = copy(edits = edit :: edits)

    val initial = header match
      case r"diff --git a/$old(.*) b/$nu(.*)" =>
        State(oldPath = old, newPath = nu)

      case _ =>
        State()

    def step(state: State, line: Text): State = line match
      case t"--- /dev/null" =>
        state.copy(oldPath = Unset, changeKind = ChangeKind.Added)

      case r"--- a/$path(.*)" =>
        state.copy(oldPath = path)

      case r"\+\+\+ /dev/null" =>
        state.copy(newPath = Unset, changeKind = ChangeKind.Deleted)

      case r"\+\+\+ b/$path(.*)" =>
        state.copy(newPath = path)

      case r"rename from $path(.*)" =>
        state.copy(oldPath = path, changeKind = ChangeKind.Renamed)

      case r"rename to $path(.*)" =>
        state.copy(newPath = path, changeKind = ChangeKind.Renamed)

      case r"copy from $path(.*)" =>
        state.copy(oldPath = path, changeKind = ChangeKind.Copied)

      case r"copy to $path(.*)" =>
        state.copy(newPath = path, changeKind = ChangeKind.Copied)

      case r"new file mode .*" =>
        state.copy(changeKind = ChangeKind.Added)

      case r"deleted file mode .*" =>
        state.copy(changeKind = ChangeKind.Deleted)

      case r"@@ -$oldRange(\S+) \+$newRange(\S+) @@$section(.*)" =>
        val flushed = state.closeHunk()
        val (oldStart, oldLines) = parseHunkRange(oldRange)
        val (newStart, newLines) = parseHunkRange(newRange)
        val hunk = Hunk(oldStart, oldLines, newStart, newLines, section.trim, Nil)
        flushed.copy(hunk = hunk, oldLine = oldStart, newLine = newStart)

      case other if state.hunk.present =>
        if other.starts(t"+") then
          val text = other.skip(1)
          state.push(Ins(state.newLine, text).retained).copy(newLine = state.newLine + 1)
        else if other.starts(t"-") then
          val text = other.skip(1)
          state.push(Del(state.oldLine, text).retained).copy(oldLine = state.oldLine + 1)
        else if other.starts(t" ") then
          val text = other.skip(1)
          val advanced = state.copy(oldLine = state.oldLine + 1, newLine = state.newLine + 1)
          advanced.push(Par(state.oldLine, state.newLine, text).retained)
        else
          state  // "\ No newline at end of file" or stray

      case _ =>
        state  // unrecognized header outside a hunk (e.g. `index ...`)

    val finalState = body.stdlib.foldLeft(initial)(step).closeHunk()

    FileDiff
      ( finalState.oldPath,
        finalState.newPath,
        finalState.changeKind,
        finalState.hunks.reverse )

case class Patch(files: List[FileDiff])
