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
package octogenarian

import ambience.*
import anticipation.*
import contingency.*
import distillate.*
import galilei.*
import gossamer.*
import guillotine.*
import nomenclature.*
import prepositional.*
import serpentine.*

package gitCommands:
  given environmentDefaultGitCommand: ( WorkingDirectory, GitEvent is Loggable, Tactic[NameError],
                                        Tactic[PathError], Tactic[IoError], Tactic[ExecError] )
  =>  (Path on Linux) is Instantiable across Paths from Text
  =>  GitCommand =

    val path: Path on Linux = sh"which git"()
    GitCommand(path.encode)

export octogenarian.internal.{GitTag, GitBranch, GitHash, Refspec}

// A `NoteRef` is just a `Path on Notes under GitHash`: the commit hash sits
// at the root (it IS a `Root`, since `GitHash extends Root`), the namespace
// segments sit in the descent.  Because `GitHash` and `NoteRef` are both
// `Path` subtypes, `commit / t"foo"` and `noteRef / t"bar"` both go straight
// to Serpentine's own `Path.def /` — Octogenarian defines no `/` of its own.
type NoteRef = Path on Notes under GitHash

extension (noteRef: NoteRef)
  // Extract the commit hash from the path's root (encoded as `<hash>/`).
  def target: GitHash = GitHash.unsafe(noteRef.root.keep(40))

  // The fully-qualified git ref path (e.g. `refs/notes/foo/bar`) that names
  // the namespace; each descent segment is validated here.
  def namespace(using Tactic[GitRefError]): Path on GitRefs =
    val initial: Path on GitRefs = GitRefs / t"notes"

    noteRef.descent.reverse.foldLeft(initial): (path, segment) =>
      GitRefs.validateSegment(segment)
      path / segment

  // Fetch the note body via `git notes show` on the implicit `GitRepo` and
  // decode it as `value`.  Aborts with `GitError(NoteNotFound)` if no note
  // is attached. (Named `content`, not `read`, to avoid colliding with
  // turbulence's `Readable` `read`: a `NoteRef` is a `Path`, which is readable.)
  def content[value: Decodable in Text]
    ( using repo:    GitRepo,
            command: GitCommand,
            wd:      WorkingDirectory,
            gitErr:  Tactic[GitError],
            refErr:  Tactic[GitRefError],
            exec:    Tactic[ExecError] )
  :   value logs GitEvent =

    repo.notes.show(noteRef.target, noteRef.namespace)
    . lest(GitError(GitError.Reason.NoteNotFound))
    . decode[value]

private[octogenarian] inline def git(using command: GitCommand): GitCommand = command
