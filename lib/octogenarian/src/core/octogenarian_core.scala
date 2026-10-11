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
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*

// A `NoteRef` is a `Path on Notes under Git.Hash`: the commit hash is the root (it IS a `Root`,
// since `Git.Hash extends Root`) and the namespace segments are the descent, so `commit / t"foo"`
// goes straight to Serpentine's own `Path.def /`.
type NoteRef = Path on Notes under Git.Hash

extension (noteRef: NoteRef)
  // The commit hash, from the path's root (encoded as `<hash>/`).
  def target: Git.Hash = Git.Hash.unsafe(noteRef.root.keep(40))

  // The fully-qualified ref (`refs/notes/foo/bar`) naming the namespace; each segment is
  // validated here.
  def namespace(using Tactic[Git.RefError]): Git.Ref =
    def recur(path: Git.Ref, segments: List[Text]): Git.Ref = segments match
      case Nil             => path
      case segment :: rest =>
        Git.Refs.validateSegment(segment)
        recur(path / segment, rest)

    recur(Git.Refs / t"notes", noteRef.descent.to(List).reverse)

  // The note's body, decoded as `value`, read through the repository handle in scope. A note is
  // reached through a ref, which another process may move, so the result is `Stale`. Aborts with
  // `NoteNotFound` if no note is attached. (Named `content`, not `read`, so as not to collide
  // with turbulence's `Readable` `read`: a `NoteRef` is a `Path`, which is readable.)
  def content[value: Decodable in Text]
    ( using repo: (Git.Repo.Handle & Granting[Grant.Read])^, gitError: Tactic[Git.Error],
            refError: Tactic[Git.RefError] )
  :   Stale[value] =

    repo.note(noteRef.target, noteRef.namespace).let: note =>
      note.lest(Git.Error(Git.Error.Reason.NoteNotFound)).as[value]
