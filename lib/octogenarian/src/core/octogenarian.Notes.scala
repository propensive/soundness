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
import gossamer.*
import kaleidoscope.*
import prepositional.*
import serpentine.*
import spectacular.*

// `Notes` is the Serpentine path plane for git notes.  Each path on this
// plane is rooted at the commit hash being annotated (which IS the `Root`
// — `Git.Hash` extends `Root` directly) and its descent is the namespace
// under `refs/notes/`.
//
// Modelling notes as Serpentine paths lets `commit / t"foo" / t"bar"` reuse
// Path's own `def /` (which preserves singleton tracking, runs the
// Admissibility check, and produces a refined `Path of (...)` type), rather
// than going through a custom `Divisible` instance or a Conversion lift.
object Notes:
  type Plane = Notes

  given filesystem: Notes is Filesystem:
    val name: Text = t"Notes"
    val separator: Text = t"/"
    val self: Text = t"@"
    val parent: Text = t".."

  // Serpentine's `/` consults `Admissible` only to decide platformed-vs-
  // unplatformed; it does not invoke `check` at construction time.  Per-
  // segment validation runs lazily in `NoteRef.namespace` (which materialises
  // the canonical `Path on Git.Refs` for the underlying git operation).
  given admissible: [text <: Text] => text is Admissible on Notes = _ => ()

  // `Git.Hash` is the root of a notes path.  Encoded form is `<hash>/`
  // (40 hex chars + separator), so an empty-descent path encodes as
  // `<hash>/` and `commit / t"foo"` encodes as `<hash>/foo`.
  given radical: Git.Hash is Radical:
    type Plane = Notes
    def length(text: Text): Int raises Path.Error = 41

    def decode(text: Text): Git.Hash raises Path.Error = text match
      case r"$hash([a-f0-9]{40})/.*" => Git.Hash.unsafe(hash)
      case _                         => abort(Path.Error(_.InvalidRoot))

    def encode(hash: Git.Hash): Text = t"${hash.text}/"

trait Notes
