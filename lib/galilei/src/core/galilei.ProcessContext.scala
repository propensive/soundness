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
package galilei

import anticipation.*
import contingency.*
import gigantism.Every
import vacuous.*

// Some paths name something only in the process that opened them: `/dev/fd/63`, `/dev/stdin`,
// `/proc/self/fd/3` are entries of one process's descriptor table, and opening them from any
// other process reaches that process's descriptor of the same number, or nothing. An
// application run by a daemon on behalf of a client — the Ethereal daemon serving an `xek`
// launcher — receives such paths as arguments and must open the *client's* descriptors, which
// only the launcher can reach. A `ProcessContext` is the client's side of that: consulted
// before every open, read, write and existence check, it answers for the paths it governs
// with a `Descriptor` that opens the right thing, and declines every other path, which then
// goes to the filesystem as it always did.
//
// It reaches galilei as a contextual value, after the manner of `Umask`: a `Provider` — the
// daemon's service handle — makes the invocation's context summonable with no import, and
// where none is in scope the no-op fallback applies. The sites that consult it take
// `Every[ProcessContext]`, so contexts may be layered, and the first that answers for a path
// wins.
object ProcessContext extends ProcessContextPriority:
  trait Provider:
    def processContext: Optional[ProcessContext]

  // What a governed path names: something that can be opened for the operations its
  // `flags` ask for — which must be ones the descriptor admits — and read or written through
  // the handle `lambda` is given, which is closed when `lambda` returns. A descriptor that
  // cannot be opened so — one whose path names nothing in the governing process (a
  // `/dev/fd/N` the client does not hold), or one opened for writing that admits only
  // reading — throws a `Refusal`, which the site that consulted the context raises as the
  // `Io.Error` of the path it was opening; a context has no path of its own to name.
  trait Descriptor:
    def open[result](flags: List[OpenFlag])(lambda: Handle => result): result

  case class Refusal(reason: Io.Error.Reason) extends Exception

  // Answers for no path.
  val none: ProcessContext = _ => Unset

  given provided: (provider: Provider^) => ProcessContext = provider.processContext.or(none)

  // The first answer among every context in scope, if any answers: a `Provider`'s, where
  // there is one, since the fallback never answers.
  def resolve(contexts: Every[ProcessContext], path: Text): Optional[Descriptor] =
    contexts.values.iterator.map(_.descriptor(path)).find(_.present).getOrElse(Unset)

// Lower priority than the companion's `provided` (a class's givens take precedence over its
// parent's), so a `Provider` in scope wins and, absent one, nothing changes.
private[galilei] trait ProcessContextPriority:
  given inherited: ProcessContext = ProcessContext.none

trait ProcessContext:
  // The descriptor a path names in this process, or `Unset` for a path this context does not
  // govern. The path is in its encoded form, as the platform would name it.
  def descriptor(path: Text): Optional[ProcessContext.Descriptor]
