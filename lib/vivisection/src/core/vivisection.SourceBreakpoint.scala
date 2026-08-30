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
package vivisection

import java.util.concurrent.atomic as juca

import scala.collection.concurrent as scc

import contingency.*
import proscenium.*
import vacuous.*

// A revocable handle on a source-position breakpoint: the executable locations it has bound to so
// far, growing as matching classes are prepared. A frontend reads an empty list as an unverified
// breakpoint, and a binding arriving later as its verification. `clear()` removes every bound
// breakpoint and revokes the prepare request which would bind more.
class SourceBreakpoint private[vivisection] (debug: Debug, prepare: Int):
  // Lock-free: bindings arrive on the dispatcher (as classes prepare) while the handle may be
  // read, bound from the registration pass, or cleared. `record` settles each race by handing
  // back any request its caller must revoke — a duplicate binding, or one that lost against
  // `clear()`.
  private val closed: juca.AtomicBoolean = juca.AtomicBoolean(false)
  private val bindings: scc.TrieMap[Jdwp.Location, Int] = scc.TrieMap()

  // The locations this breakpoint has bound to so far; empty while every matching class remains
  // unloaded.
  def locations: List[Jdwp.Location] = List(bindings.keys.toSeq*)

  def bound: Boolean = !bindings.isEmpty

  // Whether a binding at this location would currently be admitted — the cheap pre-check which
  // spares installing a breakpoint destined for revocation.
  private[vivisection] def admits(location: Jdwp.Location): Boolean =
    !closed.get && !bindings.contains(location)

  private[vivisection] def record(location: Jdwp.Location, request: Int): Optional[Int] =
    if closed.get then request
    else if bindings.putIfAbsent(location, request) != scala.None then request
    else if closed.get then
      // `clear` may have run between the write and this check; whichever of the two removes the
      // entry owns its revocation.
      bindings.remove(location) match
        case scala.Some(request0) => request0
        case scala.None           => Unset
    else
      Unset

  def clear()(using Tactic[Debugger.Error]): Unit =
    closed.set(true)
    debug.removePrepare(prepare)

    bindings.keySet.toList.foreach: location =>
      bindings.remove(location) match
        case scala.Some(request) => debug.remove(Jdwp.EventKind.Breakpoint, request)
        case scala.None          => ()
