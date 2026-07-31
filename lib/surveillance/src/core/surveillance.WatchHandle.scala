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
package surveillance

import scala.caps

import anticipation.*
import aperture.*
import contingency.*
import prepositional.*

// The scoped capability provided by opening a path (or several) as `Watch`:
// `path.open[Watch]()`. The OS watch registration lasts exactly as long as the block, and the
// handle — with anything lazily derived from its event stream — is confined to it by capture
// checking. Watching is pure observation, so no operation is grant-gated: the mode is
// irrelevant, and `Read` (the default) describes it best.
class WatchHandle private[surveillance] (watch: Watch) extends caps.ExclusiveCapability:
  def stream: Chain[WatchEvent] = watch.stream

// A named class rather than an anonymous given instance, for the reasons documented on
// galilei's `FileOpenable`.
class WatchOpenable[path: Abstractable across Paths to Text]
  ( using watcher: Watcher, watchError: Tactic[WatchError] )
extends Openable:

  type Self = path
  type Form = Watch
  type Operand = Nothing
  type Result = WatchHandle

  def open[grants <: Grant, result]
    ( value: path, mode: Mode granting grants, flags: List[Nothing] )
    ( block: ((WatchHandle & Granting[grants])^) ?=> result )
  :   result =

    val watch = Watch(List(value).stdlib)
    try block(using new WatchHandle(watch) with Granting[grants] {})
    finally watch.unregister()

// Watching several paths at once: `List(a, b, c).open[Watch]()`, with one event stream
// multiplexing all of them, exactly as `Watch` itself works. Parameterized over the concrete
// collection type, since `Openable`'s `Self` is matched invariantly.
class WatchAllOpenable[collection <: Iterable[path], path: Abstractable across Paths to Text]
  ( using watcher: Watcher, watchError: Tactic[WatchError] )
extends Openable:

  type Self = collection
  type Form = Watch
  type Operand = Nothing
  type Result = WatchHandle

  def open[grants <: Grant, result]
    ( value: collection, mode: Mode granting grants, flags: List[Nothing] )
    ( block: ((WatchHandle & Granting[grants])^) ?=> result )
  :   result =

    val watch = Watch(value)
    try block(using new WatchHandle(watch) with Granting[grants] {})
    finally watch.unregister()
