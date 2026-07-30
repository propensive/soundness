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

import java.nio.file as jnf

import anticipation.*
import aperture.*
import contingency.*
import prepositional.*
import turbulence.*
import zephyrine.*
import vacuous.*

object Watch:
  def apply[path: Abstractable across Paths to Text](paths: Iterable[path])(using watcher: Watcher)
  :   Watch raises WatchError =

    val pathGroups =
      paths.map(_.generic.s).map(jnf.Paths.get(_).nn).map: javaPath =>
        if javaPath.toFile.nn.isDirectory then (javaPath, (_: Text) => true)
        else
          val parent = Optional(javaPath.getParent).or(jnf.Paths.get("").nn)
          val filename = javaPath.getFileName.nn.toString.tt
          (parent, (_: Text) == filename)

      . groupBy(_(0)).view.mapValues(_.map(_(1)))

    val directories: Map[jnf.Path, Text -> Boolean] = Map.from:
      pathGroups.mapValues: predicates =>
        (value: Text) => predicates.exists(_(value))

    val spool: Relay[WatchEvent] = Relay()

    new Watch(spool, watcher.watch(directories, spool))

  given openable: [path: Abstractable across Paths to Text]
  =>  ( watcher: Watcher, tactic: Tactic[WatchError] )
  =>  ( WatchOpenable[path]^{tactic} ) =
    WatchOpenable[path]

  given allOpenable: [path: Abstractable across Paths to Text, collection <: Iterable[path]]
  =>  ( watcher: Watcher, tactic: Tactic[WatchError] )
  =>  ( WatchAllOpenable[collection, path]^{tactic} ) =
    WatchAllOpenable[collection, path]

// A `Watch` is the user-facing handle returned by registering one or more paths. Its `stream`
// yields events as they occur, and `unregister` cancels the backend registration and terminates
// the stream. The actual change-detection is delegated to a `Watcher` backend.
class Watch(spool: Relay[WatchEvent], registration: Watcher.Registration):
  // The legacy view of the event relay (the audited bridge): one lazy,
  // single-owner drain of the shared queue, as before.
  def stream: Progression[WatchEvent] = Progression.from(spool.stream.records)

  def unregister(): Unit =
    registration.cancel()
    spool.stop()
