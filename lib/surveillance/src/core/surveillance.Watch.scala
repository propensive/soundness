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
import fulminate.*
import prepositional.*
import scala.caps
import spectacular.*
import turbulence.*
import zephyrine.*
import vacuous.*

object Watch:
  def apply[path: Abstractable across Paths to Text](paths: Iterable[path])(using watcher: Watcher)
  :   Watch raises Watch.Error =

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

    val spool: Relay[Watch.Event] = Relay()

    new Watch(spool, watcher.watch(directories, spool))

  given openable: [path: Abstractable across Paths to Text]
  =>  ( watcher: Watcher, tactic: Tactic[Watch.Error] )
  =>  ( Openable[path]^{tactic} ) =
    Openable[path]

  given allOpenable: [path: Abstractable across Paths to Text, collection <: Iterable[path]]
  =>  ( watcher: Watcher, tactic: Tactic[Watch.Error] )
  =>  ( AllOpenable[collection, path]^{tactic} ) =
    AllOpenable[collection, path]

  // WatchError → Watch.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case LimitExceeded     extends Reason(1)
      case Nonexistent       extends Reason(2)
      case NotDirectory      extends Reason(3)
      case PermissionDenied  extends Reason(4)

    given communicable: Reason is Communicable =
      case Reason.LimitExceeded =>
        m"the operating system's limit on watched paths has been exceeded"

      case Reason.Nonexistent      => m"the path does not exist"
      case Reason.NotDirectory     => m"the path is not a directory"
      case Reason.PermissionDenied => m"the user does not have permission to watch the path"

  case class Error(reason: Watch.Error.Reason)(using Diagnostics)
  extends fulminate.Error(545, reason.number)(m"the path could not be watched because $reason")

  // WatchEvent → Watch.Event
  enum Event:
    case NewFile(dir: Text, file: Text)
    case NewDirectory(dir: Text, directory: Text)
    case Modify(dir: Text, file: Text)
    case Delete(dir: Text, file: Text)

    def dir: Text

    def path[directory: Instantiable across Paths from Text]: directory = unsafely:
      val relPath = this match
        case NewFile(_, file)      => file
        case NewDirectory(_, path) => path
        case Modify(_, file)       => file
        case Delete(_, path)       => path

        directory(jnf.Paths.get(dir.s, relPath.show.s).nn.normalize.nn.toString.show)

  // WatchHandle → Watch.Handle
  // The scoped capability provided by opening a path (or several) as `Watch`:
  // `path.open[Watch]()`. The OS watch registration lasts exactly as long as the block, and the
  // handle — with anything lazily derived from its event stream — is confined to it by capture
  // checking. Watching is pure observation, so no operation is grant-gated: the mode is
  // irrelevant, and `Read` (the default) describes it best.
  class Handle private[surveillance] (watch: Watch) extends caps.ExclusiveCapability:
    def stream: Chain[Watch.Event] = watch.stream

  // A named class rather than an anonymous given instance, for the reasons documented on
  // galilei's `FileOpenable`.
  class Openable[path: Abstractable across Paths to Text]
    ( using watcher: Watcher, watchError: Tactic[Watch.Error] )
  extends aperture.Openable:

    type Self = path
    type Form = Watch
    type Operand = Nothing
    type Result = Watch.Handle

    def open[grants <: Grant, result]
      ( value: path, mode: Mode granting grants, flags: List[Nothing] )
      ( block: ((Watch.Handle & Granting[grants])^) ?=> result )
    :   result =

      val watch = Watch(List(value).stdlib)
      try block(using new Watch.Handle(watch) with Granting[grants] {})
      finally watch.unregister()

  // Watching several paths at once: `List(a, b, c).open[Watch]()`, with one event stream
  // multiplexing all of them, exactly as `Watch` itself works. Parameterized over the concrete
  // collection type, since `Openable`'s `Self` is matched invariantly.
  class AllOpenable[collection <: Iterable[path], path: Abstractable across Paths to Text]
    ( using watcher: Watcher, watchError: Tactic[Watch.Error] )
  extends aperture.Openable:

    type Self = collection
    type Form = Watch
    type Operand = Nothing
    type Result = Watch.Handle

    def open[grants <: Grant, result]
      ( value: collection, mode: Mode granting grants, flags: List[Nothing] )
      ( block: ((Watch.Handle & Granting[grants])^) ?=> result )
    :   result =

      val watch = Watch(value)
      try block(using new Watch.Handle(watch) with Granting[grants] {}) finally watch.unregister()

// A `Watch` is the user-facing handle returned by registering one or more paths. Its `stream`
// yields events as they occur, and `unregister` cancels the backend registration and terminates
// the stream. The actual change-detection is delegated to a `Watcher` backend.
class Watch(spool: Relay[Watch.Event], registration: Watcher.Registration):
  // The legacy view of the event relay (the audited bridge): one lazy,
  // single-owner drain of the shared queue, as before.
  def stream: Chain[Watch.Event] = Chain.from(spool.stream.records)

  def unregister(): Unit =
    registration.cancel()
    spool.stop()
