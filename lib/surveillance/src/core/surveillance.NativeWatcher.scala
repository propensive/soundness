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
package surveillance

import java.io as ji
import java.nio.file as jnf, jnf.StandardWatchEventKinds.*

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import denominative.*
import parasite.*, threading.platform
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*

// The native backend wraps the operating system's filewatching service (`java.nio.file
// .WatchService`). A single service and a single polling thread are shared across every
// registration; keys are reference-counted so the service is closed only once nothing is being
// watched.
object NativeWatcher extends Watcher:
  private case class WatchService(watchService: jnf.WatchService, pollLoop: Loop):
    import probates.await

    def stop(): Unit =
      pollLoop.stop()
      try watchService.close() catch case _: ji.IOException => ()

    val async: Optional[Task[Unit]] = safely(supervise(task("surveillance".tt)(pollLoop.run())))

  private val serviceMutex: Mutex = Mutex()
  private val watchesMutex: Mutex = Mutex()
  @volatile private var serviceValue: Optional[WatchService] = Unset

  private def service: WatchService = serviceValue.or:
    serviceMutex:
      serviceValue.or:
        jnf.FileSystems.getDefault.nn.newWatchService().nn.pipe: watchService =>
          WatchService(watchService, pollLoop(watchService)).tap: service =>
            serviceValue = service

  private val watches: scm.HashMap[jnf.WatchKey, Set[PathWatch]] = scm.HashMap()

  private def pollLoop(service: jnf.WatchService): Loop = loop:
    try
      service.take().nn match
        case key: jnf.WatchKey =>
          val pathWatches = watchesMutex(watches.at(key).or(Set()))

          key.pollEvents().nn.iterator.nn.asScala.each: event =>
            pathWatches.each(_.put(event))

          // `reset` returns `false` once the key is no longer valid (e.g. the watched
          // directory was deleted); drop it so the map doesn't retain dead keys.
          if !key.reset() then watchesMutex(watches.remove(key))

    catch case _: jnf.ClosedWatchServiceException => ()

  private def registerKey(directory: jnf.Path): jnf.WatchKey raises WatchError =
    try directory.register(service.watchService, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE).nn
    catch
      case _: jnf.NoSuchFileException   => abort(WatchError(WatchError.Reason.Nonexistent))
      case _: jnf.NotDirectoryException => abort(WatchError(WatchError.Reason.NotDirectory))
      case _: jnf.AccessDeniedException => abort(WatchError(WatchError.Reason.PermissionDenied))
      case _: SecurityException         => abort(WatchError(WatchError.Reason.PermissionDenied))

      // `register` on an existing, accessible directory has essentially one remaining
      // failure mode: the operating system's watch limit (e.g. inotify `max_user_watches`).
      case _: ji.IOException            => abort(WatchError(WatchError.Reason.LimitExceeded))

  def watch(directories: Map[jnf.Path, Text => Boolean], spool: Spool[WatchEvent])
  :   Watcher.Registration raises WatchError =

    val pathWatches: Set[PathWatch] = watchesMutex:
      directories.map:
        case (directory, filter) =>
          val key = registerKey(directory)

          new PathWatch(key, directory, spool, filter).tap: pathWatch =>
            watches(key) = watches.at(key).or(Set()) + pathWatch

      . to(Set)

    Registration(pathWatches)

  private class PathWatch
    ( private[NativeWatcher] val key:    jnf.WatchKey,
      private[NativeWatcher] val base:   jnf.Path,
                             val spool:  Spool[WatchEvent],
                             val filter: Text => Boolean ):

    def put(event: jnf.WatchEvent[?]): Unit =
      event.context.nn.absolve match
        case path: jnf.Path =>
          val name = path.toString.tt

          if filter(name) then
            try
              event.kind match
                case ENTRY_CREATE =>
                  if base.resolve(path).nn.toFile.nn.isDirectory
                  then spool.put(WatchEvent.NewDirectory(base.toString.show, name))
                  else spool.put(WatchEvent.NewFile(base.toString.show, name))

                case ENTRY_MODIFY =>
                  spool.put(WatchEvent.Modify(base.toString.show, name))

                case ENTRY_DELETE =>
                  spool.put(WatchEvent.Delete(base.toString.show, name))

                case _ =>
                  ()

            catch case error: Exception => ()

  private class Registration(pathWatches: Set[PathWatch]) extends Watcher.Registration:
    def cancel(): Unit =
      watchesMutex:
        pathWatches.each: pathWatch =>
          watches(pathWatch.key) = watches.at(pathWatch.key).or(Set()) - pathWatch

          if watches.at(pathWatch.key).or(Set()).nil then
            pathWatch.key.cancel()
            watches.remove(pathWatch.key)

        if watches.nil then serviceMutex:
          serviceValue.let: service =>
            service.stop()
            serviceValue = Unset
