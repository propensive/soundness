/*
    Surveillance, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package surveillance

import java.nio.file as jnf, jnf.StandardWatchEventKinds.*

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import feudalism.*
import parasite.*, threadModels.platform
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*

object Watch:
  private case class WatchService(watchService: jnf.WatchService, pollLoop: Loop):
    import orphanDisposal.await
    def stop(): Unit = pollLoop.stop()
    val async: Optional[Task[Unit]] = safely(supervise(task("surveillance".tt)(pollLoop.run())))

  private var serviceValue: Optional[WatchService] = Unset

  private def service: WatchService = serviceValue.or:
    synchronized:
      jnf.FileSystems.getDefault.nn.newWatchService().nn.pipe: watchService =>
        WatchService(watchService, pollLoop(watchService)).tap: service =>
          serviceValue = service

  private val watches: Mutex[scm.HashMap[jnf.WatchKey, Set[Watch#PathWatch]]] = Mutex(scm.HashMap())

  private def register(paths: Map[jnf.Path, Text => Boolean]): Watch =
    new Watch().tap(_.watch(paths))

  private def pollLoop(service: jnf.WatchService): Loop = loop:
    service.take().nn match
      case key: jnf.WatchKey =>
        key.pollEvents().nn.iterator.nn.asScala.each: event =>
          watches.use: ref =>
            ref()(key)
          .each(_.put(event))

        key.reset()

  def apply[PathType: GenericPath](paths: Iterable[PathType]): Watch =
    Watch.register:
      val pathGroups: Map[jnf.Path, Iterable[Text => Boolean]] =
        paths.map(_.fullPath.s).map(jnf.Paths.get(_).nn).map: javaPath =>
          if javaPath.toFile.nn.isDirectory then (javaPath, (_: Text) => true)
          else (javaPath.getParent.nn, (_: Text) == javaPath.getFileName.nn.toString.tt)
        .groupBy(_(0)).view.mapValues(_.map(_(1))).to(Map)

      pathGroups.view.mapValues: predicates =>
        (value: Text) => predicates.exists(_(value))
      .to(Map)

class Watch():
  private val spool: Spool[WatchEvent] = Spool()
  private val watches: scm.HashSet[PathWatch] = scm.HashSet[PathWatch]()

  private class PathWatch
      (private[Watch] val key: jnf.WatchKey,
       private[Watch] val base: jnf.Path,
                      val spool: Spool[WatchEvent],
                      val filter: Text => Boolean):

    def put(event: jnf.WatchEvent[?]): Unit =
      (event.context.nn: @unchecked) match
        case path: jnf.Path =>
          val name = path.toString.tt
          if filter(name) then try event.kind match
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

          catch case err: Exception => ()

  def stream: LazyList[WatchEvent] = spool.stream

  def watch(paths: Map[jnf.Path, Text => Boolean]): Unit =
    val watches2 = paths.map:
      case (path, filter) =>
        val key = path.register(Watch.service.watchService, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE).nn

        new PathWatch(key, path, spool, filter).tap: watch =>
          Watch.watches.isolate: map =>
            map(key) = map.at(key).or(Set()) + watch

    synchronized(watches ++= watches2)

  def unregister(): Unit =
    Watch.watches.isolate: map =>
      watches.each: watch =>
        map(watch.key) = map.at(watch.key).or(Set()) - watch

        if map(watch.key).isEmpty then
          watch.key.cancel()
          map.remove(watch.key)

        if map.isEmpty then synchronized:
          Watch.serviceValue.let: service =>
            service.stop()
            Watch.serviceValue = Unset
