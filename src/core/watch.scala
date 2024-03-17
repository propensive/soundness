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

import turbulence.*
import rudiments.*
import vacuous.*
import digression.*
import parasite.*, threadModels.platform
import feudalism.*
import fulminate.*
import contingency.*
import spectacular.*
import anticipation.*

import scala.collection.mutable as scm

import java.nio.file as jnf, jnf.StandardWatchEventKinds.*

case class WatchError()
extends Error(msg"the operating system's limit on the number of paths that can be watched has been exceeded")

extension [PathType: GenericPath](path: PathType)
  def watch[ResultType](lambda: WatchSet => ResultType): ResultType raises WatchError =
    val watchSet = Watch(List(path))
    lambda(watchSet).also:
      Watch.unregister(watchSet)

extension [PathType: GenericPath](paths: Iterable[PathType])
  def watch[ResultType](lambda: WatchSet => ResultType): ResultType raises WatchError =
    val watchSet = Watch(paths)
    lambda(watchSet).also:
      Watch.unregister(watchSet)

object Watch:
  def apply[PathType: GenericPath](paths: Iterable[PathType]): WatchSet =
    register:
      paths.map(_.fullPath.s).map(jnf.Paths.get(_).nn).map: javaPath =>
        if javaPath.toFile.nn.isDirectory then (javaPath, (_: Text) => true)
        else (javaPath.getParent.nn, (_: Text) == javaPath.getFileName.nn.toString.tt)
      .toMap

  private case class WatchService(watchService: jnf.WatchService, pollLoop: Loop):
    def stop(): Unit = pollLoop.stop()
    val task: Optional[Async[Unit]] = safely(supervise(daemon(pollLoop.run())))

  private val watches: Mutex[scm.HashMap[jnf.WatchKey, Set[Watch]]] = Mutex(scm.HashMap())
  private var serviceValue: Optional[WatchService] = Unset

  private def service: WatchService = serviceValue.or:
    synchronized:
      jnf.FileSystems.getDefault.nn.newWatchService().nn.pipe: watchService =>
        WatchService(watchService, pollLoop(watchService)).tap: service =>
          serviceValue = service

  private def register(paths: Map[jnf.Path, Text => Boolean]): WatchSet =
    val funnel = Funnel[WatchEvent]()

    val watchSet =
      paths.map: (path, filter) =>
        val key = path.register(service.watchService, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE).nn
        new Watch(key, path, funnel, filter).tap: watch =>
          watches.mutate: map =>
            map(key) = map.at(key).or(Set()) + watch
      .to(Set)
    
    WatchSet(funnel, watchSet)

  private[surveillance] def unregister(watchSet: WatchSet): Unit =
    watches.mutate: map =>
      watchSet.watches.each: watch =>
        map(watch.key) = map.at(watch.key).or(Set()) - watch
      
        if map(watch.key).isEmpty then
          watch.key.cancel()
          map.remove(watch.key)
        
        if map.isEmpty then synchronized:
          serviceValue.let: service =>
            service.stop()
            serviceValue = Unset


  private def put(watch: Watch, event: jnf.WatchEvent[?]): Unit =
    (event.context.nn: @unchecked) match
      case path: jnf.Path =>
        val name = path.toString.tt
        if watch.filter(name) then try event.kind match
          case ENTRY_CREATE =>
            if watch.base.resolve(path).nn.toFile.nn.isDirectory
            then watch.funnel.put(WatchEvent.NewDirectory(watch.base.toString.show, name))
            else watch.funnel.put(WatchEvent.NewFile(watch.base.toString.show, name))
          
          case ENTRY_MODIFY =>
            watch.funnel.put(WatchEvent.Modify(watch.base.toString.show, name))
          
          case ENTRY_DELETE =>
            watch.funnel.put(WatchEvent.Delete(watch.base.toString.show, name))
          
          case _ => 
            ()
        
        catch case err: Exception => ()

  private def pollLoop(service: jnf.WatchService): Loop = loop:
    service.take().nn match
      case key: jnf.WatchKey =>
        key.pollEvents().nn.iterator.nn.asScala.each: event =>
          watches.read: ref =>
            ref()(key)
          .each(put(_, event))
  
        key.reset()

class WatchSet(funnel: Funnel[WatchEvent], private[surveillance] val watches: Set[Watch]):
  def stream: LazyList[WatchEvent] = funnel.stream

class Watch
    (private[surveillance] val key: jnf.WatchKey,
     private[surveillance] val base: jnf.Path,
                           val funnel: Funnel[WatchEvent],
                           val filter: Text => Boolean)

enum WatchEvent:
  case NewFile(dir: Text, file: Text)
  case NewDirectory(dir: Text, directory: Text)
  case Modify(dir: Text, file: Text)
  case Delete(dir: Text, file: Text)

  def dir: Text

  def path[DirectoryType: SpecificPath]: DirectoryType = unsafely:
    val relPath = this match
      case NewFile(_, file)      => file
      case NewDirectory(_, path) => path
      case Modify(_, file)       => file
      case Delete(_, path)       => path

      SpecificPath(jnf.Paths.get(dir.s, relPath.show.s).nn.normalize.nn.toString.show)

export WatchEvent.{NewFile, NewDirectory, Modify, Delete}

given Realm = realm"surveillance"
