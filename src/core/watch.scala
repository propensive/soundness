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

import eucalyptus.*
import turbulence.*
import gossamer.*
import rudiments.*
import vacuous.*
import digression.*
import parasite.*
import fulminate.*
import contingency.*
import spectacular.*
import anticipation.*

import scala.collection.mutable.HashMap

import java.nio.file as jnf
import jnf.StandardWatchEventKinds.*

case class InotifyError()
extends Error(msg"the limit on the number of paths that can be watched has been exceeded")

extension [DirectoryType: SpecificDirectory: GenericDirectory](dirs: Seq[DirectoryType])(using Monitor)
  def watch()(using Log[Text], GenericWatchService[DirectoryType]): Watcher[DirectoryType] raises InotifyError =
    Watcher[DirectoryType](dirs*)

extension [DirectoryType: SpecificDirectory: GenericDirectory](dir: DirectoryType)(using Monitor)
  def watch()(using Log[Text], GenericWatchService[DirectoryType]): Watcher[DirectoryType] raises InotifyError =
    Watcher[DirectoryType](dir)

object Watcher:
  def apply[DirectoryType: GenericWatchService: SpecificDirectory: GenericDirectory]
      (dirs: DirectoryType*)(using Log[Text], Monitor)
          : Watcher[DirectoryType] =

    val svc: jnf.WatchService = summon[GenericWatchService[DirectoryType]]()
    val watcher = Watcher[DirectoryType](svc)
    dirs.each(watcher.add(_))

    watcher

case class Watcher[DirectoryType: GenericDirectory: SpecificDirectory](private val svc: jnf.WatchService)
    (using Monitor):
  
  private val watches: HashMap[jnf.WatchKey, jnf.Path] = HashMap()
  private val dirs: HashMap[jnf.Path, jnf.WatchKey] = HashMap()
  
  private def dirPath(directory: DirectoryType): jnf.Path = jnf.Paths.get(directory.fullPath.s).nn
  private def toDirectory(path: jnf.Path): DirectoryType = SpecificDirectory(path.toString.tt)

  private val funnel = Funnel[Optional[WatchEvent]]
  private val pumpAsync = Async(pump())
  
  def stream: LazyList[WatchEvent] = funnel.stream.takeWhile(_ != Unset).collect:
    case event: WatchEvent => event
  
  def removeAll()(using Log[Text]): Unit = watches.values.map(toDirectory(_)).each(remove(_))

  @tailrec
  private def pump(): Unit =
    svc.take().nn match
      case k: jnf.WatchKey =>
        val key = k.nn
        key.pollEvents().nn.iterator.nn.asScala.flatMap(process(key, _)).each(funnel.put(_))
        key.reset()
    
    pump()

  private def process(key: jnf.WatchKey, event: jnf.WatchEvent[?]): List[WatchEvent] =
    val base = watches(key)
    val eventCtx = event.context.nn

    val absolute = (eventCtx: @unchecked) match
      case path: jnf.Path => base.resolve(path).nn
    
    val relative = (eventCtx: @unchecked) match
      case path: jnf.Path => path.toString.show
    
    try event.kind match
      case ENTRY_CREATE =>
        if absolute.toFile.nn.isDirectory
        then List(WatchEvent.NewDirectory(base.toString.show, relative))
        else List(WatchEvent.NewFile(base.toString.show, relative))
      
      case ENTRY_MODIFY =>
        List(WatchEvent.Modify(base.toString.show, relative))
      
      case ENTRY_DELETE =>
        List(WatchEvent.Delete(base.toString.show, relative))
      
      case _ =>
        Nil
    
    catch case err: Exception => List()

  def add(dir: DirectoryType)(using Log[Text]): Unit = synchronized:
    val path = dirPath(dir)
    val watchKey = path.register(svc, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE).nn
    watches(watchKey) = path
    dirs(path) = watchKey
    Log.info(t"Started watching ${path.toString.show}")
  
  def remove(dir: DirectoryType)(using Log[Text]): Unit = synchronized:
    val path = dirPath(dir)
    val watchKey = dirs(path)
    watchKey.cancel()
    dirs.remove(path)
    watches.remove(watchKey)
    Log.info(t"Stopped watching ${path.toString.show}")
    if dirs.isEmpty then funnel.put(Unset)
  
  def directories: Set[DirectoryType] = dirs.keySet.to(Set).map(toDirectory(_))

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
