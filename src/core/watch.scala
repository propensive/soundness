/*
    Surveillance, version 0.4.0. Copyright 2022-23 Jon Pretty, Propensive OÜ.

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
import parasitism.*
import serpentine.*
import anticipation.*

import scala.collection.mutable.HashMap

import java.nio.file as jnf
import jnf.StandardWatchEventKinds.*

case class InotifyError()
extends Error(err"the limit on the number of paths that can be watched has been exceeded")

extension [Dir: GenericDirectoryMaker: GenericDirectoryReader](dirs: Seq[Dir])(using Monitor)
  def watch()(using Log, GenericWatchService[Dir]): Watcher[Dir] throws InotifyError = Watcher[Dir](dirs*)

extension [Dir: GenericDirectoryMaker: GenericDirectoryReader](dir: Dir)(using Monitor)
  def watch()(using Log, GenericWatchService[Dir]): Watcher[Dir] throws InotifyError = Watcher[Dir](dir)

object Watcher:
  def apply[Dir: GenericWatchService: GenericDirectoryMaker: GenericDirectoryReader]
           (dirs: Dir*)(using Log, Monitor)
           : Watcher[Dir] =
    val svc: jnf.WatchService = summon[GenericWatchService[Dir]]()
    val watcher = Watcher[Dir](svc)
    dirs.foreach(watcher.add(_))

    watcher

case class Watcher[Dir](private val svc: jnf.WatchService)
                  (using fromDir: GenericDirectoryReader[Dir], mkdir: GenericDirectoryMaker[Dir],
                       monitor: Monitor):
  
  private val watches: HashMap[jnf.WatchKey, jnf.Path] = HashMap()
  private val dirs: HashMap[jnf.Path, jnf.WatchKey] = HashMap()
  
  private def dirPath(dir: Dir): jnf.Path = jnf.Paths.get(fromDir.directoryPath(dir).show.s).nn
  private def toDir(path: jnf.Path): Dir = mkdir.makeDirectory(Showable(path).show.s).get

  private val funnel = Funnel[Maybe[WatchEvent]]
  private val pumpTask = Task(t"watcher")(pump())
  
  def stream: LazyList[WatchEvent] throws AlreadyStreamingError =
    funnel.stream.takeWhile(_ != Unset).sift[WatchEvent]
  
  def removeAll()(using Log): Unit = watches.values.map(toDir(_)).foreach(remove(_))

  @tailrec
  private def pump(): Unit =
    svc.take().nn match
      case k: jnf.WatchKey =>
        val key = k.nn
        key.pollEvents().nn.iterator.nn.asScala.flatMap(process(key, _)).foreach(funnel.put(_))
        key.reset()
    
    pump()

  private def process(key: jnf.WatchKey, event: jnf.WatchEvent[?]): List[WatchEvent] =
    val base = watches(key)
    val eventCtx = event.context.nn

    val absolute = eventCtx match
      case path: jnf.Path => base.resolve(path).nn
      case _              => throw Mistake("Should never match")
    
    val relative = eventCtx match
      case path: jnf.Path => Relative.parse(Showable(path).show)
      case _              => throw Mistake("Should never match")
    
    try event.kind match
      case ENTRY_CREATE =>
        if absolute.toFile.nn.isDirectory
        then List(WatchEvent.NewDirectory(Showable(base).show, relative))
        else List(WatchEvent.NewFile(Showable(base).show, relative))
      
      case ENTRY_MODIFY =>
        List(WatchEvent.Modify(Showable(base).show, relative))
      
      case ENTRY_DELETE =>
        List(WatchEvent.Delete(Showable(base).show, relative))
      
      case _ =>
        Nil
    
    catch case err: Exception => List()

  def add(dir: Dir)(using Log): Unit = synchronized:
    val path = dirPath(dir)
    val watchKey = path.register(svc, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE).nn
    watches(watchKey) = path
    dirs(path) = watchKey
    Log.info(t"Started watching ${Showable(path).show}")
  
  def remove(dir: Dir)(using Log): Unit = synchronized:
    val path = dirPath(dir)
    val watchKey = dirs(path)
    watchKey.cancel()
    dirs.remove(path)
    watches.remove(watchKey)
    Log.info(t"Stopped watching ${Showable(path).show}")
    if dirs.isEmpty then funnel.put(Unset)
  
  def directories: Set[Dir] = dirs.keySet.to(Set).map(toDir(_))

enum WatchEvent:
  case NewFile(dir: Text, file: Relative)
  case NewDirectory(dir: Text, directory: Relative)
  case Modify(dir: Text, file: Relative)
  case Delete(dir: Text, file: Relative)

  def dir: Text

  def path[Dir](using mkdir: GenericPathMaker[Dir]): Dir = unsafely:
    val relPath = this match
      case NewFile(_, file)      => file
      case NewDirectory(_, path) => path
      case Modify(_, file)       => file
      case Delete(_, path)       => path

      mkdir.makePath(Showable(jnf.Paths.get(dir.s, relPath.show.s).nn.normalize.nn).show.s).get

export WatchEvent.{NewFile, NewDirectory, Modify, Delete}

given Realm(t"surveillance")
