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

import java.nio.file as jnf, jnf.StandardWatchEventKinds.*

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import denominative.*
import parasite.*, threading.platform
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*

object Watch:
  private case class WatchService(watchService: jnf.WatchService, pollLoop: Loop):
    import codicils.await

    def stop(): Unit = pollLoop.stop()

    val async: Optional[Task[Unit]] = safely(supervise(task("surveillance".tt)(pollLoop.run())))

  private val serviceMutex: Mutex = Mutex()
  private val watchesMutex: Mutex = Mutex()
  private var serviceValue: Optional[WatchService] = Unset

  private def service: WatchService = serviceValue.or:
    serviceMutex:
      jnf.FileSystems.getDefault.nn.newWatchService().nn.pipe: watchService =>
        WatchService(watchService, pollLoop(watchService)).tap: service =>
          serviceValue = service

  private val watches: scm.HashMap[jnf.WatchKey, Set[Watch#PathWatch]] = scm.HashMap()

  private def register(paths: Map[jnf.Path, Text => Boolean]): Watch =
    new Watch().tap(_.watch(paths))

  private def pollLoop(service: jnf.WatchService): Loop = loop:
    service.take().nn match
      case key: jnf.WatchKey =>
        key.pollEvents().nn.iterator.nn.asScala.each: event =>
          watchesMutex(watches(key)).each(_.put(event))

        key.reset()

  def apply[path: Abstractable across Paths to Text](paths: List[path]): Watch =
    Watch.register:
      val pathGroups: Map[jnf.Path, List[Text => Boolean]] =
        paths.map(_.generic.s).map(jnf.Paths.get(_).nn).map: javaPath =>
          if javaPath.toFile.nn.isDirectory then (javaPath, (_: Text) => true)
          else (javaPath.getParent.nn, (_: Text) == javaPath.getFileName.nn.toString.tt)

        . groupBy(_(0)).mapValues(_.map(_(1)))

      pathGroups.mapValues: predicates =>
        (value: Text) => predicates.exists(_(value))

class Watch():
  private val mutex: Mutex = Mutex()
  private val spool: Spool[WatchEvent] = Spool()
  private val watches: scm.HashSet[PathWatch] = scm.HashSet[PathWatch]()


  private class PathWatch
    ( private[Watch]  val key:    jnf.WatchKey,
      private[Watch]  val base:   jnf.Path,
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


  def stream: Stream[WatchEvent] = spool.stream

  def watch(paths: Map[jnf.Path, Text => Boolean]): Unit =
    val watches2 = paths.scala.map:
      case (path, filter) =>
        val key =
          path.register(Watch.service.watchService, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE).nn

        new PathWatch(key, path, spool, filter).tap: watch =>
          Watch.watchesMutex:
            Watch.watches(key) = Watch.watches.at(key).or(Set()) ++ Set(watch)

    mutex(watches ++= watches2)

  def unregister(): Unit =
    Watch.watchesMutex:
      List.from(watches).each: watch =>
        Watch.watches(watch.key) = Watch.watches.at(watch.key).or(Set()) -- Set(watch)

        if Watch.watches(watch.key).nil then
          watch.key.cancel()
          Watch.watches.remove(watch.key)

        if Watch.watches.nil then mutex:
          Watch.serviceValue.let: service =>
            service.stop()
            Watch.serviceValue = Unset
