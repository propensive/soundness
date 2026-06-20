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

import java.nio.file as jnf

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import denominative.*
import nomenclature.n
import parasite.*, threading.platformThreading, Async.nominative
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*

// The polling backend detects changes by snapshotting each watched directory's entries (their
// kind, modification time and size) on a fixed interval and diffing successive snapshots. It needs
// no operating-system filewatching support, at the cost of latency bounded by the interval and the
// inability to observe changes that leave size and modification time unchanged. A single background
// task per registration does the scanning; cancelling the registration interrupts it.
class PollingWatcher[duration: Abstractable across Durations to Long](interval: duration)
extends Watcher:

  import probates.awaitProbate

  private case class Entry(directory: Boolean, modified: Long, size: Long)

  private def scan(directory: jnf.Path, filter: Text => Boolean): Map[Text, Entry] =
    Optional(directory.toFile.nn.listFiles()).let: files =>
      scala.collection.immutable.ArraySeq.unsafeWrapArray(files).flatMap: file =>
        val entry = file.nn
        val name = entry.getName.nn.tt

        if filter(name)
        then List(name -> Entry(entry.isDirectory, entry.lastModified, entry.length))
        else Nil

      . to(Map)

    . or(Map())

  private def diff
    ( directory: jnf.Path,
      previous:  Map[Text, Entry],
      current:   Map[Text, Entry],
      spool:     Spool[WatchEvent] )
  :   Unit =

    val base = directory.toString.show

    current.each: (name, entry) =>
      previous.at(name) match
        case last: Entry =>
          if !entry.directory && last != entry then spool.put(WatchEvent.Modify(base, name))

        case _ =>
          if entry.directory then spool.put(WatchEvent.NewDirectory(base, name))
          else spool.put(WatchEvent.NewFile(base, name))

    previous.each: (name, _) =>
      if !current.contains(name) then spool.put(WatchEvent.Delete(base, name))

  def watch(directories: Map[jnf.Path, Text => Boolean], spool: Spool[WatchEvent])
  :   Watcher.Registration raises WatchError =

    directories.each: (directory, _) =>
      val file = directory.toFile.nn

      if !file.exists then abort(WatchError(WatchError.Reason.Nonexistent))
      else if !file.isDirectory then abort(WatchError(WatchError.Reason.NotDirectory))

    val snapshots: scm.HashMap[jnf.Path, Map[Text, Entry]] = scm.HashMap()

    directories.each: (directory, filter) =>
      snapshots(directory) = scan(directory, filter)

    val async: Optional[Task[Unit]] = safely:
      supervise:
        task(n"surveillance-poll"):
          try
            while true do
              snooze(interval)

              directories.each: (directory, filter) =>
                val current = scan(directory, filter)
                diff(directory, snapshots.at(directory).or(Map()), current, spool)
                snapshots(directory) = current

          catch case _: InterruptedException => ()

    Registration(async)

  private class Registration(async: Optional[Task[Unit]]) extends Watcher.Registration:
    def cancel(): Unit = async.let(_.cancel())
