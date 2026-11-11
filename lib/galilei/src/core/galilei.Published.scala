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
package galilei

import java.util.concurrent.atomic as juca

import scala.caps

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*
import zephyrine.Mapped
import Io.Error.{Operation, Reason}

// Sharing a file between processes through the page cache (issue #2472): one process *owns* a
// name and publishes successive versions of a file under it; any number of others map whichever
// version is current, and none of them ever waits for another.
//
// Two rules make that safe without anything arbitrating between the processes. A published file
// is never changed: each version is written to a temporary sibling, `.<name>.part`, and moved
// over the name atomically, so a reader which has mapped the previous version goes on seeing it,
// whole and unchanged, until it chooses to look again. And a name has one owner: the owning
// process holds an exclusive operating-system lock on `<name>.lock` for as long as it owns the
// name, which a second would-be owner cannot take, and which is released when the process ends,
// however it ends.
//
// The lock is advisory, and belongs to a process on one host: it is dependable between
// cooperating processes on a local filesystem, and not over NFS. A reader keeps the version it
// has mapped alive -- the file's storage is reclaimed only when its last mapping goes -- so a
// long-lived reader should `refresh` whenever it learns of a new edition.

// The identity of one published version: which file the name led to, and when it was written.
// Two editions are the same version exactly when they are equal; they carry no order.
case class Edition(identity: Stat.Identity, modified: Long) derives CanEqual

// Ownership of a name, for the scope of `path.own`: the capability to publish versions of
// `record` under it. Confined to its scope, at whose end the name is released.
trait Owner[record] extends caps.ExclusiveCapability:
  // Replaces the published file with the encoding of `value`, atomically, and answers the new
  // version's edition. Readers which have mapped an earlier version are undisturbed.
  def publish(value: record)(using (record is Encodable in Data)^, Tactic[Io.Error]): Edition

// A reader's hold on a published name, for the scope of `path.published`: the current version,
// mapped into memory, and the means to move on to a newer one. It takes no lock, and may be
// shared between threads.
trait Published[record] extends caps.SharedCapability:
  // The edition of the version which is mapped.
  def edition: Edition

  // Lends a view of the mapped version for the length of `lambda`. A `refresh` meanwhile, on
  // this or another thread, does not disturb it: the version stays mapped until the last such
  // scope over it has ended.
  def map[result](lambda: Mapped^ => result): result

  // Maps the version the name now leads to, if it is not the one already mapped, and answers
  // whether it did.
  def refresh()(using Tactic[Io.Error]): Boolean

object Published:
  // One mapped version, counted: `readers` is the number of `map` scopes now over it, and it is
  // unmapped when it has been `retired` -- superseded, or the reader's scope ended -- and that
  // number has fallen to zero. Both fields are guarded by the owning `Reader`'s monitor.
  private final class Version(val mapping: FilesystemBackend.Mapping, val edition: Edition):
    val readers: juca.AtomicInteger = juca.AtomicInteger(0)
    val retired: juca.AtomicBoolean = juca.AtomicBoolean(false)

  private[galilei] def editionOf[plane: Filesystem]
    ( path: Path on plane, backend: FilesystemBackend on plane )
    ( using Tactic[Io.Error] )
  :   Edition =

    // The metadata first: it is what fails, informatively, when nothing is published.
    val modified = backend.stat(path, true).modified

    // A platform which cannot tell one file from another cannot tell one version from another.
    val identity = backend.identity(path, true).or:
      abort(Io.Error(path, Operation.Metadata, Reason.Unsupported))

    Edition(identity, modified)

  private[galilei] final class Reader[record, plane: Filesystem]
    ( path: Path on plane, backend: FilesystemBackend on plane )
  extends Published[record]:

    // [synchronized] the mapped version, replaced under this reader's monitor
    @caps.unsafe.untrackedCaptures
    private var current: Version | Null = null

    // Maps whatever the name leads to, with its edition. The name can be moved onto between
    // the two being read, so the edition is taken on either side of the mapping, and the
    // mapping kept only if they agree.
    private def acquire()(using Tactic[Io.Error]): Version =
      val before = editionOf(path, backend)

      // [synchronized] the mapping is held in this reader's field, across threads, and its
      // lifetime is managed by hand: it is lent only within `map`, and unmapped by `release`.
      val mapping: FilesystemBackend.Mapping = caps.unsafe.unsafeAssumePure(backend.mapping(path))

      val after =
        try editionOf(path, backend) catch case throwable: Throwable =>
          mapping.unmap()
          throw throwable

      if before == after then Version(mapping, after) else
        mapping.unmap()
        acquire()

    private def release(version: Version): Unit =
      if version.retired.get && version.readers.get == 0 then version.mapping.unmap()

    private[galilei] def open()(using Tactic[Io.Error]): Unit =
      val version = acquire()
      synchronized { current = version }

    private[galilei] def close(): Unit = synchronized:
      val version = current

      if version != null then
        version.retired.set(true)
        release(version)
        current = null

    def edition: Edition = synchronized(current.nn.edition)

    def map[result](lambda: Mapped^ => result): result =
      val version: Version = synchronized:
        val held = current.nn
        held.readers.incrementAndGet()
        held

      try lambda(version.mapping) finally synchronized:
        version.readers.decrementAndGet()
        release(version)

    def refresh()(using Tactic[Io.Error]): Boolean =
      if editionOf(path, backend) == edition then false else
        val version = acquire()

        synchronized:
          val previous = current

          if previous != null && previous.edition == version.edition then
            // Another thread refreshed first.
            version.mapping.unmap()
            false
          else
            if previous != null then
              previous.retired.set(true)
              release(previous)

            current = version
            true

  private[galilei] final class Publisher[record, plane: Filesystem]
    ( path: Path on plane, part: Path on plane, backend: FilesystemBackend on plane, umask: Umask )
  extends Owner[record]:

    def publish(value: record)
      ( using encodable: (record is Encodable in Data)^, io: Tactic[Io.Error] )
    :   Edition =

      val data: Data = encodable.encoded(value)

      // `Sync`, so that the version's bytes are on the device before its name leads to them.
      val flags = List(OpenFlag.Write, OpenFlag.Create, OpenFlag.Truncate, OpenFlag.Sync)

      try
        mitigate:
          case error: Truncation.Error =>
            given Diagnostics = error.diagnostics
            Io.Error(path, Operation.Write, Reason.Physical)

        . protect:
            backend.open(part, flags, umask.mode(Umask.fileBits)): handle =>
              handle.writer(Chain(data))

        backend.move(part, path, true, false)
      catch case throwable: Throwable =>
        safely(backend.deleteIfExists(part))
        throw throwable

      editionOf(path, backend)
