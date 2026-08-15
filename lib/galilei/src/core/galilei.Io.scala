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

import scala.annotation.*

import anticipation.*
import fulminate.*
import serpentine.*

// Galilei's I/O vocabulary. `Error`'s reasons are the POSIX errno table — `Nonexistent`
// is `ENOENT`, `NotSameVolume` is `EXDEV`, `Physical` is `EIO` — so the failures a
// filesystem can report are named once, here, and the backends map onto them.
object Io:
  // IoError → Io.Error
  object Error:
    enum Operation:
      case Read, Write, Create, Copy, Move, Delete, Metadata, Open, Access

    enum Reason:
      case
        PermissionDenied, Nonexistent, AlreadyExists, IsNotDirectory, IsDirectory,
        DirectoryNotEmpty, NotSameVolume, Unsupported, Cycle, Busy, ReadOnly, TooManyLinks,
        NameTooLong, QuotaExceeded, StorageFull, InvalidData, Interrupted, Physical

    @targetName("apply2")
    def apply(path: Path, operation: Operation, reason: Reason)
      ( using filesystem: path.Plane is Filesystem, diagnostics: Diagnostics )
    :   Error =

      new Error(path, operation, reason, filesystem)


    given Reason is Communicable =
      case Reason.PermissionDenied  => m"the user did not have sufficient permissions"
      case Reason.Nonexistent       => m"the entry does not exist"
      case Reason.AlreadyExists     => m"an entry at this path already exists"
      case Reason.IsNotDirectory    => m"the entry is not a directory"
      case Reason.IsDirectory       => m"the entry is a directory"
      case Reason.DirectoryNotEmpty => m"the directory is not empty"
      case Reason.NotSameVolume     => m"the source and destination are on different volumes"
      case Reason.Unsupported       => m"it is not supported by the filesystem"
      case Reason.Cycle             => m"a cycle was detected on the filesystem"
      case Reason.Busy              => m"the entry is in use"
      case Reason.ReadOnly          => m"the filesystem is read-only"
      case Reason.TooManyLinks      => m"too many hard links point to the entry"
      case Reason.NameTooLong       => m"the entry's name is too long for the filesystem"
      case Reason.QuotaExceeded     => m"the user's storage quota would be exceeded"
      case Reason.StorageFull       => m"the filesystem has no space remaining"
      case Reason.InvalidData       => m"the entry's data is invalid"
      case Reason.Interrupted       => m"the operation was interrupted"
      case Reason.Physical          => m"of a physical input/output error"

    given Operation is Communicable =
      case Operation.Read     => m"read"
      case Operation.Access   => m"access"
      case Operation.Write    => m"write"
      case Operation.Open     => m"open"
      case Operation.Copy     => m"copy"
      case Operation.Create   => m"create"
      case Operation.Move     => m"move"
      case Operation.Delete   => m"delete"
      case Operation.Metadata => m"metadata"

    private def describe
      ( path:       Path,
        operation:  Operation,
        reason:     Reason,
        filesystem: path.Plane is Filesystem )
      ( using Diagnostics )
    :   Message =

      given path.Plane is Filesystem = filesystem
      m"the $operation operation at ${path.encode} on ${filesystem.name} failed because $reason"

  case class Error
    ( path:       Path,
      operation:  Error.Operation,
      reason:     Error.Reason,
      filesystem: path.Plane is Filesystem )
    ( using Diagnostics )
  // The message is computed in a companion method: a local `given` alias in the super-argument
  // block is a lazy val whose initialization references `this` before the super constructor,
  // which the Scala.js linker rejects (the JVM tolerates it).
  extends fulminate.Error(Error.describe(path, operation, reason, filesystem))

  // IoEvent → Io.Event
  object Event:
    given communicable: Event is Communicable =
      case Create(path)          => m"created $path"
      case Delete(path)          => m"deleted $path"
      case Move(from, to)        => m"moved $from to $to"
      case Copy(from, to)        => m"copied $from to $to"
      case HardLink(from, to)    => m"hard-linked $from to $to"
      case Symlink(link, target) => m"symlinked $link to $target"
      case Touch(path)           => m"touched $path"

  enum Event:
    case Create(path: Text) extends Event, Log.Filesystem
    case Delete(path: Text) extends Event, Log.Filesystem
    case Move(from: Text, to: Text) extends Event, Log.Filesystem
    case Copy(from: Text, to: Text) extends Event, Log.Filesystem
    case HardLink(from: Text, to: Text) extends Event, Log.Filesystem
    case Symlink(link: Text, target: Text) extends Event, Log.Filesystem
    case Touch(path: Text) extends Event, Log.Filesystem
