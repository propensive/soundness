/*
    Galilei, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package galilei

import anticipation.*
import contingency.*
import rudiments.*
import serpentine.*

import scala.compiletime.*

import java.io as ji
import java.nio as jn
import java.nio.file as jnf
import java.nio.file.attribute as jnfa

import language.experimental.pureFunctions

trait Entry:
  def path: Path
  def fullname: Text = path.fullname
  def stillExists(): Boolean = path.exists()

  def touch()(using Tactic[IoError]): Unit =
    try jnf.Files.setLastModifiedTime(path.stdlib, jnfa.FileTime.fromMillis(System.currentTimeMillis))
    catch case error: ji.IOException => raise(IoError(path))

  def hidden()(using Tactic[IoError]): Boolean =
    try jnf.Files.isHidden(path.stdlib) catch case error: ji.IOException => raise(IoError(path), false)

  object readable:
    def apply(): Boolean = jnf.Files.isReadable(path.stdlib)
    def update(status: Boolean): Unit = path.stdlib.toFile.nn.setReadable(status)

  object writable:
    def apply(): Boolean = jnf.Files.isWritable(path.stdlib)
    def update(status: Boolean): Unit = path.stdlib.toFile.nn.setWritable(status)

  object executable:
    def apply(): Boolean = jnf.Files.isExecutable(path.stdlib)
    def update(status: Boolean): Unit = path.stdlib.toFile.nn.setExecutable(status)

  def hardLinks()(using dereferenceSymlinks: DereferenceSymlinks, io: Tactic[IoError]): Int =
    try jnf.Files.getAttribute(path.stdlib, "unix:nlink", dereferenceSymlinks.options()*) match
      case count: Int => count
      case _          => raise(IoError(path), 1)
    catch case error: IllegalArgumentException => raise(IoError(path), 1)

  def volume: Volume =
    val fileStore = jnf.Files.getFileStore(path.stdlib).nn
    Volume(fileStore.name.nn.tt, fileStore.`type`.nn.tt)

  def delete()(using deleteRecursively: DeleteRecursively, io: Tactic[IoError]): Path =
    try deleteRecursively.conditionally(path)(jnf.Files.delete(path.stdlib)) catch
      case error: jnf.NoSuchFileException        => raise(IoError(path))
      case error: ji.FileNotFoundException       => raise(IoError(path))
      case error: ji.IOException                 => raise(IoError(path))
      case error: SecurityException              => raise(IoError(path))

    path

  def symlinkTo(destination: Path)
      (using overwritePreexisting: OverwritePreexisting, createNonexistentParents: CreateNonexistentParents)
      (using io: Tactic[IoError])
          : Path/*^{io, overwritePreexisting, createNonexistentParents}*/ =

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.createSymbolicLink(destination.stdlib, path.stdlib)

    destination

  def copyInto(destination: Directory)
      (using overwritePreexisting: OverwritePreexisting, dereferenceSymlinks: DereferenceSymlinks)
      (using io: Tactic[IoError])
          : Path/*^{io, overwritePreexisting, dereferenceSymlinks}*/ =

    given CreateNonexistentParents = filesystemOptions.createNonexistentParents
    copyTo(destination / path.descent.head)

  def copyTo(destination: Path)
      (using overwritePreexisting:     OverwritePreexisting,
             dereferenceSymlinks:      DereferenceSymlinks,
             createNonexistentParents: CreateNonexistentParents)
      (using io: Tactic[IoError])
          : Path/*^{io, overwritePreexisting, createNonexistentParents, dereferenceSymlinks}*/ =

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.copy(path.stdlib, destination.stdlib, dereferenceSymlinks.options()*)

    destination

  def moveInto
      (destination: Directory)
      (using overwritePreexisting: OverwritePreexisting,
             moveAtomically:       MoveAtomically,
             dereferenceSymlinks:  DereferenceSymlinks)
      (using io: Tactic[IoError])
          : Path/*^{io, overwritePreexisting, moveAtomically, dereferenceSymlinks}*/ =

    given CreateNonexistentParents = filesystemOptions.createNonexistentParents
    moveTo(destination / path.descent.head)

  def moveTo
      (destination: Path)
      (using overwritePreexisting:     OverwritePreexisting,
             moveAtomically:           MoveAtomically,
             dereferenceSymlinks:      DereferenceSymlinks,
             createNonexistentParents: CreateNonexistentParents)
      (using io: Tactic[IoError])
          : Path/*^{io, overwritePreexisting, createNonexistentParents, moveAtomically, dereferenceSymlinks}*/ =

    val options: Seq[jnf.CopyOption] = dereferenceSymlinks.options() ++ moveAtomically.options()

    createNonexistentParents(destination):
      overwritePreexisting(destination):
        jnf.Files.move(path.stdlib, destination.stdlib, options*)

    destination

  def lastModified[InstantType: SpecificInstant]: InstantType =
    SpecificInstant(jnf.Files.getLastModifiedTime(path.stdlib).nn.toInstant.nn.toEpochMilli)
