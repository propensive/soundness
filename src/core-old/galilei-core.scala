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
import fulminate.*
import guillotine.*
import rudiments.*
import serpentine.*
import vacuous.*

import scala.compiletime.*
import scala.jdk.StreamConverters.*

import java.io as ji
import java.nio as jn
import java.nio.file as jnf

import language.experimental.pureFunctions

package filesystemOptions:
  given dereferenceSymlinks: DereferenceSymlinks = new DereferenceSymlinks:
    def options(): List[jnf.LinkOption] = Nil

  given doNotDereferenceSymlinks: DereferenceSymlinks = new DereferenceSymlinks:
    def options(): List[jnf.LinkOption] = List(jnf.LinkOption.NOFOLLOW_LINKS)

  given moveAtomically: MoveAtomically with
    def options(): List[jnf.CopyOption] = List(jnf.StandardCopyOption.ATOMIC_MOVE)

  given doNotMoveAtomically: MoveAtomically with
    def options(): List[jnf.CopyOption] = Nil

  given copyAttributes: CopyAttributes with
    def options(): List[jnf.CopyOption] = List(jnf.StandardCopyOption.COPY_ATTRIBUTES)

  given doNotCopyAttributes: CopyAttributes with
    def options(): List[jnf.CopyOption] = Nil

  given deleteRecursively(using io: Tactic[IoError])
          : DeleteRecursively =

    new DeleteRecursively:
      def conditionally[ResultType](path: Path)(operation: => ResultType): ResultType =
        given symlinks: DereferenceSymlinks = doNotDereferenceSymlinks
        given creation: CreateNonexistent = doNotCreateNonexistent

        if path.exists() then
          if path.is[Directory] then path.as[Directory].children.each(conditionally(_)(()))
          jnf.Files.delete(path.stdlib)

        operation

  given doNotDeleteRecursively(using unemptyDirectory: Tactic[UnemptyDirectoryError])
          : DeleteRecursively =
    new DeleteRecursively:
      def conditionally[ResultType](path: Path)(operation: => ResultType): ResultType =
        try operation
        catch case error: jnf.DirectoryNotEmptyException => abort(UnemptyDirectoryError(path))

  given overwritePreexisting(using deleteRecursively: DeleteRecursively): OverwritePreexisting =
    new OverwritePreexisting:
      def apply[ResultType](path: Path)(operation: => ResultType): ResultType =
        deleteRecursively.conditionally(path)(operation)

  given doNotOverwritePreexisting(using overwrite: Tactic[OverwriteError]): OverwritePreexisting =
    new OverwritePreexisting:
      def apply[ResultType](path: Path)(operation: => ResultType): ResultType =
        try operation catch case error: jnf.FileAlreadyExistsException => abort(OverwriteError(path))

  given createNonexistentParents(using Tactic[IoError]): CreateNonexistentParents =
    new CreateNonexistentParents:
      def apply[ResultType](path: Path)(operation: => ResultType): ResultType =
        path.parent.let: parent =>
          given DereferenceSymlinks = filesystemOptions.doNotDereferenceSymlinks

          if !parent.exists() || !parent.is[Directory]
          then jnf.Files.createDirectories(parent.stdlib)

        operation

  given doNotCreateNonexistentParents(using io: Tactic[IoError]): CreateNonexistentParents =

    new CreateNonexistentParents:
      def apply[ResultType](path: Path)(operation: => ResultType): ResultType =
        try operation catch case error: ji.FileNotFoundException => abort(IoError(path))

  given createNonexistent(using createNonexistentParents: CreateNonexistentParents)
          : CreateNonexistent =
    new CreateNonexistent:
      def apply(path: Path)(operation: => Unit): Unit =
        if !path.exists() then createNonexistentParents(path)(operation)

  given doNotCreateNonexistent: CreateNonexistent = new CreateNonexistent:
    def apply(path: Path)(operation: => Unit): Unit = ()

  given writeSynchronously: WriteSynchronously with
    def options(): List[jnf.StandardOpenOption] = List(jnf.StandardOpenOption.SYNC)

  given doNotWriteSynchronously: WriteSynchronously with
    def options(): List[jnf.StandardOpenOption] = Nil

given (using log: IoEvent is Loggable) => ExecEvent is Loggable =
  log.contramap(IoEvent.Exec(_))

type GeneralForbidden = Windows.Forbidden | Unix.Forbidden
