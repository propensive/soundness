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
  object dereferenceSymlinks:
    given DereferenceSymlinks as enabled = () => Nil
    given DereferenceSymlinks as disabled = () => List(jnf.LinkOption.NOFOLLOW_LINKS)

  object moveAtomically:
    given MoveAtomically as enabled = () => List(jnf.StandardCopyOption.ATOMIC_MOVE)
    given MoveAtomically as disabled = () => Nil

  object copyAttributes:
    given CopyAttributes as enabled = () => List(jnf.StandardCopyOption.COPY_ATTRIBUTES)
    given CopyAttributes as disabled = () => Nil

  object deleteRecursively
    given (using Tactic[IoError]) => DeleteRecursively as enabled:
      def conditionally[ResultType](path: Path)(operation: => ResultType): ResultType =
        given symlinks: DereferenceSymlinks = doNotDereferenceSymlinks
        given creation: CreateNonexistent = doNotCreateNonexistent

        if path.exists() then
          if path.entry == Directory then path.children.each(conditionally(_)(()))
          jnf.Files.delete(path.stdlib)

        operation

    given (using Tactic[IoError]) => DeleteRecursively as disabled:
      def conditionally[ResultType](path: Path)(operation: => ResultType): ResultType =
        try operation catch case error: jnf.DirectoryNotEmptyException =>
          raise(IoError(path, IoError.Operation.Delete, IoError.Reason.NotEmpty))
  
  object overwritePreexisting:
    given (using DeleteRecursively) => OverwritePreexisting as enabled:
      def apply[ResultType](path: Path)(operation: => ResultType): ResultType =
        deleteRecursively.conditionally(path)(operation)

    given (using Tactic[IoError]) => OverwritePreexisting as disabled:
      def apply[ResultType](path: Path)(operation: => ResultType): ResultType =
        try operation catch case error: jnf.FileAlreadyExistsException =>
          raise(IoError(path, IoError.Operation.Write, IoError.Reason.AlreadyExists))

  object createNonexistentParents:
    given (using Tactic[IoError]) => CreateNonexistentParents as enabled:
      def apply[ResultType](path: Path)(operation: => ResultType): ResultType =
        path.parent.let: parent =>
          given DereferenceSymlinks = filesystemOptions.doNotDereferenceSymlinks

          if !parent.exists() || parent.entry != Directory
          then jnf.Files.createDirectories(parent.stdlib)

        operation

    given (using Tactic[IoError]) => CreateNonexistentParents as disabled:
      def apply[ResultType](path: Path)(operation: => ResultType): ResultType =
        try operation catch case error: ji.FileNotFoundException =>
          abort(IoError(path), IoError.Operation.Write, IoError.Reason.Nonexistent)

  object createNonexistent:
    given (using CreateNonexistentParents) => CreateNonexistent as enabled:
      def apply(path: Path)(operation: => Unit): Unit =
        if !path.exists() then createNonexistentParents(path)(operation)

    given CreateNonexistent as disabled:
      def apply(path: Path)(operation: => Unit): Unit = ()

  object writeSynchronously:
    given WriteSynchronously as enabled = () => List(jnf.StandardOpenOption.SYNC)
    given WriteSynchronously as disabled = () => Nil
