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

import contingency.*
import rudiments.*

import scala.jdk.StreamConverters.*

import java.nio as jn
import java.nio.file as jnf

import language.experimental.pureFunctions

object PathResolver:
  // given entry
  //     (using dereferenceSymlinks: DereferenceSymlinks, io: Tactic[IoError])
  //     : PathResolver[Entry, Path] =
  //   new PathResolver[Entry, Path]:
  //     def apply(path: Path): Entry =
  //       if path.exists() then path.entryType() match
  //         case PathStatus.Directory => Directory(path)
  //         case _                    => File(path)
  //       else raise(IoError(path))(Directory(path))

  given file
      (using createNonexistent:   CreateNonexistent,
             dereferenceSymlinks: DereferenceSymlinks,
             io:                  Tactic[IoError])
          : PathResolver[File, Path] = path =>

    if path.exists() && path.entryType() == PathStatus.File then File(path)
    else createNonexistent(path)(jnf.Files.createFile(path.stdlib))

    File(path)

  given directory
      (using createNonexistent: CreateNonexistent,
             dereferenceSymlinks: DereferenceSymlinks,
             io: Tactic[IoError])
          : PathResolver[Directory, Path] = path =>
    if path.exists() && path.entryType() == PathStatus.Directory then Directory(path)
    else createNonexistent(path):
      jnf.Files.createDirectory(path.stdlib)

    Directory(path)

@capability
trait PathResolver[+EntryType <: Entry, -PathType <: Path]:
  def apply(value: PathType): EntryType
