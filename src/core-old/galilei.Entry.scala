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

