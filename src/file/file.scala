/*
    Anticipation, version 0.4.0. Copyright 2021-22 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package anticipation

import java.nio.file as jnf
import java.io as ji

trait DirectoryProvider[+T]:
  def make(path: String, readOnly: Boolean = false): Option[T]

trait DirectoryInterpreter[-T]:
  def path(value: T): String

trait FileProvider[+T]:
  def make(path: String, readOnly: Boolean = false): Option[T]

trait FileInterpreter[-T]:
  def path(value: T): String

trait PathProvider[+T]:
  def make(path: String, readOnly: Boolean = false): Option[T]

trait PathInterpreter[-T]:
  def path(value: T): String

trait WatchService[+T]:
  def apply(): jnf.WatchService

package integration:
  given javaNio: FileProvider[jnf.Path] with DirectoryProvider[jnf.Path]
      with DirectoryInterpreter[jnf.Path] with FileInterpreter[jnf.Path]
      with PathProvider[jnf.Path] with PathInterpreter[jnf.Path] with
    def make(path: String, readOnly: Boolean = false): Option[jnf.Path] =
      try Some(jnf.Paths.get(path).nn)
      catch case err: jnf.InvalidPathException => None
    def path(value: jnf.Path): String = value.toAbsolutePath.nn.toString

  given javaIo: FileProvider[ji.File] with DirectoryProvider[ji.File]
      with DirectoryInterpreter[ji.File] with FileInterpreter[ji.File]
      with PathProvider[ji.File] with PathInterpreter[ji.File] with
    def make(path: String, readOnly: Boolean = false): Option[ji.File] = Some(ji.File(path))
    def path(value: ji.File): String = value.getAbsolutePath.nn