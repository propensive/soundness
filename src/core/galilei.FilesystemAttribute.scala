/*
    Galilei, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import java.nio.file as jnf

import prepositional.*
import serpentine.*

object FilesystemAttribute:
  class Readable[PlatformType <: Filesystem](path: Path on PlatformType):
    def apply(): Boolean = jnf.Files.isReadable(path.javaPath)
    def update(value: Boolean): Unit = path.javaFile.setReadable(value)
  
  class Writable[PlatformType <: Filesystem](path: Path on PlatformType):
    def apply(): Boolean = jnf.Files.isWritable(path.javaPath)
    def update(value: Boolean): Unit = path.javaFile.setWritable(value)
  
  class Executable[PlatformType <: Posix](path: Path on PlatformType):
    def apply(): Boolean = jnf.Files.isExecutable(path.javaPath)
    def update(value: Boolean): Unit = path.javaFile.setExecutable(value)
