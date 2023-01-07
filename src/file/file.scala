/*
    Anticipation, version 0.4.0. Copyright 2021-23 Jon Pretty, Propensive OÜ.

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

import annotation.implicitNotFound

import java.nio.file as jnf
import java.io as ji

@implicitNotFound("""|anticipation: a contextual DirectoryProvider[${T}] instance is required, for example one of:
                     |    import filesystemIntegration.javaNioPath  // represent a directory as a java.nio.Path (requires Dierutic)
                     |    import filesystemIntegration.javaIo       // represent a directory as a java.io.File (requires Dierutic)
                     |    import filesystemIntegration.galileiPath  // represent a directory as a galilei.Directory (requires Galilei)""".stripMargin)
trait GenericDirectoryMaker[+T]:
  def makeDirectory(path: String, readOnly: Boolean = false): Option[T]


trait GenericDirectoryReader[-T]:
  def directoryPath(value: T): String

@implicitNotFound("""|anticipation: a contextual FileProvider[${T}] instance is required, for example one of:
                     |    import filesystemIntegration.javaNioPath  // represent a file as a java.nio.Path (requires Diuretic)
                     |    import filesystemIntegration.javaIo       // represent a file as a java.io.File (requires Diuretic)
                     |    import filesystemIntegration.galileiPath  // represent a file as a galilei.File (requires Galilei)""".stripMargin)
trait GenericFileMaker[+T]:
  def makeFile(path: String, readOnly: Boolean = false): Option[T]

trait GenericFileReader[-T]:
  def filePath(value: T): String

@implicitNotFound("""|anticipation: a contextual PathProvider[${T}] instance is required, for example one of:
                     |    import integration.javaNioPath  // represent a path as a java.nio.Path
                     |    import integration.javaIo       // represent a path as a java.io.File
                     |    import integration.galileiPath  // represent a path as a galilei.Path (requires Galilei)""".stripMargin)
trait GenericPathMaker[+T]:
  def makePath(path: String, readOnly: Boolean = false): Option[T]

trait GenericPathReader[-T]:
  def getPath(value: T): String

@implicitNotFound("""|anticipation: a contextual WatchService[${T}] instance is required.""".stripMargin)
trait GenericWatchService[+T]:
  def apply(): jnf.WatchService

package filesystemIntegration {}