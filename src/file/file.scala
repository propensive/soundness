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

@implicitNotFound("""|anticipation: a contextual DirectoryProvider[${T}] instance is required, for example one of:
                     |    import filesystemIntegration.javaNioPath  // represent a directory as a java.nio.Path (requires Dierutic)
                     |    import filesystemIntegration.javaIo       // represent a directory as a java.io.File (requires Dierutic)
                     |    import filesystemIntegration.galileiPath  // represent a directory as a galilei.Directory (requires Galilei)""".stripMargin)
trait GenericDirectoryMaker[+DirectoryType]:
  def makeDirectory(directory: String, readOnly: Boolean = false): Option[DirectoryType]


trait GenericDirectoryReader[-DirectoryType]:
  def directoryPath(directory: DirectoryType): String

@implicitNotFound("""|anticipation: a contextual FileProvider[${T}] instance is required, for example one of:
                     |    import filesystemIntegration.javaNioPath  // represent a file as a java.nio.Path (requires Diuretic)
                     |    import filesystemIntegration.javaIo       // represent a file as a java.io.File (requires Diuretic)
                     |    import filesystemIntegration.galileiPath  // represent a file as a galilei.File (requires Galilei)""".stripMargin)
trait GenericFileMaker[+FileType]:
  def makeFile(file: String, readOnly: Boolean = false): Option[FileType]

trait GenericFileReader[-FileType]:
  def filePath(file: FileType): String

@implicitNotFound("""|anticipation: a contextual PathProvider[${T}] instance is required, for example one of:
                     |    import integration.javaNioPath  // represent a path as a java.nio.Path
                     |    import integration.javaIo       // represent a path as a java.io.File
                     |    import integration.galileiPath  // represent a path as a galilei.Path (requires Galilei)""".stripMargin)
trait GenericPathMaker[+PathType]:
  def makePath(path: String, readOnly: Boolean = false): Option[PathType]

trait GenericPathReader[-PathType]:
  def getPath(path: PathType): String

@implicitNotFound("""|anticipation: a contextual WatchService[${T}] instance is required.""".stripMargin)
trait GenericWatchService[+T]:
  def apply(): jnf.WatchService

def makeGenericPath[PathType](path: String, readOnly: Boolean = false)(using maker: GenericPathMaker[PathType])
                   : Option[PathType] =
  maker.makePath(path, readOnly)

def readGenericPath[PathType](path: PathType)(using reader: GenericPathReader[PathType]): String =
  reader.getPath(path)

def makeGenericFile[FileType](file: String, readOnly: Boolean = false)(using maker: GenericFileMaker[FileType])
                   : Option[FileType] =
  maker.makeFile(file, readOnly)

def readGenericFile[FileType](file: FileType)(using reader: GenericFileReader[FileType]): String =
  reader.filePath(file)

def makeGenericDirectory[DirectoryType](directory: String, readOnly: Boolean = false)
                        (using maker: GenericDirectoryMaker[DirectoryType])
                        : Option[DirectoryType] =
  maker.makeDirectory(directory, readOnly)

def readGenericDirectory[DirectoryType](directory: DirectoryType)
                        (using reader: GenericDirectoryReader[DirectoryType])
                        : String =
  reader.directoryPath(directory)