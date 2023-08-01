/*
    Anticipation, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import language.experimental.captureChecking

trait GenericDirectoryMaker[+DirectoryType]:
  def makeDirectory(directory: Text, readOnly: Boolean = false): DirectoryType


trait GenericDirectoryReader[-DirectoryType]:
  def directoryPath(directory: DirectoryType): Text

trait GenericFileMaker[+FileType]:
  def makeFile(file: Text, readOnly: Boolean = false): FileType

trait GenericFileReader[-FileType]:
  def filePath(file: FileType): Text

trait GenericPathMaker[+PathType]:
  def makePath(path: Text, readOnly: Boolean = false): PathType

trait GenericPathReader[-PathType]:
  def getPath(path: PathType): Text

trait GenericWatchService[+T]:
  def apply(): jnf.WatchService

trait AbstractPathMaker[+PathType]:
  def makePath(path: Text): PathType

trait AbstractPathReader[-PathType]:
  def getPath(path: PathType): Text

def makeGenericPath[PathType](path: Text, readOnly: Boolean = false)(using maker: GenericPathMaker[PathType])
                   : PathType =
  maker.makePath(path, readOnly)

def readGenericPath[PathType](path: PathType)(using reader: GenericPathReader[PathType]): Text =
  reader.getPath(path)

def makeGenericFile[FileType](file: Text, readOnly: Boolean = false)(using maker: GenericFileMaker[FileType])
                   : FileType =
  maker.makeFile(file, readOnly)

def readGenericFile[FileType](file: FileType)(using reader: GenericFileReader[FileType]): Text =
  reader.filePath(file)

def makeGenericDirectory[DirectoryType](directory: Text, readOnly: Boolean = false)
                        (using maker: GenericDirectoryMaker[DirectoryType])
                        : DirectoryType =
  maker.makeDirectory(directory, readOnly)

def readGenericDirectory[DirectoryType](directory: DirectoryType)
                        (using reader: GenericDirectoryReader[DirectoryType])
                        : Text =
  reader.directoryPath(directory)

def makeAbstractPath
    [PathType](path: Text)(using maker: AbstractPathMaker[PathType]): PathType =
  maker.makePath(path)

def readAbstractPath
    [PathType](path: PathType)(using reader: AbstractPathReader[PathType]): Text =
  reader.getPath(path)
