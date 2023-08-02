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
  def makeDirectory(directory: Text): DirectoryType

trait GenericPathMaker[+PathType]:
  def makePath(path: Text): PathType

trait GenericFileMaker[+FileType]:
  def makeFile(path: Text): FileType

trait GenericPathReader[-PathType]:
  def fromPath(path: PathType): Text

trait GenericDirectoryReader[-DirectoryType]:
  def fromDirectory(path: DirectoryType): Text

trait GenericFileReader[-FileType]:
  def fromFile(path: FileType): Text

trait GenericWatchService[+T]:
  def apply(): jnf.WatchService

extension [PathType](path: PathType)
  inline def fullPath: Text = compiletime.summonFrom:
    case reader: GenericPathReader[PathType]      => reader.fromPath(path)
    case reader: GenericFileReader[PathType]      => reader.fromFile(path)
    case reader: GenericDirectoryReader[PathType] => reader.fromDirectory(path)

object GenericPath:
  def apply[PathType](name: Text)(using maker: GenericPathMaker[PathType]): PathType =
    maker.makePath(name)

object GenericFile:
  def apply[FileType](name: Text)(using maker: GenericFileMaker[FileType]): FileType =
    maker.makeFile(name)

object GenericDirectory:
  def apply[DirectoryType](name: Text)(using maker: GenericDirectoryMaker[DirectoryType]): DirectoryType =
    maker.makeDirectory(name)
