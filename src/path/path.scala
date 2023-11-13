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

import annotation.*

import java.nio.file as jnf

import language.experimental.captureChecking

@capability
trait SpecificDirectory[+DirectoryType]:
  def directory(directory: Text): DirectoryType

@capability
trait SpecificPath[+PathType]:
  def path(path: Text): PathType

@capability
trait SpecificFile[+FileType]:
  def file(path: Text): FileType

@capability
trait GenericPath[-PathType]:
  def pathText(path: PathType): Text

@capability
trait GenericDirectory[-DirectoryType]:
  def directoryText(path: DirectoryType): Text

@capability
trait GenericFile[-FileType]:
  def fileText(path: FileType): Text

@capability
trait GenericWatchService[+T]:
  def apply(): jnf.WatchService

extension [PathType](path: PathType)
  inline def fullPath: Text = compiletime.summonFrom:
    case generic: GenericPath[PathType]      => generic.pathText(path)
    case generic: GenericFile[PathType]      => generic.fileText(path)
    case generic: GenericDirectory[PathType] => generic.directoryText(path)

extension [PathType](path: PathType)(using generic: GenericPath[PathType])
  def pathText: Text = generic.pathText(path)

extension [FileType](file: FileType)(using generic: GenericFile[FileType])
  def fileText: Text = generic.fileText(file)

extension [DirectoryType](directory: DirectoryType)(using generic: GenericDirectory[DirectoryType])
  def directoryText: Text = generic.directoryText(directory)

object SpecificPath:
  def apply[PathType](name: Text)(using specific: SpecificPath[PathType]): PathType =
    specific.path(name)

object SpecificFile:
  def apply[FileType](name: Text)(using specific: SpecificFile[FileType]): FileType =
    specific.file(name)

object SpecificDirectory:
  def apply[DirectoryType](name: Text)(using specific: SpecificDirectory[DirectoryType]): DirectoryType =
    specific.directory(name)
