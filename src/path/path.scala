/*
    Anticipation, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
trait SpecificDirectory:
  type Self
  def directory(directory: Text): Self

@capability
trait SpecificPath:
  type Self
  def path(path: Text): Self

@capability
trait SpecificFile:
  type Self
  def file(path: Text): Self

@capability
trait GenericPath:
  type Self
  def pathText(path: Self): Text

@capability
trait GenericDirectory:
  type Self
  def directoryText(path: Self): Text

@capability
trait GenericFile:
  type Self
  def fileText(path: Self): Text

@capability
trait GenericWatchService:
  def apply(): jnf.WatchService

extension [PathType](path: PathType)
  inline def fullPath: Text = compiletime.summonFrom:
    case generic: (GenericPath { type Self = PathType })      => generic.pathText(path)
    case generic: (GenericFile { type Self = PathType })      => generic.fileText(path)
    case generic: (GenericDirectory { type Self = PathType }) => generic.directoryText(path)

extension [PathType](path: PathType)(using generic: GenericPath { type Self = PathType })
  def pathText: Text = generic.pathText(path)

extension [FileType](file: FileType)(using generic: GenericFile { type Self = FileType })
  def fileText: Text = generic.fileText(file)

extension [DirectoryType](directory: DirectoryType)(using generic: GenericDirectory { type Self = DirectoryType })
  def directoryText: Text = generic.directoryText(directory)

object SpecificPath:
  def apply[PathType](name: Text)(using specific: SpecificPath { type Self = PathType }): PathType = specific.path(name)

object SpecificFile:
  def apply[FileType](name: Text)(using specific: SpecificFile { type Self = FileType }): FileType = specific.file(name)

object SpecificDirectory:
  def apply[DirectoryType](name: Text)(using specific: SpecificDirectory { type Self = DirectoryType }): DirectoryType =
    specific.directory(name)
