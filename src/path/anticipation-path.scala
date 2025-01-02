/*
    Anticipation, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import scala.annotation.*

import language.experimental.captureChecking

extension [PathType](path: PathType)
  inline def fullPath: Text = compiletime.summonFrom:
    case generic: (PathType is GenericPath)      => generic.pathText(path)
    case generic: (PathType is GenericFile)      => generic.fileText(path)
    case generic: (PathType is GenericDirectory) => generic.directoryText(path)

extension [PathType: GenericPath](path: PathType)
  def pathText: Text = PathType.pathText(path)

extension [FileType: GenericFile](file: FileType)
  def fileText: Text = FileType.fileText(file)

extension [DirectoryType: GenericDirectory](directory: DirectoryType)
  def directoryText: Text = DirectoryType.directoryText(directory)
