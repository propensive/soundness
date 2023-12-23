/*
    Rudiments, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import anticipation.*
import fulminate.*

import language.experimental.captureChecking

case class WorkingDirectoryError() extends Error(msg"there is no working directory")
case class HomeDirectoryError() extends Error(msg"there is no home directory")

object WorkingDirectory:
  given default(using Quickstart): WorkingDirectory = workingDirectories.default

@capability
trait WorkingDirectory:
  def directory(): Text
  def path[PathType: SpecificPath]: PathType = SpecificPath(directory())

object HomeDirectory:
  given default(using Quickstart): HomeDirectory = () => System.getProperty("user.home").nn.tt

@capability
trait HomeDirectory:
  def directory(): Text
  def path[PathType: SpecificPath]: PathType = SpecificPath(directory())

package workingDirectories:
  given default: WorkingDirectory = () => System.getProperty("user.dir").nn.tt
  //given none(using Raises[WorkingDirectoryError]): WorkingDirectory = () => abort(WorkingDirectoryError())

package homeDirectories:
  given default: HomeDirectory = () => System.getProperty("user.home").nn.tt
  //given none(using Raises[HomeDirectoryError]): HomeDirectory = () => abort(HomeDirectoryError())

def workingDirectory
    [PathType]
    (using directory: WorkingDirectory, specificPath: SpecificPath[PathType])
    : PathType^{specificPath} =
  directory.path[PathType]

def homeDirectory
    [PathType]
    (using directory: HomeDirectory, specificPath: SpecificPath[PathType])
    : PathType^{specificPath} =
  directory.path[PathType]
