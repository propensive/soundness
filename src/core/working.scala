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

import language.experimental.captureChecking

object WorkingDirectory:
  def apply(text: Maybe[Text] = Unset): WorkingDirectory = new WorkingDirectory(text) {}
  given default(using Quickstart): WorkingDirectory = workingDirectories.default

@capability
trait WorkingDirectory(val directory: Maybe[Text]):
  def path[PathType](using specificPath: SpecificPath[PathType]): Maybe[PathType^{specificPath}] =
    directory.let(SpecificPath(_))

object HomeDirectory:
  given default(using Quickstart): HomeDirectory = HomeDirectory(System.getProperty("user.home").nn.tt)

@capability
case class HomeDirectory(text: Text):
  def path[PathType: SpecificPath]: PathType = SpecificPath(text)

package workingDirectories:
  given default: WorkingDirectory = WorkingDirectory(Maybe(System.getProperty("user.dir")).let(_.tt))

package homeDirectories:
  given default: HomeDirectory = HomeDirectory(System.getProperty("user.home").nn.tt)

def workingDirectory
    [PathType]
    (using directory: WorkingDirectory, specificPath: SpecificPath[PathType])
    : Maybe[PathType^{specificPath}] =
  directory.path[PathType]

def homeDirectory
    [PathType]
    (using directory: HomeDirectory, specificPath: SpecificPath[PathType])
    : Maybe[PathType^{specificPath}] =
  directory.path[PathType]
