/*
    Octogenarian, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package octogenarian

import scala.compiletime.*

import anticipation.*, filesystemApi.serpentinePath
import contingency.*
import fulminate.*
import galilei.*
import guillotine.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*

import pathNavigation.posix

import GitError.Reason.*

given Realm = realm"octogenarian"

package gitCommands:
  given (using WorkingDirectory, GitEvent is Loggable) => GitCommand raises NameError raises PathError raises IoError raises ExecError as environmentDefault =

    val path: Path on Posix = sh"which git"()
    GitCommand(path)

export Octogenarian.{Tag, Branch, CommitHash, Refspec}

private[octogenarian] inline def git(using command: GitCommand): GitCommand = command
