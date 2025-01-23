/*
    Ambience, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package ambience

import language.experimental.pureFunctions

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

object Xdg:
  def dataHome[PathType: SpecificPath](using environment: Environment, home: HomeDirectory)
  :     PathType =
    safely(Environment.xdgDataHome[PathType]).or(SpecificPath(t"${home.directory()}/.local/share"))

  def configHome[PathType: SpecificPath](using environment: Environment, home: HomeDirectory)
  :     PathType =
    safely(Environment.xdgConfigHome[PathType]).or(SpecificPath(t"${home.directory()}/.config"))

  def cacheHome[PathType: SpecificPath](using environment: Environment, home: HomeDirectory)
  :     PathType =

    safely(Environment.xdgCacheHome[PathType]).or(SpecificPath(t"${home.directory()}/.cache"))

  def stateHome[PathType: SpecificPath](using environment: Environment, home: HomeDirectory)
  :     PathType =

    safely(Environment.xdgStateHome[PathType]).or(SpecificPath(t"${home.directory()}/.local/state"))

  def runtimeDir[PathType: SpecificPath](using environment: Environment): Optional[PathType] =
    safely(Environment.xdgRuntimeDir[PathType])

  def bin[PathType: SpecificPath](using environment: Environment, home: HomeDirectory): PathType =
    safely(Environment.xdgConfigHome[PathType]).or(SpecificPath(t"${home.directory()}/.local/bin"))

  def dataDirs[PathType: SpecificPath](using Environment, SystemProperties): List[PathType] =
    safely(Environment.xdgDataDirs[List[PathType]]).or:
      List(t"/usr/local/share", t"/usr/share").map(SpecificPath(_))

  def configDirs[PathType: SpecificPath](using Environment, SystemProperties): List[PathType] =
    safely(Environment.xdgConfigDirs[List[PathType]]).or(List(t"/etc/xdg").map(SpecificPath(_)))
