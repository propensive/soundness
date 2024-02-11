/*
    Ambience, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import gossamer.*
import contingency.*
import anticipation.*
import rudiments.*
import vacuous.*
import ambience.*

//import language.experimental.captureChecking

object Xdg:

  def dataHome
      [PathType]
      (using specificPath: SpecificPath[PathType], environment: Environment, home: HomeDirectory)
      : PathType/*^{specificPath, environment, home}*/ =
    safely(Environment.xdgDataHome[PathType]).or(SpecificPath(t"${home.directory()}/.local/share"))
  
  def configHome
      [PathType]
      (using specificPath: SpecificPath[PathType], environment: Environment, home: HomeDirectory)
      : PathType/*^{specificPath, environment, home}*/ =
    safely(Environment.xdgConfigHome[PathType]).or(SpecificPath(t"${home.directory()}/.config"))
  
  def cacheHome
      [PathType]
      (using specificPath: SpecificPath[PathType], environment: Environment, home: HomeDirectory)
      : PathType/*^{specificPath, environment, home}*/ =
    safely(Environment.xdgCacheHome[PathType]).or(SpecificPath(t"${home.directory()}/.cache"))
  
  def stateHome
      [PathType]
      (using environment: Environment, specificPath: SpecificPath[PathType], home: HomeDirectory)
      : PathType/*^{specificPath, environment, home}*/ =
    safely(Environment.xdgStateHome[PathType]).or(SpecificPath(t"${home.directory()}/.local/state"))
  
  def runtimeDir
      [PathType]
      (using specificPath: SpecificPath[PathType], environment: Environment)
      : Optional[PathType/*^{specificPath, environment}*/] =
    safely(Environment.xdgRuntimeDir[PathType])
  
  def bin
      [PathType]
      (using specificPath: SpecificPath[PathType], environment: Environment, home: HomeDirectory)
      : PathType/*^{specificPath, environment, home}*/ =
    safely(Environment.xdgConfigHome[PathType]).or(SpecificPath(t"${home.directory()}/.local/bin"))
  
  def dataDirs
      [PathType]
      (using SpecificPath[PathType], Environment, SystemProperties): List[PathType] =
    safely(Environment.xdgDataDirs[List[PathType]]).or:
      List(t"/usr/local/share", t"/usr/share").map(SpecificPath(_))
  
  def configDirs[PathType: SpecificPath](using Environment, SystemProperties): List[PathType] =
    safely(Environment.xdgConfigDirs[List[PathType]]).or:
      List(t"/etc/xdg").map(SpecificPath(_))
