/*
    Ambience, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import digression.*
import anticipation.*
import rudiments.*
import ambience.*

case class Xdg(home: Text):
  def dataHome[PathType: GenericPathMaker](using Environment): PathType =
    safely(Environment.xdgDataHome[PathType]).or(GenericPath(t"$home/.local/share"))
  
  def configHome[PathType: GenericPathMaker](using Environment): PathType =
    safely(Environment.xdgConfigHome[PathType]).or(GenericPath(t"$home/.config"))
  
  def cacheHome[PathType: GenericPathMaker](using Environment): PathType =
    safely(Environment.xdgCacheHome[PathType]).or(GenericPath(t"$home/.cache"))
  
  def stateHome[PathType: GenericPathMaker](using Environment): PathType =
    safely(Environment.xdgStateHome[PathType]).or(GenericPath(t"$home/.local/state"))
  
  def runtimeDir[PathType: GenericPathMaker](using Environment): Maybe[PathType] =
    safely(Environment.xdgRuntimeDir[PathType])
  
  def bin[PathType: GenericPathMaker](using Environment): PathType =
    safely(Environment.xdgConfigHome[PathType]).or(GenericPath(t"$home/.local/bin"))
  
  def dataDirs[PathType: GenericPathMaker](using Environment, SystemProperties): List[PathType] =
    safely(Environment.xdgDataDirs[List[PathType]]).or:
      List(t"/usr/local/share", t"/usr/share").map(GenericPath(_))
  
  def configDirs[PathType: GenericPathMaker](using Environment, SystemProperties): List[PathType] =
    safely(Environment.xdgConfigDirs[List[PathType]]).or:
      List(t"/etc/xdg").map(GenericPath(_))
