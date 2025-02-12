                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                    ╭───╮                                         ┃
┃  ╭─────────╮                                       │   │                                         ┃
┃  │   ╭─────╯╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮  ┃
┃  │   ╰─────╮│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯  ┃
┃  ╰─────╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ├╌╯╌─╯╰─╌ ╰───╮╰─╌ ╰───╮  ┃
┃  ╭─────╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╌   │╭───╌   │  ┃
┃  ╰─────────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯  ┃
┃                                                                                                  ┃
┃    Soundness, version 0.27.0. © Copyright 2023-25 Jon Pretty, Propensive OÜ.                     ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        http://www.apache.org/licenses/LICENSE-2.0                                                ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package ambience

import language.experimental.pureFunctions

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

object Xdg:
  def dataHome[PathType: Instantiable across Paths from Text](using environment: Environment, home: HomeDirectory)
  :     PathType =
    safely(Environment.xdgDataHome[PathType]).or(PathType(t"${home.directory()}/.local/share"))

  def configHome[PathType: Instantiable across Paths from Text](using environment: Environment, home: HomeDirectory)
  :     PathType =
    safely(Environment.xdgConfigHome[PathType]).or(PathType(t"${home.directory()}/.config"))

  def cacheHome[PathType: Instantiable across Paths from Text](using environment: Environment, home: HomeDirectory)
  :     PathType =

    safely(Environment.xdgCacheHome[PathType]).or(PathType(t"${home.directory()}/.cache"))

  def stateHome[PathType: Instantiable across Paths from Text](using environment: Environment, home: HomeDirectory)
  :     PathType =

    safely(Environment.xdgStateHome[PathType]).or(PathType(t"${home.directory()}/.local/state"))

  def runtimeDir[PathType: Instantiable across Paths from Text](using environment: Environment): Optional[PathType] =
    safely(Environment.xdgRuntimeDir[PathType])

  def bin[PathType: Instantiable across Paths from Text](using environment: Environment, home: HomeDirectory): PathType =
    safely(Environment.xdgConfigHome[PathType]).or(PathType(t"${home.directory()}/.local/bin"))

  def dataDirs[PathType: Instantiable across Paths from Text](using Environment, SystemProperties): List[PathType] =
    safely(Environment.xdgDataDirs[List[PathType]]).or:
      List(t"/usr/local/share", t"/usr/share").map(PathType(_))

  def configDirs[PathType: Instantiable across Paths from Text](using Environment, SystemProperties): List[PathType] =
    safely(Environment.xdgConfigDirs[List[PathType]]).or(List(t"/etc/xdg").map(PathType(_)))
