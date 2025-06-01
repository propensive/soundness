                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.32.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
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
  def dataHome[path: Instantiable across Paths from Text]
        ( using environment: Environment, home: HomeDirectory )
  : path =

      safely(Environment.xdgDataHome[path]).or(path(t"${home.directory()}/.local/share"))


  def configHome[path: Instantiable across Paths from Text]
        ( using environment: Environment, home: HomeDirectory )
  : path =

      safely(Environment.xdgConfigHome[path]).or(path(t"${home.directory()}/.config"))


  def cacheHome[path: Instantiable across Paths from Text]
        ( using environment: Environment, home: HomeDirectory )
  : path =

      safely(Environment.xdgCacheHome[path]).or(path(t"${home.directory()}/.cache"))


  def stateHome[path: Instantiable across Paths from Text]
        ( using environment: Environment, home: HomeDirectory )
  : path =

      safely(Environment.xdgStateHome[path]).or(path(t"${home.directory()}/.local/state"))


  def runtimeDir[path: Instantiable across Paths from Text](using environment: Environment)
  : Optional[path] =

      safely(Environment.xdgRuntimeDir[path])


  def bin[path: Instantiable across Paths from Text]
        ( using environment: Environment, home: HomeDirectory )
  : path =

      safely(Environment.xdgConfigHome[path]).or(path(t"${home.directory()}/.local/bin"))


  def dataDirs[path: Instantiable across Paths from Text]
        ( using environment: Environment, systemProperties: SystemProperties )
  : List[path] =

      safely(Environment.xdgDataDirs[List[path]]).or:
        List(t"/usr/local/share", t"/usr/share").map(path(_))


  def configDirs[path: Instantiable across Paths from Text](using Environment, SystemProperties)
  : List[path] =

      safely(Environment.xdgConfigDirs[List[path]]).or(List(t"/etc/xdg").map(path(_)))
