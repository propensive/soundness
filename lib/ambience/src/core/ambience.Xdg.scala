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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.language.experimental.pureFunctions

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

object Xdg:
  def dataHome[path]
    ( using instantiable: (path is Instantiable across Paths from Text)^ )
    ( using environment: Environment, system: System )
  :   path =

    safely(Environment.xdgDataHome[path]).or(instantiable(t"${Directories.homeText}/.local/share"))


  def configHome[path]
    ( using instantiable: (path is Instantiable across Paths from Text)^ )
    ( using environment: Environment, system: System )
  :   path =

    safely(Environment.xdgConfigHome[path]).or(instantiable(t"${Directories.homeText}/.config"))


  def cacheHome[path]
    ( using instantiable: (path is Instantiable across Paths from Text)^ )
    ( using environment: Environment, system: System )
  :   path =

    safely(Environment.xdgCacheHome[path]).or(instantiable(t"${Directories.homeText}/.cache"))


  def stateHome[path]
    ( using instantiable: (path is Instantiable across Paths from Text)^ )
    ( using environment: Environment, system: System )
  :   path =

    safely(Environment.xdgStateHome[path]).or(instantiable(t"${Directories.homeText}/.local/state"))


  def runtimeDir[path]
    ( using instantiable: (path is Instantiable across Paths from Text)^ )
    ( using environment: Environment )
  :   Optional[path] =

    safely(Environment.xdgRuntimeDir[path])


  def bin[path]
    ( using instantiable: (path is Instantiable across Paths from Text)^ )
    ( using environment: Environment, system: System )
  :   path =

    safely(Environment.xdgConfigHome[path]).or(instantiable(t"${Directories.homeText}/.local/bin"))


  def dataDirs[path]
    ( using instantiable: (path is Instantiable across Paths from Text)^ )
    ( using environment: Environment, system: System )
  :   List[path] =

    safely(Environment.xdgDataDirs[List[path]]).or:
      List(t"/usr/local/share", t"/usr/share").map(instantiable(_))


  def configDirs[path]
    ( using instantiable: (path is Instantiable across Paths from Text)^ )
    ( using Environment, System )
  :   List[path] =

    safely(Environment.xdgConfigDirs[List[path]]).or(List(t"/etc/xdg").map(instantiable(_)))
