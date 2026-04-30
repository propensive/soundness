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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import fulminate.*
import gossamer.*
import prepositional.*
import vacuous.*

object Directories:
  def home[path: Instantiable across Paths from Text](using system: System): path =
    path(homeText)


  def homeText(using system: System): Text =
    system(t"user.home").or(panic(m"the `user.home` system property is not set"))


  def dataHome[path: Instantiable across Paths from Text]
    ( using environment: Environment, system: System )
  :   path =

    if isWindows then roamingAppData[path] else Xdg.dataHome[path]


  def configHome[path: Instantiable across Paths from Text]
    ( using environment: Environment, system: System )
  :   path =

    if isWindows then roamingAppData[path] else Xdg.configHome[path]


  def cacheHome[path: Instantiable across Paths from Text]
    ( using environment: Environment, system: System )
  :   path =

    if isWindows then localAppData[path] else Xdg.cacheHome[path]


  def stateHome[path: Instantiable across Paths from Text]
    ( using environment: Environment, system: System )
  :   path =

    if isWindows then localAppData[path] else Xdg.stateHome[path]


  def runtimeDir[path: Instantiable across Paths from Text]
    ( using environment: Environment, system: System )
  :   Optional[path] =

    if isWindows then path(t"${localAppDataText}\\Temp") else Xdg.runtimeDir[path]


  def bin[path: Instantiable across Paths from Text]
    ( using environment: Environment, system: System )
  :   path =

    if isWindows then path(t"${localAppDataText}\\Programs") else Xdg.bin[path]


  def dataDirs[path: Instantiable across Paths from Text]
    ( using environment: Environment, system: System )
  :   List[path] =

    if isWindows then List(programData[path]) else Xdg.dataDirs[path]


  def configDirs[path: Instantiable across Paths from Text]
    ( using environment: Environment, system: System )
  :   List[path] =

    if isWindows then List(programData[path]) else Xdg.configDirs[path]


  private def isWindows(using system: System): Boolean =
    system(t"os.name").or(t"").lower.starts(t"windows")


  private def localAppDataText(using environment: Environment, system: System): Text =
    safely(Environment[Text](t"LOCALAPPDATA")).or(t"${homeText}\\AppData\\Local")


  private def roamingAppDataText(using environment: Environment, system: System): Text =
    safely(Environment[Text](t"APPDATA")).or(t"${homeText}\\AppData\\Roaming")


  private def programDataText(using environment: Environment): Text =
    safely(Environment[Text](t"PROGRAMDATA")).or(t"C:\\ProgramData")


  private def localAppData[path: Instantiable across Paths from Text]
    ( using environment: Environment, system: System )
  :   path =

    path(localAppDataText)


  private def roamingAppData[path: Instantiable across Paths from Text]
    ( using environment: Environment, system: System )
  :   path =

    path(roamingAppDataText)


  private def programData[path: Instantiable across Paths from Text]
    ( using environment: Environment )
  :   path =

    path(programDataText)
