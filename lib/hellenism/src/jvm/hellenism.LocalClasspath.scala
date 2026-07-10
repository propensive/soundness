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
package hellenism

import ambience.*
import anticipation.*
import contingency.*
import distillate.*
import galilei.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import symbolism.*

import filesystemBackends.virtualMachine

object LocalClasspath:
  given encodable: System => LocalClasspath is Encodable in Text = _()


  given decodable: (System, Tactic[PropertyError])
  =>  LocalClasspath is Decodable in Text =

    classpath =>
      val entries: List[ClasspathEntry.Directory | ClasspathEntry.Jar] =
        classpath.cut(System.properties.path.separator()).map: path =>
          if path.ends(t"/") then ClasspathEntry.Directory(path)
          else if path.ends(t".jar") then ClasspathEntry.Jar(path)
          else ClasspathEntry.Directory(path)

      new LocalClasspath(entries, entries.to(Set))


  def apply
    ( entries: (ClasspathEntry.Directory | ClasspathEntry.Jar | ClasspathEntry.JavaRuntime.type)* )
  :   LocalClasspath =

    new LocalClasspath(entries.toList, entries.to(Set))


  def apply[path: Abstractable across Paths to Text]
    ( path: path )
    ( using Tactic[PathError],
            Tactic[IoError],
            DereferenceSymlinks )
  :   LocalClasspath =

    new LocalClasspath(Nil, Set.empty) + path


  given paths: [path: Abstractable across Paths to Text]
  =>  ( Tactic[PathError], Tactic[IoError], DereferenceSymlinks )
  =>  LocalClasspath is Addable by path to LocalClasspath =

    (classpath, path) =>
      path.generic.decode[Path on Linux].pipe: path =>
        val entry: ClasspathEntry.Directory | ClasspathEntry.Jar = path.entry() match
          case Directory => ClasspathEntry.Directory(path.encode)
          case _         => ClasspathEntry.Jar(path.encode)

        if classpath.entrySet.has(entry) then classpath
        else new LocalClasspath(entry :: classpath.entries, classpath.entrySet + entry)


class LocalClasspath private
  ( val entries
  : List[ClasspathEntry.Directory | ClasspathEntry.Jar | ClasspathEntry.JavaRuntime.type],
    val entrySet: Set[ClasspathEntry] )
extends Classpath:
  def apply()(using System): Text =
    entries.flatMap:
      case ClasspathEntry.Directory(directory) => List(directory)
      case ClasspathEntry.Jar(jar)             => List(jar)
      case _                                   => Nil

    . join(unsafely(System.properties.path.separator()))
