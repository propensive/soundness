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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import proscenium.compat.*

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

  given decodable: (System, Tactic[Property.Error])
  =>  LocalClasspath is Decodable in Text =

    classpath =>
      val entries =
        classpath.cut(System.properties.path.separator()).stdlib
        . map[Classpath.Entry.Directory | Classpath.Entry.Jar]: path =>
          if path.ends(t"/") then Classpath.Entry.Directory(path)
          else if path.ends(t".jar") then Classpath.Entry.Jar(path)
          else Classpath.Entry.Directory(path)

      new LocalClasspath(List.of(entries), Set.from(entries))


  def apply
    ( entries: (Classpath.Entry.Directory | Classpath.Entry.Jar | Classpath.Entry.JavaRuntime.type)* )
  :   LocalClasspath =

    new LocalClasspath(List.of(entries.toList), Set.from(entries))


  def apply[path: Abstractable across Paths to Text]
    ( path: path )
    ( using Tactic[Path.Error],
            Tactic[IoError],
            DereferenceSymlinks )
  :   LocalClasspath =

    new LocalClasspath(Nil, Set.empty) + path


  given paths: [path: Abstractable across Paths to Text]
  =>  ( pathTactic: Tactic[Path.Error], ioTactic: Tactic[IoError], deref: DereferenceSymlinks )
  =>  ((LocalClasspath is Addable by path to LocalClasspath)^{pathTactic, ioTactic}) =

    (classpath, path) =>
      path.generic.as[Path on Linux].pipe: path =>
        val entry: Classpath.Entry.Directory | Classpath.Entry.Jar = path.entry() match
          case Directory => Classpath.Entry.Directory(path.encode)
          case _         => Classpath.Entry.Jar(path.encode)

        if classpath.entrySet.has(entry) then classpath
        else new LocalClasspath(entry :: classpath.entries, Set.of(classpath.entrySet.stdlib + entry))

class LocalClasspath private
  ( val entries
  : List[Classpath.Entry.Directory | Classpath.Entry.Jar | Classpath.Entry.JavaRuntime.type],
    val entrySet: Set[Classpath.Entry] )
extends Classpath:
  def apply()(using System): Text =
    entries.bind:
      case Classpath.Entry.Directory(directory) => List(directory)
      case Classpath.Entry.Jar(jar)             => List(jar)
      case _                                   => Nil

    . join(unsafely(System.properties.path.separator()))
