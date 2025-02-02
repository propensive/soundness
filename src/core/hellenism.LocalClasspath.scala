/*
    Hellenism, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hellenism

import ambience.*
import anticipation.*
import contingency.*
import galilei.*
import gossamer.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import symbolism.*

object LocalClasspath:

  given SystemProperties => LocalClasspath is Encodable in Text = _()

  given (SystemProperties, Tactic[SystemPropertyError]) => Decoder[LocalClasspath] =
    classpath =>
      val entries: List[ClasspathEntry.Directory | ClasspathEntry.Jar] =
        classpath.cut(Properties.path.separator()).map: path =>
          if path.ends(t"/") then ClasspathEntry.Directory(path)
          else if path.ends(t".jar") then ClasspathEntry.Jar(path)
          else ClasspathEntry.Directory(path)

      new LocalClasspath(entries, entries.to(Set))

  def apply
     (entries: List[ClasspathEntry.Directory | ClasspathEntry.Jar |
                ClasspathEntry.JavaRuntime.type])
  :     LocalClasspath =
    new LocalClasspath(entries, entries.to(Set))

  given [PathType: Abstractable across Paths into Text]
  =>    (Tactic[PathError], Tactic[IoError], Tactic[NameError], Navigable, DereferenceSymlinks)
  =>    LocalClasspath is Addable by PathType into LocalClasspath =
    (classpath, path) =>
      Path.parse[Filesystem](path.generic).pipe: path =>
        val entry: ClasspathEntry.Directory | ClasspathEntry.Jar = path.entry() match
          case Directory => ClasspathEntry.Directory(path.text)
          case _         => ClasspathEntry.Jar(path.text)

        if classpath.entrySet.contains(entry) then classpath
        else new LocalClasspath(entry :: classpath.entries, classpath.entrySet + entry)

class LocalClasspath private
   (val entries: List
                  [ClasspathEntry.Directory
                   | ClasspathEntry.Jar
                   | ClasspathEntry.JavaRuntime.type],
    val entrySet: Set[ClasspathEntry])
extends Classpath:

  def apply()(using SystemProperties): Text =
    entries.flatMap:
      case ClasspathEntry.Directory(directory) => List(directory)
      case ClasspathEntry.Jar(jar)             => List(jar)
      case _                                   => Nil

    . join(unsafely(Properties.path.separator()))
