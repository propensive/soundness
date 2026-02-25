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
package anthology

import ambience.*
import anticipation.*
import contingency.*
import digression.*
import distillate.*
import eucalyptus.*
import galilei.*
import gossamer.*
import hellenism.*
import prepositional.*
import revolution.*
import serpentine.*
import turbulence.*
import vacuous.*
import zeppelin.*

import manifestAttributes.*
import workingDirectories.java
import systems.java
import logging.silent
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.createNonexistent.disabled
import filesystemTraversal.preOrder

object Bundler:
  def classpath(out: Path on Linux): LocalClasspath = LocalClasspath:
    Classpath.Directory(out)
    :: (classloaders.threadContext.classpath.match
      case classpath: LocalClasspath => classpath.entries

      case _ =>
        unsafely(System.properties.java.`class`.path().decode[LocalClasspath]).entries)

  def bundle(directory: Path on Linux, jarfile0: Optional[Path on Linux], main: Optional[Fqcn])
  :   Path on Linux =
    val jarfile = jarfile0.or(directory.peer("tmpfile.jar"))

    val manifest =
      main.let(MainClass(_)).let: main =>
        Manifest(ManifestVersion(()), CreatedBy(t"Soundness"), main)
      . or:
          Manifest(ManifestVersion(()), CreatedBy(t"Soundness"))


    unsafely:
      Zipfile.write(jarfile):
        ZipEntry(%.on[Zip] / "META-INF" / "MANIFEST.MF", manifest)
        :: classpath(directory).entries.to(List).flatMap:
          case ClasspathEntry.Directory(directory) =>
            unsafely:
              val root = directory.decode[Path on Linux]
              root.descendants.to(List).map: file =>
                file.open: handle =>
                  val ref = %.on[Zip] + root.toward(file).on[Zip]
                  ZipEntry(ref, handle.read[Data])

          case ClasspathEntry.Jar(jar) =>
            unsafely:
              val jarfile = workingDirectory[Path on Linux].resolve(jar)
              jarfile.open: handle =>
                ZipStream(handle).keep { path => path.encode != t"META-INF/MANIFEST.MF" }
                . map: entry =>
                    ZipEntry(entry.ref, entry.read[Data])

                . to(List)

          case _ =>
            List()

      jarfile
