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

import filesystemOptions.createNonexistent.disabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled
import filesystemTraversal.preOrderTraversal
import logging.silentLogging
import manifestAttributes.*
import systems.javaSystem
import workingDirectories.javaWorkingDirectory

import filesystemBackends.virtualMachine

object Bundler:
  def classpath(out: Path on Linux): LocalClasspath =
    val entries = Classpath.Directory(out) :: (classloaders.threadContextClassloader.classpath.match
      case classpath: LocalClasspath => classpath.entries

      case _ =>
        unsafely(System.properties.java.`class`.path().decode[LocalClasspath]).entries)

    LocalClasspath(entries*)


  def bundle(directory: Path on Linux, jarfile0: Optional[Path on Linux], main: Optional[Fqcn])
  :   Path on Linux raises ZipError raises PathError raises IoError raises StreamError =

    val jarfile = jarfile0.or(directory.peer("tmpfile.jar"))

    val manifest =
      main.let(MainClass(_)).let: main =>
        Manifest(ManifestVersion(()), CreatedBy(t"Soundness"), main)

      . or:
          Manifest(ManifestVersion(()), CreatedBy(t"Soundness"))


    val omissions: Set[Text] = Set("MANIFEST.MF", "plugin.properties")

    Zipfile.write(jarfile):
      val entries =
        Zip.Entry(%.on[Zip] / "META-INF" / "MANIFEST.MF", manifest) ::
          classpath(directory).entries.to(List).flatMap:
          case ClasspathEntry.Directory(directory) =>
            val root = directory.decode[Path on Linux]
            root.descendants.to(List).filter: entry => !omissions(entry.name)
            . map: file =>
              if file.entry() == Directory then Unset else file.open: handle =>
                val ref = %.on[Zip] + root.toward(file).on[Zip]
                Zip.Entry(ref, handle.read[Data])

            . compact

          case ClasspathEntry.Jar(jar) =>
            val jarfile = workingDirectory[Path on Linux].resolve(jar)

            // Re-emit each entry verbatim: it already carries its compressed bytes, so no
            // decompression or recompression is needed.
            Zipfile.read(jarfile).entries.to(List).filter: entry =>
              val name: Text = entry.ref.encode
              !entry.directory && name != t"META-INF/MANIFEST.MF"

          case _ =>
            Nil

      entries.distinctBy(_.ref)

    jarfile
