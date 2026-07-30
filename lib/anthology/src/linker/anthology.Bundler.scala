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
package anthology

import proscenium.compat.*

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
import rudiments.*
import revolution.*
import serpentine.*
import turbulence.*
import vacuous.*
import zeppelin.*

import filesystemBackends.virtualMachine
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemTraversal.preOrderTraversal
import logging.silentLogging
import manifestAttributes.*
import systems.javaSystem
import workingDirectories.javaWorkingDirectory

object Bundler:
  // The classpath of the running application, as introspected from the thread-context
  // classloader (or the `java.class.path` system property as a fallback). A staging rig pairs
  // this with its compiled output as a `Compilation` and links it into a self-contained JAR
  // with `Linker[Artifact.Jar]`, so the JAR carries the rig's own executor and dependencies.
  def applicationClasspath: LocalClasspath =
    val entries = classloaders.threadContextClassloader.classpath.match
      case classpath: LocalClasspath => classpath.entries

      case _ =>
        unsafely(System.properties.java.`class`.path().as[LocalClasspath]).entries

    LocalClasspath(entries*)


  private[anthology] def assemble
    ( classpath: LocalClasspath, jarfile: Path on Linux, main: Optional[Fqcn] )
  :   Path on Linux raises ZipError raises PathError raises IoError raises StreamError =

    val manifest =
      main.let(MainClass(_)).let: main =>
        Manifest(ManifestVersion(()), CreatedBy(t"Soundness"), main)

      . or:
          Manifest(ManifestVersion(()), CreatedBy(t"Soundness"))


    val omissions: Set[Text] = Set("MANIFEST.MF", "plugin.properties")

    Zipfile.write(jarfile):
      val entries =
        Zip.Entry(%.on[Zip] / "META-INF" / "MANIFEST.MF", manifest) ::
          classpath.entries.bind:
          case ClasspathEntry.Directory(directory) =>
            val root = directory.as[Path on Linux]
            root.descendants.stdlib.filter: entry => !omissions(entry.name)
            . map: file =>
              if file.entry() == Directory then Unset else
                val ref = %.on[Zip] + root.toward(file).on[Zip]
                Zip.Entry(ref, file.read[Data])

            . compact

          case ClasspathEntry.Jar(jar) =>
            val jarfile = workingDirectory[Path on Linux].resolve(jar)

            // Re-emit each entry verbatim: it already carries its compressed bytes, so no
            // decompression or recompression is needed.
            Zipfile.read(jarfile).entries.stdlib.filter: entry =>
              val name: Text = entry.ref.encode
              !entry.directory && name != t"META-INF/MANIFEST.MF"

          case _ =>
            Nil.stdlib

      entries.distinctBy(_.ref)

    jarfile
