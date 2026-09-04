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

import filesystemBackends.javaBaseFilesystem
import filesystemOptions.dereferenceSymlinks
import filesystemTraversal.preOrderTraversal
import logging.silentLogging
import manifestAttributes.*
import systems.javaBaseSystem
import workingDirectories.javaBaseWorkingDirectory

object Bundler:
  // The classpath of the running application, as introspected from the thread-context
  // classloader (or the `java.class.path` system property as a fallback). A staging rig pairs
  // this with its compiled output as an `Emission` and packages it into a self-contained JAR
  // through the `Jar` edge, so the JAR carries the rig's own executor and dependencies.
  def applicationClasspath: LocalClasspath =
    val entries = classloaders.threadContextClassloader.classpath.match
      case classpath: LocalClasspath => classpath.entries

      case _ =>
        unsafely(System.properties.java.`class`.path().as[LocalClasspath]).entries

    LocalClasspath(entries*)


  private[anthology] def assemble
    ( classpath: LocalClasspath, jarfile: Path on Linux, main: Optional[Fqcn] )
  :   Path on Linux raises Zip.Error raises Path.Error raises Io.Error raises Truncation.Error =

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
          case Classpath.Entry.Directory(directory) =>
            val root = directory.as[Path on Linux]

            root.descendants.filter { entry => !omissions.has(entry.name) }.bind: file =>
              if file.entry() == Directory then Nil else
                val ref = %.on[Zip] + root.toward(file).on[Zip]
                List(Zip.Entry(ref, file.read[Data]))

            . to[List]

          case Classpath.Entry.Jar(jar) =>
            val jarfile = workingDirectory[Path on Linux].resolve(jar)

            // Re-emit each entry verbatim: it already carries its compressed bytes, so no
            // decompression or recompression is needed.
            Zipfile.read(jarfile).entries.filter: entry =>
              val name: Text = entry.ref.encode
              !entry.directory && name != t"META-INF/MANIFEST.MF"

          case _ =>
            Nil

      entries.deduplicate(_.ref)

    jarfile
