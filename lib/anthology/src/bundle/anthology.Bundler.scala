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

import filesystemBackends.virtualMachine
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemTraversal.preOrderTraversal
import logging.silentLogging
import manifestAttributes.*
import systems.javaSystem
import workingDirectories.javaWorkingDirectory

object Bundler:
  def classpath(out: Path on Linux): LocalClasspath =
    val entries = Classpath.Directory(out) :: (classloaders.threadContextClassloader.classpath.match
      case classpath: LocalClasspath => classpath.entries

      case _ =>
        unsafely(System.properties.java.`class`.path().as[LocalClasspath]).entries)

    LocalClasspath(entries*)


  def bundle(directory: Path on Linux, jarfile0: Optional[Path on Linux], main: Optional[Fqcn])
  :   Path on Linux raises ZipError raises PathError raises IoError raises StreamError =

    assemble(classpath(directory), jarfile0.or(directory.peer("tmpfile.jar")), main)


  // Bundles a classfile compilation, together with the classpath it was compiled against, as an
  // executable JAR file.
  def bundle
    ( compilation: Compilation[Universe.Classfile],
      jarfile0:    Optional[Path on Linux],
      main:        Optional[Fqcn] )
  :   Path on Linux raises ZipError raises PathError raises IoError raises StreamError =

    val entries = Classpath.Directory(compilation.out) :: compilation.classpath.entries.to(List)
    assemble(LocalClasspath(entries*), jarfile0.or(compilation.out.peer("tmpfile.jar")), main)


  // Bundles a compilation's output—classfiles, and any `.sjsir` or `.nir` alongside them—as a
  // library JAR for downstream assembly: the JAR is a valid classpath entry for further
  // compilations in the same universe, and for a later `Linker`.
  def library[universe <: Universe]
    ( compilation: Compilation[universe], jarfile0: Optional[Path on Linux] )
  :   Path on Linux raises ZipError raises PathError raises IoError raises StreamError =

    val entries = List(Classpath.Directory(compilation.out))
    assemble(LocalClasspath(entries*), jarfile0.or(compilation.out.peer("tmpfile.jar")), Unset)


  private def assemble
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
          classpath.entries.to(List).flatMap:
          case ClasspathEntry.Directory(directory) =>
            val root = directory.as[Path on Linux]
            root.descendants.to(List).filter: entry => !omissions(entry.name)
            . map: file =>
              if file.entry() == Directory then Unset else
                val ref = %.on[Zip] + root.toward(file).on[Zip]
                Zip.Entry(ref, file.read[Data])

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
