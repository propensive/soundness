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
┃    Soundness, version 0.41.0.                                                                    ┃
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
package exoskeleton

import anthology.*
import anticipation.*
import contingency.*
import digression.*
import distillate.*
import eucalyptus.*
import galilei.*
import gossamer.*
import guillotine.*
import hellenism.*
import hieroglyph.*
import jacinta.*
import prepositional.*
import revolution.*
import rudiments.*
import serpentine.*
import spectacular.*
import superlunary.*
import symbolism.*
import turbulence.*
import zeppelin.*

import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.createNonexistent.disabled
import filesystemTraversal.preOrder
import manifestAttributes.*

import logging.silent
import workingDirectories.jre
import charEncoders.utf8


case class ShellScript(name: Text)(using Classloader) extends Rig:
  type Result[output] = output
  type Form = Text
  type Target = Path on Linux
  type Transport = Json

  def deploy(out: Path on Linux): Path on Linux =
    val jarfile = out.peer("tmpfile.jar")
    val target = unsafely(out.peer(name))
    println(jarfile.encode.toString)
    val manifest =
      Manifest
       (ManifestVersion(()),
        CreatedBy(t"Soundness"),
        MainClass(fqcn"superlunary.Executor2"))

    unsafely:
      Zipfile.write(jarfile):
        ZipEntry(%.on[Zip] / "META-INF" / "MANIFEST.MF", manifest)
        :: classpath(out).entries.to(List).flatMap:
          case ClasspathEntry.Directory(directory) =>
            unsafely:
              val root = directory.decode[Path on Linux]
              root.descendants.to(List).map: file =>
                file.open: handle =>
                  val ref = %.on[Zip] + file.relativeTo(root).on[Zip]
                  ZipEntry(ref, handle.read[Bytes])

          case ClasspathEntry.Jar(jar) =>
            println(t"Jar: $jar")
            unsafely:
              val jarfile = workingDirectory[Path on Linux].resolve(jar)
              jarfile.open: handle =>
                ZipStream(handle).keep { path => path.show != t"META-INF/MANIFEST.MF" }
                . map: entry =>
                    ZipEntry(entry.ref, entry.read[Bytes])

                . to(List)

          case _ =>
            List()

      sh"java -Dbuild.executable=$target -jar $jarfile '[]'".exec[Exit]() match
        case Exit.Ok      => target
        case Exit.Fail(_) => ???


  protected val scalac: Scalac[3.7] = Scalac(List(scalacOptions.experimental))

  protected def invoke[output](deployment: Deployment[output, Text, Path on Linux]): output =
    println(deployment.target)
    deployment.remote: input =>
      t"[\"result\"]"
