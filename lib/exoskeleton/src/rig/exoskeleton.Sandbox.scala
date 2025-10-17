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
┃    Soundness, version 0.44.0.                                                                    ┃
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

import ambience.*
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
import parasite.*
import prepositional.*
import revolution.*
import rudiments.*
import serpentine.*
import spectacular.*
import superlunary.*
import symbolism.*
import turbulence.*
import vacuous.*
import zeppelin.*

import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.createNonexistent.disabled
import filesystemOptions.createNonexistentParents.disabled
import filesystemOptions.overwritePreexisting.enabled
import filesystemOptions.deleteRecursively.disabled
import filesystemTraversal.preOrder
import manifestAttributes.*

import logging.silent
import workingDirectories.jre
import homeDirectories.jre
import charEncoders.utf8
import codicils.cancel


object Sandbox:
  case class Tool(path: Path on Linux, pid: Pid):
    def command: Text = path.name

    def completions(using Monitor)[result](block: => Unit): Optional[Text] =
      val promise = Promise[Text]()

      async:
        promise.offer(safely(sh"$path '{admin}' await".exec[Text]()).or(t"failed"))

      block
      safely(promise.await())

  case class Launcher(path: Path on Linux):
    def sandbox[result](block: (tool: Tool) ?=> result): result =
      val completionScripts = unsafely(sh"$path '{admin}' install".exec[Text]())
      val pid = Pid(unsafely(sh"$path '{admin}' pid".exec[Text]().trim.decode[Int]))
      val tool = Tool(path, pid)

      block(using tool).also:
        unsafely:
          sh"$path '{admin}' kill".exec[Exit]()
          completionScripts.trim.lines.map(_.decode[Path on Linux]).each(_.delete())


case class Sandbox(name: Text)(using Classloader, Environment) extends Rig:
  type Result[output] = Sandbox.Launcher
  type Form = Text
  type Target = Path on Linux
  type Transport = Json

  def stage(out: Path on Linux): Path on Linux =
    val jarfile = out.peer("tmpfile.jar")
    val target = unsafely(out.peer(name))

    val manifest =
      Manifest(ManifestVersion(()), CreatedBy(t"Soundness"), MainClass(fqcn"superlunary.Executor2"))

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


  protected def invoke[output](stage: Stage[output, Text, Path on Linux])
  : Sandbox.Launcher =

      stage.remote: input =>
        unsafely:
          variables(inputParameters = input):
            sh"${stage.target}".exec[Exit]()

        t"""[""]"""

      Sandbox.Launcher(stage.target)
