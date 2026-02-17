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
import workingDirectories.java
import homeDirectories.java
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
    val target = unsafely(out.peer(name))

    unsafely:
      val jarfile = unsafely(out.peer(t"$name.jar"))
      val bundle = Bundler.bundle(out, jarfile, fqcn"superlunary.Executor2")

      sh"java -Dbuild.executable=$target -jar $jarfile '[]'".exec[Exit]() match
        case Exit.Ok         => target
        case Exit.Fail(fail) => ???

  protected val scalac: Scalac[3.7] = Scalac(List(scalacOptions.experimental))


  protected def invoke[output](stage: Stage[output, Text, Path on Linux])
  :   Sandbox.Launcher =

      stage.remote: input =>
        unsafely:
          variables(inputParameters = input):
            sh"${stage.target}".exec[Exit]()

        t"""[""]"""

      Sandbox.Launcher(stage.target)
