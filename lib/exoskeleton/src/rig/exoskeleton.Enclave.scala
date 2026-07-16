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
import jacinta.*
import parasite.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import superlunary.*
import symbolism.*
import vacuous.*

import filesystemOptions.deleteRecursively.disabled
import logging.silentLogging
import probates.cancelProbate
import workingDirectories.javaWorkingDirectory

import filesystemBackends.virtualMachine


object Enclave:
  // A `Tool` is a *capability*: it references a live installed daemon process whose lifetime
  // is the `sandbox` block that spawns it (killed, and its files deleted, after the block).
  case class Tool(path: Path on Linux, pid: Pid) extends caps.ExclusiveCapability:
    def command: Text = path.name

    def completions(using Monitor)[result](block: => Unit): Optional[Text] =
      val promise = Promise[Text]()

      async:
        promise.offer(safely(sh"$path '{admin}' await".exec[Text]()).or(t"failed"))

      block
      safely(promise.await())

  case class Launcher(path: Path on Linux):
    def sandbox[result](block: (tool: Tool) ?=> result)
    :   result raises ExecError raises NumberError raises PathError =

      val completionScripts = sh"$path '{admin}' install".exec[Text]()
      val pid = Pid(sh"$path '{admin}' pid".exec[Text]().trim.as[Int])
      val tool = Tool(path, pid)

      block(using tool).also:
        sh"$path '{admin}' kill".exec[Exit]()

        completionScripts.trim.lines.map(_.as[Path on Linux]).each: item =>
          safely(item.delete())


case class Enclave(name: Text, buildId: Optional[Int] = Unset)(using Classloader, Environment)
extends Rig:
  type Result[output] = Enclave.Launcher
  type Form = Text
  type Target = Path on Linux
  type Transport = Json

  def stage(out: Path on Linux): Path on Linux =
    val target = unsafely(out.peer(name))

    unsafely:
      val name2 = t"$name.jar"
      val jarfile = out.peer(name2)
      // `Fqcn.apply` rather than the `fqcn""` interpolator: the macro's synthesized tree
      // fails capture-variable unification when expanded in a capture-checked module.
      val executor: Fqcn = safely(Fqcn(t"superlunary.Executor2")).vouch
      val bundle = Bundler.bundle(out, jarfile, executor)

      val cmd = (buildId: @unchecked) match
        case id: Int => sh"java -Dbuild.id=$id -Dbuild.executable=$target -jar $jarfile '[]'"
        case Unset   => sh"java -Dbuild.executable=$target -jar $jarfile '[]'"

      cmd.exec[Exit]() match
        case Exit.Ok         => target
        case Exit.Fail(fail) => ???

  protected val scalac: Scalac[3.8] = Scalac(List(scalacOptions.experimental))


  protected def invoke[output](stage: Stage[output, Text, Path on Linux])
  :   Enclave.Launcher =

    stage.remote: input =>
      unsafely:
        variables(inputParameters = input):
          sh"${stage.target}".exec[Exit]()

      t"""[""]"""

    Enclave.Launcher(stage.target)
