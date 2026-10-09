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
package exoskeleton

import scala.caps

import ambience.*
import anthology.*
import anticipation.*
import contingency.*
import digression.*
import distillate.*
import eucalyptus.*
import fulminate.*
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

import filesystemOptions.deleteOnlyEmpty
import logging.silentLogging
import probates.cancelProbate
import systems.javaBaseSystem
import threading.platformThreading
import workingDirectories.javaBaseWorkingDirectory

import filesystemBackends.javaBaseFilesystem

object Enclave:
  // Raised when the daemon does not answer the `{admin} pid` request. Overwhelmingly the cause
  // is a protocol mismatch: the launcher a tool was packaged with, and the daemon it starts,
  // carry different `ethereal-launcher` schema signatures, so the daemon refuses the very first
  // document and closes the connection, leaving `pid` with nothing to print.
  case class Error(tool: Path on Linux)(using Diagnostics)
  extends fulminate.Error(347, 0)
    (m"""
      the tool $tool did not report a process ID; its launcher and its daemon may disagree on the
      launcher protocol schema, in which case the `xek` version pinned in `etc/xek.tsv` needs to
      be one whose runner carries the signature in `ethereal.Launcher`
    """)

  // A `Tool` is a *capability*: it references a live installed daemon process whose lifetime
  // is the `sandbox` block that spawns it (killed, and its files deleted, after the block).
  // A shared capability: the built tool is a descriptor (its path and the daemon's pid) that
  // every tmux session of a suite drives at once — captured by each session's action and passed
  // to the loan that runs it.
  case class Tool(path: Path on Linux, pid: Pid) extends caps.SharedCapability:
    def command: Text = path.name

    def completions(using Monitor, Environment)[result](block: => Unit): Optional[Text] =
      val promise = Promise[Text]()

      async:
        promise.offer(safely(sh"$path '{admin}' await".exec[Text]()).or(t"failed"))

      block
      safely(promise.await())

  // The published `xek` builder, which also signs, resolved from `$XEK`, else `dist/xek` under
  // the working directory, where `make xek-fetch` puts the one pinned in `etc/xek.tsv`.
  private def xek(using Environment): Text = safely(Environment.xek[Text]).or(t"dist/xek")

  // The JVM's own environment, but with `XEK` naming `builder`, for a suite which knows where its
  // builder is: run inside a daemon, whose environment is sanitized and whose working directory
  // is `/`, a suite can rely on neither `$XEK` nor `dist/xek`.
  def environment(builder: Text): Environment = new Environment:
    private val base: Environment = environments.javaBaseEnvironment

    def variable(name: Text): Optional[Text] =
      if name == t"XEK" then builder else base.variable(name)

    override def entries: Optional[Map[Text, Text]] = base.entries.let(_ + Map(t"XEK" -> builder))

  // Generates an ML-DSA-44 key pair with `xek keygen`, as `<prefix>.seed` and `<prefix>.pub`,
  // returning the seed, to sign with, and the public key, to build an `Enclave` with. Signing
  // needs Java 24 or later.
  def keygen(prefix: Path on Linux)(using Environment)
  :   (Path on Linux, Path on Linux) raises Exec.Error raises Path.Error =

    sh"$xek keygen --out $prefix".exec[Exit]() match
      case Exit.Ok =>
        (t"${prefix.encode}.seed".as[Path on Linux], t"${prefix.encode}.pub".as[Path on Linux])

      case Exit.Fail(status) =>
        panic(m"xek could not generate a key pair at $prefix (exit status $status)")

  case class Launcher(path: Path on Linux):
    // Signs this executable with the key whose seed is `seed`, writing the signed copy to
    // `out`. `foreign` allows a key the executable's record does not carry, which a launcher
    // will then reject.
    def sign(seed: Path on Linux, out: Path on Linux, foreign: Boolean = false)
      ( using Environment )
    :   Path on Linux raises Exec.Error =

      val options: List[Text] = if foreign then List(t"--foreign-key") else Nil

      sh"$xek sign --key $seed --in $path --out $out $options".exec[Exit]() match
        case Exit.Ok           => out
        case Exit.Fail(status) => panic(m"xek could not sign $path (exit status $status)")

    // Explicit `using` evidence instead of `raises` sugar: a context-function result would
    // hide the `block` parameter, which the separation checker rejects.
    def sandbox[result](block: (tool: Tool) ?=> result)
      ( using Environment,
              Tactic[Enclave.Error],
              Tactic[Exec.Error],
              Tactic[Number.Error],
              Tactic[Path.Error] )
    :   result =

      val completionScripts = sh"$path '{admin}' install".exec[Text]()

      // `finally`, not `also`: `install` has already started a daemon by this point, so every
      // exit from here — including an abort before `block` is ever reached — must kill it. An
      // abandoned daemon holds a JVM for the rest of the session, and enough of them starve the
      // machine of the processes later suites need to fork.
      try
        val reported = sh"$path '{admin}' pid".exec[Text]().trim
        if reported == t"" then abort(Enclave.Error(path))
        block(using Tool(path, Pid(reported.as[Int])))

      finally
        safely(sh"$path '{admin}' kill".exec[Exit]())

        // Parsed under `safely`, like the deletion: an `install` that printed nothing (as when the
        // launcher and daemon disagree on the protocol) must not raise here, over the error that
        // is already propagating.
        completionScripts.trim.lines.each: line =>
          safely(line.as[Path on Linux]).let: (item: Path on Linux) =>
            safely(item.delete())


// `releaseKey`, `recoveryKey` and `appId` are baked into the executable's record, so that its
// launcher accepts a signed upgrade (xek's `spec/ethrcfg.md`); without a release key and an
// application id, it refuses every one.
case class Enclave
  ( name:        Text,
    buildId:     Optional[Long]          = Unset,
    releaseKey:  Optional[Path on Linux] = Unset,
    recoveryKey: Optional[Path on Linux] = Unset,
    appId:       Optional[Text]          = Unset )
  ( using Classloader, Environment )
extends Rig:
  type Result[output] = Enclave.Launcher
  type Form = Text
  type Target = Path on Linux
  type Transport = Json

  def stage(out: Path on Linux): Path on Linux =
    // Children of the per-stage `out` directory (not `peer` siblings): each staged build's
    // artifacts must stay in its own directory, or two builds of the same tool name — an
    // upgrade test's v1 and v2 — would collide at the same path.
    val target = unsafely(out / name)

    unsafely:
      // `Fqcn.apply` rather than the `fqcn""` interpolator: the macro's synthesized tree
      // fails capture-variable unification when expanded in a capture-checked module.
      val executor: Fqcn =
        safely(Fqcn(t"superlunary.Executor2"))
        . or(panic(m"the constant fully-qualified class name is well-formed"))

      val jarfile = supervise:
        unsafely:
          Toolchain(jarEdges()).produce
            ( Deliverable.Emission(out, Bundler.applicationClasspath),
              Universe.Classfile,
              Jar,
              out,
              List(jarOptions.name(t"$name.jar")),
              List(EntryPoint(executor)) )

      // Package the staged jar into an executable for this platform with the published `xek`
      // builder (spec lives in the `propensive/xek` repo), which fetches the runner stub it
      // needs.
      val build: List[Text] = buildId.lay(Nil): id => List(t"--build-id", id.show)
      val release: List[Text] = releaseKey.lay(Nil): key => List(t"--public-key", key.encode)
      val recovery: List[Text] = recoveryKey.lay(Nil): key => List(t"--recovery-key", key.encode)
      val application: List[Text] = appId.lay(Nil): id => List(t"--app-id", id)
      val options: List[Text] = build + release + recovery + application

      sh"${Enclave.xek} build $options $jarfile $target".exec[Exit]() match
        case Exit.Ok         => target
        case Exit.Fail(fail) => panic(m"xek could not package $target (exit status $fail)")

  protected val scalac: Scalac[3.8, Universe.Classfile] = Scalac(List(scalacOptions.experimental))


  protected def invoke[output](stage: Stage[output, Text, Path on Linux])
  :   Enclave.Launcher =

    stage.remote: input =>
      unsafely:
        variables(inputParameters = input):
          sh"${stage.target}".exec[Exit]()

      t"""[""]"""

    Enclave.Launcher(stage.target)
