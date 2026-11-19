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
import aperture.*
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
import nomenclature.*
import parasite.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import superlunary.*
import symbolism.*
import vacuous.*
import logging.silentLogging
import probates.cancelProbate
import systems.javaBaseSystem
import temporaryDirectories.systemTemporaryDirectory
import threads.platformThreads
import workingDirectories.javaBaseWorkingDirectory
import filesystemBackends.javaBaseFilesystem

object Enclave:
  // Raised when the daemon does not answer the `{admin} pid` request. Overwhelmingly the cause
  // is a protocol mismatch: the launcher a tool was packaged with, and the daemon it starts,
  // carry different `ethereal-launcher` schema signatures, so the daemon refuses the very first
  // document and closes the connection, leaving `pid` with nothing to print.
  case class Error(tool: Path on Local)(using Diagnostics)
  extends fulminate.Error(109, 2, 0)
    ( m"""
      the tool $tool did not report a process ID; its launcher and its daemon may disagree on the
      launcher protocol schema, in which case the `xek` version pinned in `etc/xek.tsv` needs to
      be one whose runner carries the signature in `ethereal.Launcher`
    """ )

  // A `Tool` is a *capability*: it references a live installed daemon process whose lifetime
  // is the `sandbox` block that spawns it (killed, and its files deleted, after the block).
  // A shared capability: the built tool is a descriptor (its path and the daemon's pid) that
  // every pane of a suite drives at once — captured by each pane's action and passed to the loan
  // that runs it. `home` is the scratch home its daemon and its panes' shells run under.
  // `runtime` is the tool's `XDG_RUNTIME_DIR`, where its daemon's socket is.
  case class Tool
    ( path: Path on Local, pid: Pid, home: Path on Local, runtime: Optional[Path on Local] = Unset )
  extends anticipation.Durable:
    def command: Text = path.name

    def completions(using Monitor, Environment)[result](block: => Unit): Optional[Text] =
      val promise = Promise[Text]()

      async:
        promise.offer(safely(sh"$path '{admin}' await".exec[Text]()).or(t"failed"))

      block
      safely(promise.await())

  // The variables which locate a user's configuration — their home, their XDG directories, and
  // bash-completion's own — all directed into `home`, and, given `runtime`, the directory a
  // tool's daemon keeps its socket in. A tool installed under these, and the shells its tests
  // start under them, read and write nothing of the real user's configuration, and meet no
  // daemon of the real user's.
  def scratch(home: Path on Local, runtime: Optional[Path on Local] = Unset): Map[Text, Text] =
    val configuration: Map[Text, Text] =
      Map
        ( t"HOME"                     -> home.encode,
          t"XDG_CONFIG_HOME"          -> t"${home.encode}/.config",
          t"XDG_DATA_HOME"            -> t"${home.encode}/.local/share",
          t"BASH_COMPLETION_USER_DIR" -> t"${home.encode}/.local/share/bash-completion" )

    runtime.lay(configuration): runtime =>
      configuration + Map(t"XDG_RUNTIME_DIR" -> runtime.encode)

  // `base`, with the variables `scratch` directs into `home` and `runtime`.
  def environment(base: Environment, home: Path on Local, runtime: Optional[Path on Local] = Unset)
  :   Environment = new Environment:
    private val overrides = scratch(home, runtime)

    def variable(name: Text): Optional[Text] = overrides.at(name).or(base.variable(name))
    override def entries: Optional[Map[Text, Text]] = base.entries.let(_ + overrides)

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
  def keygen(prefix: Path on Local)(using Environment)
  :   (Path on Local, Path on Local) raises Exec.Error raises Path.Error =

    sh"$xek keygen --out $prefix".exec[Exit]() match
      case Exit.Ok =>
        (t"${prefix.encode}.seed".as[Path on Local], t"${prefix.encode}.pub".as[Path on Local])

      case Exit.Fail(status) =>
        panic(m"xek could not generate a key pair at $prefix (exit status $status)")

  case class Launcher(path: Path on Local):
    // Signs this executable with the key whose seed is `seed`, writing the signed copy to
    // `out`. `foreign` allows a key the executable's record does not carry, which a launcher
    // will then reject.
    def sign(seed: Path on Local, out: Path on Local, foreign: Boolean = false)
      ( using Environment )
    :   Path on Local raises Exec.Error =

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
              Tactic[Path.Error],
              Tactic[Io.Error] )
    :   result =

      // The daemon, and the shells the panes start, run under a scratch home rather than the
      // user's, deleted when the block ends. The tool is not `install`ed: installation writes
      // completion scripts into the configuration of whatever home the daemon sees — and
      // appends to a PowerShell profile — so the panes write the scripts they need into the
      // scratch home themselves.
      temporaryDirectory[Path on Local].open[Scratch](Read & Write): scratch ?=>
        val home: Path on Local = scratch.stem

        // The daemon's socket is in a runtime directory of its own, beneath `/tmp`: beneath the
        // home, or the temporary directory a forked suite is given, its path can exceed the 104
        // bytes macOS allows a socket's.
        (% / "tmp").on[Local].open[Scratch](Read & Write): runtimeScratch ?=>
          val runtime: Path on Local = runtimeScratch.stem

          given Environment = Enclave.environment(summon[Environment], home, runtime)

          // `finally`, not `also`: asking for the daemon's pid starts it, so every exit from here
          // must kill it — before its home is deleted. An abandoned daemon holds a JVM for the
          // rest of the session, and enough of them starve the machine of the processes later
          // suites need to fork.
          try
            val reported = sh"$path '{admin}' pid".exec[Text]().trim
            if reported == t"" then abort(Enclave.Error(path))
            block(using Tool(path, Pid(reported.as[Int]), home, runtime))

          finally safely(sh"$path '{admin}' kill".exec[Exit]())


// `releaseKey`, `recoveryKey` and `appId` are baked into the executable's record, so that its
// launcher accepts a signed upgrade (xek's `spec/ethrcfg.md`); without a release key and an
// application id, it refuses every one.
case class Enclave
  ( name:        Text,
    buildId:     Optional[Long]          = Unset,
    releaseKey:  Optional[Path on Local] = Unset,
    recoveryKey: Optional[Path on Local] = Unset,
    appId:       Optional[Text]          = Unset )
  ( using Classloader, Environment )
extends Rig uses parasite.threads:
  type Result[output] = Enclave.Launcher
  type Form = Text
  type Target = Path on Local
  type Transport = Json

  def stage(out: Path on Local)(using Tactic[Rig.Error]): Path on Local =
    mitigate:
      case error: Name.Error  => Rig.Error(Rig.Error.Reason.Staging)(using error.diagnostics)
      case error: Link.Error  => Rig.Error(Rig.Error.Reason.Staging)(using error.diagnostics)
      case error: Async.Error => Rig.Error(Rig.Error.Reason.Staging)(using error.diagnostics)
      case error: Exec.Error  => Rig.Error(Rig.Error.Reason.Staging)(using error.diagnostics)

    . protect:
      // Children of the per-stage `out` directory (not `peer` siblings): each staged build's
      // artifacts must stay in its own directory, or two builds of the same tool name — an
      // upgrade test's v1 and v2 — would collide at the same path.
      val target: Path on Local = out / name

      // `Fqcn.apply` rather than the `fqcn""` interpolator: the macro's synthesized tree
      // fails capture-variable unification when expanded in a capture-checked module.
      val executor: Fqcn =
        safely(Fqcn(t"superlunary.Executor2"))
        . or(panic(m"the constant fully-qualified class name is well-formed"))

      val jarfile = supervise:
        Toolchain(jarEdges()).produce
          ( Deliverable.Emission(out, Bundler.applicationClasspath),
            Toolchain.Universe.Classfile,
            Artifact.Jar,
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

  protected val scalac: Scalac[3.8, Toolchain.Universe.Classfile] = Scalac(List(scalacOptions.experimental))


  protected def invoke[output](stage: Stage[output, Text, Path on Local])(using Tactic[Rig.Error])
  :   Enclave.Launcher =

    stage.remote: input =>
      mitigate:
        case error: Exec.Error => Rig.Error(Rig.Error.Reason.Invocation)(using error.diagnostics)

      . protect:
        variables(inputParameters = input):
          sh"${stage.target}".exec[Exit]()

      t"""[""]"""

    Enclave.Launcher(stage.target)
