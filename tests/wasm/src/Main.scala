package e2e

import scala.scalajs.wit
import scala.scalajs.wit.annotation.*
import componentmodel.exports.wasi.cli.Run

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

import strategies.throwUnsafely

// The wasm e2e component `etc/ci/wasm-e2e.sh` drives under wasmtime. One reachable `@WitExport`
// keeps the whole surface out of DCE: every scenario below goes through one `.wasi` backend's
// deferred `invoke` machinery, so linking this component (and running each scenario) regression-
// tests the WIT ABI of all seven backends. The scenario is selected with `--env SCENARIO=…`,
// which itself exercises ambience's environment backend.
@WitImplementation
object Main extends Run:

  private def scenarioEnv(): ambience.Environment =
    import ambience.wasiEnvironmentApi
    import ambience.environments.wasiEnvironment
    summon[ambience.Environment]

  private def stdio(scenario: Text): Unit =
    import turbulence.{Stdio, wasiCliApi}
    import turbulence.stdios.wasiStdio
    import anticipation.termcapDefinitions.basicTermcap

    val io = summon[Stdio]
    val buffer = new Array[Byte](256)
    val count = io.in.read(buffer, 0, buffer.length)
    val received = if count <= 0 then "<no input>" else new String(buffer, 0, count, "UTF-8").trim.nn
    io.print(("echo: " + received + "\n").tt)
    io.printErr(("stderr: " + received + "\n").tt)

  private def clock(): Unit =
    import aviation.{monotonic, wasiClockApi}
    import aviation.clocks.wasiMonotonicClock

    val first = monotonic()
    val second = monotonic()
    System.out.nn.println("clock: ok")

  private def random(): Unit =
    import capricious.{arbitrary, stochastic, wasiRandomApi}
    import capricious.randomization.wasiRandomization

    stochastic:
      val first = arbitrary[Long]()
      val second = arbitrary[Long]()
      System.out.nn.println(if first != second then "random: ok" else "random: suspicious")

  private def fs(): Unit =
    import galilei.{FilesystemBackend, Linux, OpenFlag, wasiFilesystemApi}
    import galilei.filesystemBackends.wasi
    import serpentine.*

    val backend: FilesystemBackend on Linux = galilei.filesystemBackends.wasi[Linux]

    def path(parts: Text*): Path on Linux = Path[Linux, Text, Tuple](t"/", proscenium.List(parts.reverse*))

    backend.createDirectory(path(t"work", t"sub"))
    val file = path(t"work", t"sub", t"probe.txt")
    val payload: Data = "wasm e2e probe".getBytes("UTF-8").nn.immutable(using Unsafe)

    backend.open(file, proscenium.List(OpenFlag.Write, OpenFlag.Create)): handle =>
      handle.writer(proscenium.Progression(payload))

    val content: Text = backend.open(file, proscenium.List(OpenFlag.Read)): handle =>
      handle.reader().map(_.utf8).join

    System.out.nn.println("fs: " + content.s)

  private def tcp(environment: ambience.Environment): Unit =
    import coaxial.{SocketBackend, wasiSocketsApi}
    import coaxial.socketBackends.wasi
    import urticose.{Endpoint, Port, Tcp}

    val port = environment.variable(t"PORT").or(t"9099").s.toInt
    val backend = summon[SocketBackend]
    val endpoint = Endpoint("127.0.0.1".tt, Port.unsafe[Tcp](port))

    val exchange = backend.dialTcp(endpoint, Unset, proscenium.Nil)
    System.out.nn.println("tcp: connected")
    backend.hangUp(exchange)

  private def http(environment: ambience.Environment): Unit =
    import telekinesis.{Http, wasiHttpApi}
    import telekinesis.httpBackends.wasi
    import zephyrine.Stream

    val url = environment.variable(t"URL").or(t"http://example.com/")
    val backend = summon[Http.Backend]
    val response = backend.request(url, Http.Get, proscenium.Nil, () => Stream(Iterator.empty[anticipation.Data]))
    System.out.nn.println("http: " + response.status.code)

  @WitExport("wasi:cli/run@0.2.0", "run")
  def run(): wit.Result[Unit, Unit] =
    try
      val environment = scenarioEnv()
      val scenario = environment.variable(t"SCENARIO").or(t"none")

      scenario.s match
        case "stdio"  => stdio(scenario)
        case "clock"  => clock()
        case "random" => random()
        case "fs"     => fs()
        case "tcp"    => tcp(environment)
        case "http"   => http(environment)
        case other    => System.out.nn.println("unknown scenario: " + other)

      new wit.Ok(())
    catch
      case throwable: Throwable =>
        System.out.nn.println("EXCEPTION: " + throwable.getClass.getName + ": " + throwable.getMessage)
        throwable.getStackTrace.nn.take(12).foreach { e => System.out.nn.println("  at " + e) }
        new wit.Err(())
