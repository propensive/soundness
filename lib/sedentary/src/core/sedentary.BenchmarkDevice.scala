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
package sedentary

import ambience.*
import anticipation.*
import aperture.*
import contingency.*
import eucalyptus.*
import galilei.*
import gossamer.*
import guillotine.*
import inimitable.*
import prepositional.*
import rudiments.*
import serpentine.*
import urticose.*
import vacuous.*

import logging.silentLogging
import workingDirectories.javaWorkingDirectory
import beneficence.*

trait BenchmarkDevice extends Findable:
  def deploy(path: Path on Linux, uuid: Uuid): Unit raises Bench.Error

  // `heap` overrides the measurement JVM's fixed heap size (both `-Xms` and `-Xmx`), in
  // `-Xmx` syntax, e.g. `t"256m"`; when unset, the default of `1g` applies. Constrained-heap
  // stress tests use this to demonstrate what a workload sustains in a small, pinned heap.
  //
  // `cpus` limits the measurement JVM to that many processors. Everywhere it sets
  // `-XX:ActiveProcessorCount`, which sizes GC/JIT threads and every pool derived from
  // `Runtime.availableProcessors`; on a Linux device the process is additionally pinned to
  // that many cores with `taskset`. Note that without OS-level pinning (macOS localhost),
  // raw platform threads still schedule across all cores, so the limit is advisory there —
  // for hard CPU isolation, run against a Linux `NetworkDevice`.
  //
  // `gc` names the measurement JVM's collector, in `-XX:+Use<gc>GC` syntax (`t"G1"`,
  // `t"Parallel"`, `t"Z"`); when unset, `Serial` applies. Serial's single-threaded,
  // deterministic pauses suit microbenchmarks, but its stop-the-world pauses dominate tail
  // latency in saturated multi-threaded workloads, which should select `G1`.
  def invoke
    ( path: Path on Linux, input: Text, heap: Optional[Text] = Unset,
      cpus: Optional[Int] = Unset, gc: Optional[Text] = Unset )
  :   Text raises Bench.Error

  def undeploy(path: Path on Linux, uuid: Uuid): Unit raises Bench.Error

object NetworkDevice:
  given sessional: (error: Tactic[Bench.Error]) => NetworkDeviceSessional =
    NetworkDeviceSessional()

  // The measurement JVM's invocation, shared by the per-call and session forms; see
  // `BenchmarkDevice.invoke` for the meaning of `heap`, `cpus` and `gc`.
  private[sedentary] def measurement
    ( path: Path on Linux, input: Text, heap: Optional[Text], cpus: Optional[Int],
      gc: Optional[Text] )
  :   Command =

    val size = heap.or(t"1g")
    val collector = gc.or(t"Serial")

    val pin = cpus.lay(List[Text]()): n => List(t"taskset", t"-c", t"0-${n - 1}")

    val processors = cpus.lay(List[Text]()): n => List(t"-XX:ActiveProcessorCount=$n")

    sh"""
      $pin
      java
        -XX:+AlwaysPreTouch
        -Xms$size
        -Xmx$size
        -XX:CICompilerCount=2
        -XX:+Use${collector}GC
        $processors
        -jar ${path.name}
        '$input'
        2> /dev/null
    """

  // A benchmark device multiplexing every operation over one OpenSSH ControlMaster
  // connection (`device.session`), so `deploy`, `invoke` and `undeploy` skip the per-call
  // SSH handshake the plain `NetworkDevice` pays.
  class Session private[sedentary] (device: NetworkDevice, socket: Text)
  extends BenchmarkDevice:
    def deploy(path: Path on Linux, uuid: Uuid): Unit raises Bench.Error =
      val user = device.user
      val host = device.host

      safely(sh"scp -o ControlPath=$socket $path $user@$host:$uuid.jar".exec[Exit]())
      . lest(Bench.Error())

    def invoke
      ( path: Path on Linux, input: Text, heap: Optional[Text] = Unset,
        cpus: Optional[Int] = Unset, gc: Optional[Text] = Unset )
    :   Text raises Bench.Error =

      val user = device.user
      val host = device.host
      val command = NetworkDevice.measurement(path, input, heap, cpus, gc)

      safely(sh"""ssh -o ControlPath=$socket $user@$host ${command.escape}""".exec[Text]())
      . lest(Bench.Error())

    def undeploy(path: Path on Linux, uuid: Uuid): Unit raises Bench.Error =
      val user = device.user
      val host = device.host

      safely(sh"ssh -o ControlPath=$socket $user@$host rm $path".exec[Text]())
      . lest(Bench.Error())

class NetworkDevice(val user: Text, val host: Hostname) extends BenchmarkDevice:
  def deploy(path: Path on Linux, uuid: Uuid): Unit raises Bench.Error =
    val user = this.user
    val host = this.host
    safely(sh"scp $path $user@$host:$uuid.jar".exec[Exit]()).lest(Bench.Error())

  def invoke
    ( path: Path on Linux, input: Text, heap: Optional[Text] = Unset,
      cpus: Optional[Int] = Unset, gc: Optional[Text] = Unset )
  :   Text raises Bench.Error =

    val user = this.user
    val host = this.host
    val command = NetworkDevice.measurement(path, input, heap, cpus, gc)

    safely(sh"""ssh $user@$host ${command.escape}""".exec[Text]()).lest(Bench.Error())

  def undeploy(path: Path on Linux, uuid: Uuid): Unit raises Bench.Error =
    val user = this.user
    val host = this.host
    safely(sh"ssh $user@$host rm $path".exec[Text]()).lest(Bench.Error())

// A scoped session on a `NetworkDevice`: an OpenSSH ControlMaster connection is opened
// (and authenticated) once, the multiplexing `Session` device is lent to the lambda, and
// the master connection is closed when the scope ends. A named instance class rather than
// an anonymous given, matching the capture-checked instances elsewhere; this module is
// not yet capture-checked, so the handle carries no capability annotations here.
class NetworkDeviceSessional(using error: Tactic[Bench.Error]) extends Sessional:
  type Self = NetworkDevice
  type Result = NetworkDevice.Session

  def session[result](target: NetworkDevice)(lambda: (session: Result) ?=> result): result =
    val user = target.user
    val host = target.host
    val socket: Text = t"/tmp/sedentary-${Uuid()}.ssh"

    safely(sh"ssh -M -N -f -o ControlPath=$socket $user@$host".exec[Exit]())
    . lest(Bench.Error())

    try lambda(using NetworkDevice.Session(target, socket))
    finally safely(sh"ssh -O exit -o ControlPath=$socket $user@$host".exec[Exit]()).let(_ => ())

object LocalhostDevice extends BenchmarkDevice:
  def deploy(path: Path on Linux, uuid: Uuid): Unit raises Bench.Error = ()

  def invoke
    ( path: Path on Linux, input: Text, heap: Optional[Text] = Unset,
      cpus: Optional[Int] = Unset, gc: Optional[Text] = Unset )
  :   Text raises Bench.Error =

    val size = heap.or(t"1g")
    val collector = gc.or(t"Serial")

    val processors = cpus.lay(List[Text]()): n => List(t"-XX:ActiveProcessorCount=$n")

    val opts =
      sh"-XX:+AlwaysPreTouch -Xms$size -Xmx$size -XX:CICompilerCount=2 -XX:+Use${collector}GC"
    val cmd = sh"java $opts $processors -jar $path $input"
    safely(cmd.exec[Text]()).lest(Bench.Error())

  def undeploy(path: Path on Linux, uuid: Uuid): Unit raises Bench.Error = ()
