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
package embarcadero

import anticipation.*
import aperture.*
import contingency.*
import telekinesis.*
import gossamer.*
import locomotion.*
import obligatory.*
import parasite.*
import proscenium.compat.*
import rudiments.*
import scala.caps
import vacuous.*

object Workload:
  // Anchored here so `container.open[Workload](...)` resolves with no import.
  given openable: (containerd: Containerd^, monitor: Monitor^)
  =>  ( grpcTactic: Tactic[Grpc.Error], http2Tactic: Tactic[Http2.Error],
        asyncTactic: Tactic[Async.Error], protobufTactic: Tactic[Protobuf.Error] )
  =>  ( Openable^{containerd, monitor, grpcTactic, http2Tactic, asyncTactic,
        protobufTactic} ) =
    Openable()

  // WorkloadGrant/WorkloadHandle/WorkloadOpenable → Workload.Grant/Handle/Openable
  // Grants particular to container workloads, in the deliberately-open `Grant` hierarchy:
  // `Launch` means the task is started when opened, and confers awaiting its exit; `Signal`
  // confers sending signals to it.
  object Grant:
    trait Launch extends aperture.Grant
    trait Signal extends aperture.Grant

  object Handle:
    // Operations are extensions gated by the grant-refined handle types rather than
    // typeclass givens: summoning a typeclass on a *scoped* capability's refined type can
    // fail under capture checking, while extensions resolve directly.
    extension (handle: (Workload.Handle & Granting[aperture.Grant.Read])^)
      def state()
      ( using Monitor^ )
      ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
      :   ProcessStatus =

        handle.containerd.task(handle.containerId).state

    extension (handle: (Workload.Handle & Granting[Grant.Launch])^)
      // `await`, not `wait`: the latter would clash with `Object#wait`.
      def await()
      ( using Monitor^ )
      ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
      :   WaitResponse =

        handle.containerd.waitTask(handle.containerId)

    extension (handle: (Workload.Handle & Granting[Grant.Signal])^)
      def kill(signal: Int, all: Boolean = false)
      ( using Monitor^ )
      ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
      :   Unit =

        handle.containerd.killTask(handle.containerId, signal, all = all)

  // A container task scoped to an `open` block: the container metadata and its task exist
  // exactly for the block's duration, and are torn down — killed if started, reaped, and
  // deleted — on every exit path. The lifetime of the workload *is* the scope.
  class Handle private[embarcadero]
    ( private[embarcadero] val containerd: Containerd^,
      val containerId: Text,
      val pid: Int )
  extends caps.ExclusiveCapability

  // A named class rather than an anonymous instance in the `given`, because instantiating an
  // anonymous subclass freshens the capability field types in the inferred `Result` member,
  // which then fails to conform to the declared refinement.
  class Openable
    ( using containerd:    Containerd^,
            monitor:       Monitor^,
            grpcError:     Tactic[Grpc.Error],
            http2Error:    Tactic[Http2.Error],
            asyncError:    Tactic[Async.Error],
            protobufError: Tactic[Protobuf.Error] )
  extends aperture.Openable:

    type Self = Container
    type Form = Workload
    type Operand = Mount
    type Result = Workload.Handle

    def open[grants <: aperture.Grant, result]
      ( value: Container, mode: Mode granting grants, flags: List[Mount] )
      ( block: ((Workload.Handle & Granting[grants])^) ?=> result )
    :   result =

      val created = containerd.createContainer(value)

      try
        val response = containerd.createTask(created.id, flags)
        val started = mode.atoms.has(Launch)
        val pid = if started then containerd.startTask(created.id) else response.pid

        try block(using new Workload.Handle(containerd, created.id, pid) with Granting[grants] {})
        finally
          // Teardown is best-effort: a failure here must not mask the block's own result
          // or error. SIGKILL, because scope end is unconditional; a started task is then
          // reaped before deletion, since containerd refuses to delete an unreaped task.
          if started then
            scala.caps.unsafe.unsafeAssumeSeparate(safely(containerd.killTask(created.id, 9, all = true)))
            scala.caps.unsafe.unsafeAssumeSeparate(safely(containerd.waitTask(created.id)))

          scala.caps.unsafe.unsafeAssumeSeparate(safely(containerd.deleteTask(created.id)))
      finally scala.caps.unsafe.unsafeAssumeSeparate(safely(containerd.deleteContainer(created.id)))

// A task's process state (`containerd.v1.types.Workload`, a subset): which container and
// exec it belongs to, its `pid`, the raw `status` code, and the exit status/time once
// it has stopped. `state` decodes the status code to the `ProcessStatus` enum.
case class Workload
  ( @field(1)  containerId: Text      = t"",
    @field(2)  id:          Text      = t"",
    @field(3)  pid:         Int       = 0,
    @field(4)  status:      Int       = 0,
    @field(9)  exitStatus:  Int       = 0,
    @field(10) exitedAt:    Timestamp = Timestamp() )
derives CanEqual:

  def state: ProcessStatus = ProcessStatus.of(status).or(ProcessStatus.Unknown)
