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
package embarcadero

import scala.caps

import proscenium.compat.*

import anticipation.*
import aperture.*
import contingency.*
import cordillera.*
import locomotion.*
import obligatory.*
import parasite.*

// Grants particular to container workloads, in the deliberately-open `Grant` hierarchy:
// `Run` means the task is started when opened, and confers awaiting its exit; `Signal`
// confers sending signals to it.
object WorkloadGrant:
  trait Run extends Grant
  trait Signal extends Grant

object WorkloadHandle:
  // Operations are extensions gated by the grant-refined handle types rather than
  // typeclass givens: summoning a typeclass on a *scoped* capability's refined type can
  // fail under capture checking, while extensions resolve directly.
  extension (handle: (WorkloadHandle & Granting[Grant.Read])^)
    def state()
    ( using Monitor^ )
    ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
    :   ProcessStatus =

      handle.containerd.task(handle.containerId).state

  extension (handle: (WorkloadHandle & Granting[WorkloadGrant.Run])^)
    // `await`, not `wait`: the latter would clash with `Object#wait`.
    def await()
    ( using Monitor^ )
    ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
    :   WaitResponse =

      handle.containerd.waitTask(handle.containerId)

  extension (handle: (WorkloadHandle & Granting[WorkloadGrant.Signal])^)
    def kill(signal: Int, all: Boolean = false)
    ( using Monitor^ )
    ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
    :   Unit =

      handle.containerd.killTask(handle.containerId, signal, all = all)

// A container task scoped to an `open` block: the container metadata and its task exist
// exactly for the block's duration, and are torn down — killed if started, reaped, and
// deleted — on every exit path. The lifetime of the workload *is* the scope.
class WorkloadHandle private[embarcadero]
  ( private[embarcadero] val containerd: Containerd^,
    val containerId: Text,
    val pid: Int )
extends caps.ExclusiveCapability

// A named class rather than an anonymous instance in the `given`, because instantiating an
// anonymous subclass freshens the capability field types in the inferred `Result` member,
// which then fails to conform to the declared refinement.
class WorkloadOpenable
  ( using containerd:    Containerd^,
          monitor:       Monitor^,
          grpcError:     Tactic[GrpcError],
          http2Error:    Tactic[Http2Error],
          asyncError:    Tactic[AsyncError],
          protobufError: Tactic[ProtobufError] )
extends Openable:

  type Self = Container
  type Form = Workload
  type Operand = Mount
  type Result = WorkloadHandle

  def open[grants <: Grant, result]
    ( value: Container, mode: Mode granting grants, flags: List[Mount] )
    ( block: ((WorkloadHandle & Granting[grants])^) ?=> result )
  :   result =

    val created = containerd.createContainer(value)

    try
      val response = containerd.createTask(created.id, flags)
      val started = mode.atoms.contains(Run)
      val pid = if started then containerd.startTask(created.id) else response.pid

      try block(using new WorkloadHandle(containerd, created.id, pid) with Granting[grants] {})
      finally
        // Teardown is best-effort: a failure here must not mask the block's own result
        // or error. SIGKILL, because scope end is unconditional; a started task is then
        // reaped before deletion, since containerd refuses to delete an unreaped task.
        if started then
          safely(containerd.killTask(created.id, 9, all = true))
          safely(containerd.waitTask(created.id))

        safely(containerd.deleteTask(created.id))
    finally safely(containerd.deleteContainer(created.id))
