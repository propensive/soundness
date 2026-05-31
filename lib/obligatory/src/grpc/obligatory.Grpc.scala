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
package obligatory

import anticipation.*
import fulminate.*
import gossamer.*
import vacuous.*

// The gRPC vocabulary. Grouped under `Grpc` so its generic names — `Status`,
// `Method`, `Metadata` — don't crowd the top-level namespace, mirroring how
// `telekinesis` groups its types under `Http` and `cordillera` under `Http2`.
object Grpc:
  object Status:
    given communicable: Status is Communicable = status => m"${status.toString.tt}"

    // The canonical status code (RFC: grpc-status), or `Unset` if unrecognised.
    def of(code: Int): Optional[Status] = values.find(_.code == code).optional

  // The seventeen canonical gRPC status codes, carried in the `grpc-status`
  // trailer of a response. `Ok` (0) signifies success; the rest are failures.
  enum Status(val code: Int):
    case Ok                 extends Status(0)
    case Cancelled          extends Status(1)
    case Unknown            extends Status(2)
    case InvalidArgument    extends Status(3)
    case DeadlineExceeded   extends Status(4)
    case NotFound           extends Status(5)
    case AlreadyExists      extends Status(6)
    case PermissionDenied   extends Status(7)
    case ResourceExhausted  extends Status(8)
    case FailedPrecondition extends Status(9)
    case Aborted            extends Status(10)
    case OutOfRange         extends Status(11)
    case Unimplemented      extends Status(12)
    case Internal           extends Status(13)
    case Unavailable        extends Status(14)
    case DataLoss           extends Status(15)
    case Unauthenticated    extends Status(16)

  // A fully-qualified gRPC method, addressed on the wire as the HTTP/2 `:path`
  // `/<package>.<Service>/<Method>`, e.g. `/grpc.health.v1.Health/Check`.
  case class Method(service: Text, rpc: Text):
    def path: Text = t"/$service/$rpc"

  // Custom call metadata: arbitrary key/value pairs sent as HTTP/2 headers
  // alongside the gRPC pseudo-headers (e.g. containerd's `containerd-namespace`).
  case class Metadata(entries: List[(Text, Text)] = Nil)

  // Derive a client stub for a service interface of `@rpc`-annotated methods over the
  // given channel; `service` is the fully-qualified proto service name (the package
  // and `Service`), and each method's own name completes the `:path`.
  inline def remote[interface](channel: GrpcChannel, service: Text): interface =
    ${grpcInternal.remote[interface]('channel, 'service)}
