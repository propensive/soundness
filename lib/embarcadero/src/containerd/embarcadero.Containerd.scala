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

import scala.caps

import anticipation.*
import coaxial.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import locomotion.*
import obligatory.*
import parasite.*
import prepositional.*
import telekinesis.*
import vacuous.*

object Containerd:
  // containerd multiplexes every service over the one connection; each method is
  // addressed by its fully-qualified proto path.
  private val versionMethod: Grpc.Method =
    Grpc.Method(t"containerd.services.version.v1.Version", t"Version")

  private val containersService: Text = t"containerd.services.containers.v1.Containers"
  private val listContainersMethod: Grpc.Method = Grpc.Method(containersService, t"List")
  private val getContainerMethod: Grpc.Method = Grpc.Method(containersService, t"Get")
  private val createContainerMethod: Grpc.Method = Grpc.Method(containersService, t"Create")
  private val deleteContainerMethod: Grpc.Method = Grpc.Method(containersService, t"Delete")

  private val namespacesService: Text = t"containerd.services.namespaces.v1.Namespaces"
  private val listNamespacesMethod: Grpc.Method = Grpc.Method(namespacesService, t"List")
  private val createNamespaceMethod: Grpc.Method = Grpc.Method(namespacesService, t"Create")
  private val deleteNamespaceMethod: Grpc.Method = Grpc.Method(namespacesService, t"Delete")

  private val imagesService: Text = t"containerd.services.images.v1.Images"
  private val listImagesMethod: Grpc.Method = Grpc.Method(imagesService, t"List")
  private val getImageMethod: Grpc.Method = Grpc.Method(imagesService, t"Get")
  private val deleteImageMethod: Grpc.Method = Grpc.Method(imagesService, t"Delete")

  private val tasksService: Text = t"containerd.services.tasks.v1.Tasks"
  private val createTaskMethod: Grpc.Method = Grpc.Method(tasksService, t"Create")
  private val startTaskMethod: Grpc.Method = Grpc.Method(tasksService, t"Start")
  private val killTaskMethod: Grpc.Method = Grpc.Method(tasksService, t"Kill")
  private val waitTaskMethod: Grpc.Method = Grpc.Method(tasksService, t"Wait")
  private val deleteTaskMethod: Grpc.Method = Grpc.Method(tasksService, t"Delete")
  private val getTaskMethod: Grpc.Method = Grpc.Method(tasksService, t"Get")
  private val listTasksMethod: Grpc.Method = Grpc.Method(tasksService, t"List")

  // Connect to a containerd endpoint (typically a Unix socket carrying cleartext h2c),
  // binding every call to `namespace` via the mandatory `containerd-namespace` header.
  // Must be called inside a `supervise` scope (the connection runs background daemons).
  def apply[endpoint]
    ( endpoint: Http2.Endpoint[endpoint], namespace: Text )
    ( using monitor:     Monitor,
            probate:     Probate,
            asyncError:  Tactic[Async.Error],
            socketError: Tactic[Socket.Error] )
  :   Containerd^{monitor, caps.any} =

    val metadata = Grpc.Metadata(List(t"containerd-namespace" -> namespace))
    Containerd(Grpc.Channel(endpoint, metadata))

  object AnyMessage:
    opaque type Payload = Array[Byte]^{}

    object Payload:
      val empty: Payload = Array.empty[Byte]

      def apply(data: Data): Payload = data

      given encodable: Payload is Encodable in Protobuf = payload =>
        Protobuf.Wire(WireType.Len, payload)

      given decodable: Payload is Decodable in Protobuf = _.payload

    extension (payload: Payload) def data: Data = payload

  // The protobuf well-known `google.protobuf.Any`: a serialized message tagged with its
  // type URL. containerd uses it to carry opaque payloads — notably a container's OCI
  // runtime spec and a task's runtime options — that this client passes through verbatim.
  // Named `AnyMessage` to avoid `scala.Any`.
  //
  // The payload is a dedicated pure opaque over the stdlib's immutable byte array, not a raw
  // `Data` field: inline protobuf derivation freshens a frozen-array field to an `any.rd`
  // capture that the `Data` codec instances cannot match.
  case class AnyMessage
    ( @field(1) typeUrl: Text               = t"",
      @field(2) value:   AnyMessage.Payload = AnyMessage.Payload.empty )

  // A containerd container metadata record (`containerd.services.containers.v1`). The
  // protobuf field numbers are preserved, so this round-trips with the daemon even
  // though only a subset of fields is modelled — unknown fields are skipped on decode.
  // `spec` carries the OCI runtime spec as an opaque `AnyMessage`; the `extensions`
  // `Any` map (10) is still omitted.
  case class Container
    ( @field(1)  id:          Text,
      @field(2)  labels:      Map[Text, Text] = Map(),
      @field(3)  image:       Text            = t"",
      @field(4)  runtime:     Runtime         = Runtime(),
      @field(5)  spec:        AnyMessage      = AnyMessage(),
      @field(6)  snapshotter: Text            = t"",
      @field(7)  snapshotKey: Text            = t"",
      @field(8)  createdAt:   Timestamp       = Timestamp(),
      @field(9)  updatedAt:   Timestamp       = Timestamp(),
      @field(11) sandbox:     Text            = t"" )
  derives CanEqual

  // containerd's protobuf content descriptor (`containerd.types.Descriptor`): a typed,
  // digest-addressed pointer to a blob. Named `ContentDescriptor` to avoid clashing with
  // the OCI JSON `Descriptor` in this same package; the field numbers match the proto.
  case class ContentDescriptor
    ( @field(1) mediaType:   Text            = t"",
      @field(2) digest:      Text            = t"",
      @field(3) size:        Long            = 0L,
      @field(5) annotations: Map[Text, Text] = Map() )
  derives CanEqual

  // The request for `Containers.Create`: the full container record to register.
  case class CreateContainerRequest(@field(1) container: Container = Container(t""))

  // The reply from `Containers.Create`: the container as stored.
  case class CreateContainerResponse(@field(1) container: Container = Container(t""))

  // The request for `Namespaces.Create`: the namespace to create.
  case class CreateNamespaceRequest(@field(1) namespace: Namespace = Namespace(t""))

  // The reply from `Namespaces.Create`: the created namespace, as stored.
  case class CreateNamespaceResponse(@field(1) namespace: Namespace = Namespace(t""))

  // The request for `Tasks.Create`: a new task for `containerId`, with its root filesystem
  // `rootfs` mounts, optional stdio paths, a `terminal` flag, and opaque runtime `options`.
  case class CreateTaskRequest
    ( @field(1)  containerId: Text        = t"",
      @field(3)  rootfs:      List[Mount] = Nil,
      @field(4)  stdin:       Text        = t"",
      @field(5)  stdout:      Text        = t"",
      @field(6)  stderr:      Text        = t"",
      @field(7)  terminal:    Boolean     = false,
      @field(10) options:     AnyMessage  = AnyMessage() )
  derives CanEqual

  // The reply from `Tasks.Create`: the container id and the host pid of the new task.
  case class CreateTaskResponse(@field(1) containerId: Text = t"", @field(2) pid: Int = 0)
  derives CanEqual

  // The request for `Containers.Delete`: the id of the container to remove.
  case class DeleteContainerRequest(@field(1) id: Text)

  // The request for `Images.Delete`: the image reference to remove. `sync` waits for the
  // content to be garbage-collected before the call returns.
  case class DeleteImageRequest(@field(1) name: Text, @field(2) sync: Boolean = false)

  // The request for `Namespaces.Delete`: the name of the namespace to remove.
  case class DeleteNamespaceRequest(@field(1) name: Text)

  // The request for `Tasks.Delete`: the task (and optional exec process) to remove.
  case class DeleteTaskRequest(@field(1) containerId: Text = t"", @field(2) execId: Text = t"")
  derives CanEqual

  // The reply from `Tasks.Delete`: the removed task's exec `id`, host `pid`, and the
  // `exitStatus`/`exitedAt` it stopped with.
  case class DeleteTaskResponse
    ( @field(1) id:         Text      = t"",
      @field(2) pid:        Int       = 0,
      @field(3) exitStatus: Int       = 0,
      @field(4) exitedAt:   Timestamp = Timestamp() )
  derives CanEqual

  object Event:
    given communicable: Event is Communicable =
      case ContainerCreated(id)          => m"created the container $id"
      case ContainerDeleted(id)          => m"deleted the container $id"
      case ImageDeleted(name)            => m"deleted the image $name"
      case TaskCreated(container)        => m"created a task for container $container"
      case TaskStarted(container, pid)   => m"started the task for container $container as PID $pid"
      case TaskKilled(container, signal) => m"signalled task for container $container with $signal"
      case TaskDeleted(container)        => m"deleted the task for container $container"

  enum Event:
    case ContainerCreated(id: Text) extends Event, Log.Process
    case ContainerDeleted(id: Text) extends Event, Log.Process
    case ImageDeleted(name: Text) extends Event, Log.Resource
    case TaskCreated(container: Text) extends Event, Log.Process
    case TaskStarted(container: Text, pid: Int) extends Event, Log.Process
    case TaskKilled(container: Text, signal: Int) extends Event, Log.Process
    case TaskDeleted(container: Text) extends Event, Log.Process

  // The protobuf well-known `google.protobuf.Empty`: a message with no fields, used as
  // the request type of parameterless containerd calls such as `Version`.
  case class Empty()

  // The request for `Containers.Get`: the id of the container to fetch.
  case class GetContainerRequest(@field(1) id: Text)

  // The reply from `Containers.Get`: the requested container.
  case class GetContainerResponse(@field(1) container: Container = Container(t""))

  // The request for `Images.Get`: the image reference to fetch.
  case class GetImageRequest(@field(1) name: Text)

  // The reply from `Images.Get`: the requested image record.
  case class GetImageResponse(@field(1) image: ImageRecord = ImageRecord())

  // The request for `Tasks.Get`: the task (or exec process) to look up.
  case class GetTaskRequest(@field(1) containerId: Text = t"", @field(2) execId: Text = t"")
  derives CanEqual

  // The reply from `Tasks.Get`: the requested process state.
  case class GetTaskResponse(@field(1) process: Workload = Workload()) derives CanEqual

  // A containerd image metadata record (`containerd.services.images.v1`): a `name`
  // (the image reference) pointing at content via `target`, plus labels and timestamps.
  // Named `ImageRecord` to avoid clashing with the OCI `Image` config in this package.
  case class ImageRecord
    ( @field(1) name:      Text                = t"",
      @field(2) labels:    Map[Text, Text]     = Map(),
      @field(3) target:    ContentDescriptor   = ContentDescriptor(),
      @field(7) createdAt: Timestamp           = Timestamp(),
      @field(8) updatedAt: Timestamp           = Timestamp() )
  derives CanEqual

  // The request for `Tasks.Kill`: send `signal` to the task (or a specific exec process),
  // or to `all` processes in the container.
  case class KillRequest
    ( @field(1) containerId: Text    = t"",
      @field(2) execId:      Text    = t"",
      @field(3) signal:      Int     = 0,
      @field(4) all:         Boolean = false )
  derives CanEqual

  // The request for `Containers.List`: optional `filters` narrow the result (containerd
  // filter expressions, e.g. `labels."tier"==db`); an empty list returns everything.
  case class ListContainersRequest(@field(1) filters: List[Text] = Nil)

  // The reply from `Containers.List`: the matching containers, in containerd's order.
  case class ListContainersResponse(@field(1) containers: List[Container] = Nil)

  // The request for `Images.List`: optional containerd filter expressions.
  case class ListImagesRequest(@field(1) filters: List[Text] = Nil)

  // The reply from `Images.List`: every matching image record.
  case class ListImagesResponse(@field(1) images: List[ImageRecord] = Nil)

  // The request for `Namespaces.List`: an optional containerd filter expression.
  case class ListNamespacesRequest(@field(1) filter: Text = t"")

  // The reply from `Namespaces.List`: every matching namespace.
  case class ListNamespacesResponse(@field(1) namespaces: List[Namespace] = Nil)

  // The request for `Tasks.List`: an optional containerd filter expression.
  case class ListTasksRequest(@field(1) filter: Text = t"")
  derives CanEqual

  // The reply from `Tasks.List`: the matching task processes.
  case class ListTasksResponse(@field(1) tasks: List[Workload] = Nil) derives CanEqual

  // A mount in a task's root filesystem (`containerd.types.Mount`): a `source` mounted at
  // `target` with a filesystem `mountType` (proto `type`) and mount `options`.
  case class Mount
    ( @field(1) mountType: Text       = t"",
      @field(2) source:    Text       = t"",
      @field(3) target:    Text       = t"",
      @field(4) options:   List[Text] = Nil )
  derives CanEqual

  // A containerd namespace (`containerd.services.namespaces.v1`): an isolation scope for
  // containers, images and the rest, identified by `name` and carrying free-form `labels`.
  case class Namespace(@field(1) name: Text, @field(2) labels: Map[Text, Text] = Map())
  derives CanEqual

  object ProcessStatus:
    // The status code carried in a `Workload` (a protobuf enum, encoded as its number).
    def of(code: Int): Optional[ProcessStatus] = values.find(_.code == code).optional

  enum ProcessStatus(val code: Int) derives CanEqual:
    case Unknown extends ProcessStatus(0)
    case Created extends ProcessStatus(1)
    case Running extends ProcessStatus(2)
    case Stopped extends ProcessStatus(3)
    case Paused  extends ProcessStatus(4)
    case Pausing extends ProcessStatus(5)

  // A container's runtime binding (`containerd.services.containers.v1.Container.Runtime`):
  // the runtime plugin `name` (e.g. `io.containerd.runc.v2`) and its opaque `options`.
  case class Runtime(@field(1) name: Text = t"", @field(2) options: AnyMessage = AnyMessage())

  // The request for `Tasks.Start`: the task (and optional exec process) to start.
  case class StartRequest(@field(1) containerId: Text = t"", @field(2) execId: Text = t"")
  derives CanEqual

  // The reply from `Tasks.Start`: the host pid of the started process.
  case class StartResponse(@field(1) pid: Int = 0) derives CanEqual

  object Timestamp:
    // Build a protobuf timestamp from any instant type — e.g. Aviation's `Instant` —
    // through anticipation's generic time abstraction, so no dependency on a specific
    // time library is needed. Millisecond precision: the generic representation is
    // epoch milliseconds, so protobuf's sub-millisecond `nanos` are not carried.
    def of[time: Abstractable across Instants to Long](time: time): Timestamp =
      val millis = time.generic
      Timestamp(millis/1000, ((millis%1000)*1000000).toInt)

  // The protobuf well-known `google.protobuf.Timestamp`: whole seconds and the
  // nanosecond fraction since the Unix epoch.
  case class Timestamp(@field(1) seconds: Long = 0L, @field(2) nanos: Int = 0)
  derives CanEqual:

    // Convert to any instant type — e.g. Aviation's `Instant` — at millisecond
    // precision, via the same generic time abstraction.
    def instant[time: Instantiable across Instants from Long]: time =
      (seconds*1000 + nanos/1000000).instantiate

  // The reply from containerd's `Version` RPC (`containerd.services.version.v1`): the
  // daemon's semantic version and the git revision it was built from.
  case class VersionResponse(@field(1) version: Text, @field(2) revision: Text)

  // The request for `Tasks.Wait`: the task (or exec process) to wait for.
  case class WaitRequest(@field(1) containerId: Text = t"", @field(2) execId: Text = t"")
  derives CanEqual

  // The reply from `Tasks.Wait`: the task's exit status and the time it exited.
  case class WaitResponse
    ( @field(1) exitStatus: Int = 0, @field(2) exitedAt: Timestamp = Timestamp() )
  derives CanEqual

// A connected containerd client. Each method maps to one gRPC call over the shared
// channel; the namespace travels in the channel's default metadata.
// The client retains its channel — a capability holding the ambient `Monitor` — so
// a client is itself a capability.
case class Containerd(channel: Grpc.Channel^):
  import Containerd.*

  // The daemon's version and build revision (`containerd.services.version.v1.Version`).
  def version()
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   VersionResponse =

    // The RPC and its codecs share only the resolution-scoped tactics; no aliased writer.
    // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
    scala.caps.unsafe.unsafeAssumeSeparate:
      channel.unary[Empty, VersionResponse](Containerd.versionMethod, Empty())

  // The containers in the bound namespace (`containerd.services.containers.v1`),
  // optionally narrowed by containerd `filters`.
  def containers(filters: List[Text] = Nil)
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   List[Container] =

    val request = ListContainersRequest(filters)

    // The RPC and its codecs share only the resolution-scoped tactics; no aliased writer.
    // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
    scala.caps.unsafe.unsafeAssumeSeparate:
      channel.unary[ListContainersRequest, ListContainersResponse]
        (Containerd.listContainersMethod, request).containers

  // Register a container, returning it as stored (`Containers.Create`). The container's
  // `spec` carries the OCI runtime spec as an `AnyMessage`.
  def createContainer(container: Container)
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   Container =

    val request = CreateContainerRequest(container)

    val created =
      // The RPC and its codecs share only the resolution-scoped tactics; no aliased writer.
      // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
      scala.caps.unsafe.unsafeAssumeSeparate:
        channel.unary[CreateContainerRequest, CreateContainerResponse]
          (Containerd.createContainerMethod, request).container

    Log.info(Containerd.Event.ContainerCreated(created.id))
    created

  // A single container by id (`Containers.Get`).
  def container(id: Text)
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   Container =

    val request = GetContainerRequest(id)

    // The RPC and its codecs share only the resolution-scoped tactics; no aliased writer.
    // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
    scala.caps.unsafe.unsafeAssumeSeparate:
      channel.unary[GetContainerRequest, GetContainerResponse]
        (Containerd.getContainerMethod, request).container

  // Remove a container by id (`Containers.Delete`).
  def deleteContainer(id: Text)
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   Unit =

    val request = DeleteContainerRequest(id)
    // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
    val _ = scala.caps.unsafe.unsafeAssumeSeparate:
      channel.unary[DeleteContainerRequest, Empty](Containerd.deleteContainerMethod, request)

    Log.info(Containerd.Event.ContainerDeleted(id))

  // The namespaces known to the daemon (`Namespaces.List`).
  def namespaces(filter: Text = t"")
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   List[Namespace] =

    val request = ListNamespacesRequest(filter)

    // The RPC and its codecs share only the resolution-scoped tactics; no aliased writer.
    // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
    scala.caps.unsafe.unsafeAssumeSeparate:
      channel.unary[ListNamespacesRequest, ListNamespacesResponse]
        (Containerd.listNamespacesMethod, request).namespaces

  // Create a namespace, returning it as stored (`Namespaces.Create`).
  def createNamespace(namespace: Namespace)
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   Namespace =

    val request = CreateNamespaceRequest(namespace)

    // The RPC and its codecs share only the resolution-scoped tactics; no aliased writer.
    // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
    scala.caps.unsafe.unsafeAssumeSeparate:
      channel.unary[CreateNamespaceRequest, CreateNamespaceResponse]
        (Containerd.createNamespaceMethod, request).namespace

  // Remove a namespace by name (`Namespaces.Delete`).
  def deleteNamespace(name: Text)
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   Unit =

    val request = DeleteNamespaceRequest(name)
    // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
    val _ = scala.caps.unsafe.unsafeAssumeSeparate:
      channel.unary[DeleteNamespaceRequest, Empty](Containerd.deleteNamespaceMethod, request)

  // The images in the bound namespace (`Images.List`), optionally filtered.
  def images(filters: List[Text] = Nil)
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   List[ImageRecord] =

    val request = ListImagesRequest(filters)

    // The RPC and its codecs share only the resolution-scoped tactics; no aliased writer.
    // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
    scala.caps.unsafe.unsafeAssumeSeparate:
      channel.unary[ListImagesRequest, ListImagesResponse]
        (Containerd.listImagesMethod, request).images

  // A single image by reference (`Images.Get`).
  def image(name: Text)
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   ImageRecord =

    val request = GetImageRequest(name)

    // The RPC and its codecs share only the resolution-scoped tactics; no aliased writer.
    // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
    scala.caps.unsafe.unsafeAssumeSeparate:
      channel.unary[GetImageRequest, GetImageResponse]
        (Containerd.getImageMethod, request).image

  // Remove an image by reference (`Images.Delete`).
  def deleteImage(name: Text, sync: Boolean = false)
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   Unit =

    val request = DeleteImageRequest(name, sync)
    // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
    val _ = scala.caps.unsafe.unsafeAssumeSeparate:
      channel.unary[DeleteImageRequest, Empty](Containerd.deleteImageMethod, request)

    Log.info(Containerd.Event.ImageDeleted(name))

  // Create a task for a container (`Tasks.Create`): give it a root filesystem (the
  // `rootfs` mounts, e.g. from an unpacked snapshot) and optional runtime `options`,
  // returning the container id and the new task's host pid.
  def createTask(containerId: Text, rootfs: List[Mount] = Nil, options: AnyMessage = AnyMessage())
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   CreateTaskResponse =

    val request = CreateTaskRequest(containerId, rootfs, options = options)
    Log.info(Containerd.Event.TaskCreated(containerId))
    // The RPC and its codecs share only the resolution-scoped tactics; no aliased writer.
    // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
    scala.caps.unsafe.unsafeAssumeSeparate:
      channel.unary[CreateTaskRequest, CreateTaskResponse](Containerd.createTaskMethod, request)

  // Start a created task (`Tasks.Start`), returning its host pid.
  def startTask(containerId: Text, execId: Text = t"")
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   Int =

    val request = StartRequest(containerId, execId)
    // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
    val pid = scala.caps.unsafe.unsafeAssumeSeparate:
      channel.unary[StartRequest, StartResponse](Containerd.startTaskMethod, request).pid

    Log.info(Containerd.Event.TaskStarted(containerId, pid))
    pid

  // Send a signal to a task (`Tasks.Kill`); `all` targets every process in the container.
  def killTask(containerId: Text, signal: Int, execId: Text = t"", all: Boolean = false)
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   Unit =

    val request = KillRequest(containerId, execId, signal, all)
    // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
    val _ = scala.caps.unsafe.unsafeAssumeSeparate:
      channel.unary[KillRequest, Empty](Containerd.killTaskMethod, request)

    Log.info(Containerd.Event.TaskKilled(containerId, signal))

  // Wait for a task to exit (`Tasks.Wait`), returning its exit status and time.
  def waitTask(containerId: Text, execId: Text = t"")
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   WaitResponse =

    val request = WaitRequest(containerId, execId)
    // The RPC and its codecs share only the resolution-scoped tactics; no aliased writer.
    // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
    scala.caps.unsafe.unsafeAssumeSeparate:
      channel.unary[WaitRequest, WaitResponse](Containerd.waitTaskMethod, request)

  // Delete a task (`Tasks.Delete`), returning its final exit status.
  def deleteTask(containerId: Text, execId: Text = t"")
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   DeleteTaskResponse =

    val request = DeleteTaskRequest(containerId, execId)
    Log.info(Containerd.Event.TaskDeleted(containerId))
    // The RPC and its codecs share only the resolution-scoped tactics; no aliased writer.
    // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
    scala.caps.unsafe.unsafeAssumeSeparate:
      channel.unary[DeleteTaskRequest, DeleteTaskResponse](Containerd.deleteTaskMethod, request)

  // The state of a single task (`Tasks.Get`).
  def task(containerId: Text, execId: Text = t"")
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   Workload =

    val request = GetTaskRequest(containerId, execId)
    // The RPC and its codecs share only the resolution-scoped tactics; no aliased writer.
    // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
    scala.caps.unsafe.unsafeAssumeSeparate:
      channel.unary[GetTaskRequest, GetTaskResponse](Containerd.getTaskMethod, request).process

  // Every task known to the daemon (`Tasks.List`), optionally filtered.
  def tasks(filter: Text = t"")
  ( using Monitor^ )
  ( using Tactic[Grpc.Error], Tactic[Http2.Error], Tactic[Async.Error], Tactic[Protobuf.Error] )
  :   List[Workload] =

    val request = ListTasksRequest(filter)
    // The RPC and its codecs share only the resolution-scoped tactics; no aliased writer.
    // [by-name-receiver] rPC and its codecs share resolution-scoped tactics
    scala.caps.unsafe.unsafeAssumeSeparate:
      channel.unary[ListTasksRequest, ListTasksResponse](Containerd.listTasksMethod, request).tasks
