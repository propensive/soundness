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

import anticipation.*
import contingency.*
import cordillera.*
import gossamer.*
import locomotion.*
import obligatory.*
import parasite.*

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
    ( using monitor: Monitor, probate: Probate, asyncError: Tactic[AsyncError] )
  :   Containerd^{monitor, caps.any} =

    val metadata = Grpc.Metadata(List(t"containerd-namespace" -> namespace))
    Containerd(GrpcChannel(endpoint, metadata))

// A connected containerd client. Each method maps to one gRPC call over the shared
// channel; the namespace travels in the channel's default metadata.
// The client retains its channel — a capability holding the ambient `Monitor` — so
// a client is itself a capability.
case class Containerd(channel: GrpcChannel^):
  // The daemon's version and build revision (`containerd.services.version.v1.Version`).
  def version()
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   VersionResponse =

    channel.unary[Empty, VersionResponse](Containerd.versionMethod, Empty())

  // The containers in the bound namespace (`containerd.services.containers.v1`),
  // optionally narrowed by containerd `filters`.
  def containers(filters: List[Text] = Nil)
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   List[Container] =

    val request = ListContainersRequest(filters)

    channel.unary[ListContainersRequest, ListContainersResponse]
      (Containerd.listContainersMethod, request).containers

  // Register a container, returning it as stored (`Containers.Create`). The container's
  // `spec` carries the OCI runtime spec as an `AnyMessage`.
  def createContainer(container: Container)
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   Container =

    val request = CreateContainerRequest(container)

    val created =
      channel.unary[CreateContainerRequest, CreateContainerResponse]
        (Containerd.createContainerMethod, request).container

    Log.info(DockerEvent.ContainerCreated(created.id))
    created

  // A single container by id (`Containers.Get`).
  def container(id: Text)
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   Container =

    val request = GetContainerRequest(id)

    channel.unary[GetContainerRequest, GetContainerResponse]
      (Containerd.getContainerMethod, request).container

  // Remove a container by id (`Containers.Delete`).
  def deleteContainer(id: Text)
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   Unit =

    val request = DeleteContainerRequest(id)
    val _ = channel.unary[DeleteContainerRequest, Empty](Containerd.deleteContainerMethod, request)
    Log.info(DockerEvent.ContainerDeleted(id))

  // The namespaces known to the daemon (`Namespaces.List`).
  def namespaces(filter: Text = t"")
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   List[Namespace] =

    val request = ListNamespacesRequest(filter)

    channel.unary[ListNamespacesRequest, ListNamespacesResponse]
      (Containerd.listNamespacesMethod, request).namespaces

  // Create a namespace, returning it as stored (`Namespaces.Create`).
  def createNamespace(namespace: Namespace)
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   Namespace =

    val request = CreateNamespaceRequest(namespace)

    channel.unary[CreateNamespaceRequest, CreateNamespaceResponse]
      (Containerd.createNamespaceMethod, request).namespace

  // Remove a namespace by name (`Namespaces.Delete`).
  def deleteNamespace(name: Text)
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   Unit =

    val request = DeleteNamespaceRequest(name)
    val _ = channel.unary[DeleteNamespaceRequest, Empty](Containerd.deleteNamespaceMethod, request)

  // The images in the bound namespace (`Images.List`), optionally filtered.
  def images(filters: List[Text] = Nil)
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   List[ImageRecord] =

    val request = ListImagesRequest(filters)

    channel.unary[ListImagesRequest, ListImagesResponse]
      (Containerd.listImagesMethod, request).images

  // A single image by reference (`Images.Get`).
  def image(name: Text)
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   ImageRecord =

    val request = GetImageRequest(name)

    channel.unary[GetImageRequest, GetImageResponse]
      (Containerd.getImageMethod, request).image

  // Remove an image by reference (`Images.Delete`).
  def deleteImage(name: Text, sync: Boolean = false)
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   Unit =

    val request = DeleteImageRequest(name, sync)
    val _ = channel.unary[DeleteImageRequest, Empty](Containerd.deleteImageMethod, request)
    Log.info(DockerEvent.ImageDeleted(name))

  // Create a task for a container (`Tasks.Create`): give it a root filesystem (the
  // `rootfs` mounts, e.g. from an unpacked snapshot) and optional runtime `options`,
  // returning the container id and the new task's host pid.
  def createTask(containerId: Text, rootfs: List[Mount] = Nil, options: AnyMessage = AnyMessage())
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   CreateTaskResponse =

    val request = CreateTaskRequest(containerId, rootfs, options = options)
    Log.info(DockerEvent.TaskCreated(containerId))
    channel.unary[CreateTaskRequest, CreateTaskResponse](Containerd.createTaskMethod, request)

  // Start a created task (`Tasks.Start`), returning its host pid.
  def startTask(containerId: Text, execId: Text = t"")
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   Int =

    val request = StartRequest(containerId, execId)
    val pid = channel.unary[StartRequest, StartResponse](Containerd.startTaskMethod, request).pid
    Log.info(DockerEvent.TaskStarted(containerId, pid))
    pid

  // Send a signal to a task (`Tasks.Kill`); `all` targets every process in the container.
  def killTask(containerId: Text, signal: Int, execId: Text = t"", all: Boolean = false)
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   Unit =

    val request = KillRequest(containerId, execId, signal, all)
    val _ = channel.unary[KillRequest, Empty](Containerd.killTaskMethod, request)
    Log.info(DockerEvent.TaskKilled(containerId, signal))

  // Wait for a task to exit (`Tasks.Wait`), returning its exit status and time.
  def waitTask(containerId: Text, execId: Text = t"")
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   WaitResponse =

    val request = WaitRequest(containerId, execId)
    channel.unary[WaitRequest, WaitResponse](Containerd.waitTaskMethod, request)

  // Delete a task (`Tasks.Delete`), returning its final exit status.
  def deleteTask(containerId: Text, execId: Text = t"")
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   DeleteTaskResponse =

    val request = DeleteTaskRequest(containerId, execId)
    Log.info(DockerEvent.TaskDeleted(containerId))
    channel.unary[DeleteTaskRequest, DeleteTaskResponse](Containerd.deleteTaskMethod, request)

  // The state of a single task (`Tasks.Get`).
  def task(containerId: Text, execId: Text = t"")
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   Workload =

    val request = GetTaskRequest(containerId, execId)
    channel.unary[GetTaskRequest, GetTaskResponse](Containerd.getTaskMethod, request).process

  // Every task known to the daemon (`Tasks.List`), optionally filtered.
  def tasks(filter: Text = t"")
  ( using Monitor^ )
  ( using Tactic[GrpcError], Tactic[Http2Error], Tactic[AsyncError], Tactic[ProtobufError] )
  :   List[Workload] =

    val request = ListTasksRequest(filter)
    channel.unary[ListTasksRequest, ListTasksResponse](Containerd.listTasksMethod, request).tasks
