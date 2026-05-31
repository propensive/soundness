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

  // Connect to a containerd endpoint (typically a Unix socket carrying cleartext h2c),
  // binding every call to `namespace` via the mandatory `containerd-namespace` header.
  // Must be called inside a `supervise` scope (the connection runs background daemons).
  def apply[endpoint]
    ( endpoint: Http2.Endpoint[endpoint], namespace: Text )
    ( using Monitor, Codicil )
  :   Containerd raises AsyncError =

    val metadata = Grpc.Metadata(List(t"containerd-namespace" -> namespace))
    Containerd(GrpcChannel(endpoint, metadata))

// A connected containerd client. Each method maps to one gRPC call over the shared
// channel; the namespace travels in the channel's default metadata.
case class Containerd(channel: GrpcChannel):
  // The daemon's version and build revision (`containerd.services.version.v1.Version`).
  def version()
  :   VersionResponse raises GrpcError raises Http2Error raises AsyncError raises ProtobufError =

    channel.unary[Empty, VersionResponse](Containerd.versionMethod, Empty())

  // The containers in the bound namespace (`containerd.services.containers.v1`),
  // optionally narrowed by containerd `filters`.
  def containers(filters: List[Text] = Nil)
  :   List[Container] raises GrpcError raises Http2Error raises AsyncError raises ProtobufError =

    val request = ListContainersRequest(filters)

    channel.unary[ListContainersRequest, ListContainersResponse]
      (Containerd.listContainersMethod, request).containers

  // Register a container, returning it as stored (`Containers.Create`). The container's
  // `spec` carries the OCI runtime spec as an `AnyMessage`.
  def createContainer(container: Container)
  :   Container raises GrpcError raises Http2Error raises AsyncError raises ProtobufError =

    val request = CreateContainerRequest(container)

    channel.unary[CreateContainerRequest, CreateContainerResponse]
      (Containerd.createContainerMethod, request).container

  // A single container by id (`Containers.Get`).
  def container(id: Text)
  :   Container raises GrpcError raises Http2Error raises AsyncError raises ProtobufError =

    val request = GetContainerRequest(id)

    channel.unary[GetContainerRequest, GetContainerResponse]
      (Containerd.getContainerMethod, request).container

  // Remove a container by id (`Containers.Delete`).
  def deleteContainer(id: Text)
  :   Unit raises GrpcError raises Http2Error raises AsyncError raises ProtobufError =

    val request = DeleteContainerRequest(id)
    val _ = channel.unary[DeleteContainerRequest, Empty](Containerd.deleteContainerMethod, request)

  // The namespaces known to the daemon (`Namespaces.List`).
  def namespaces(filter: Text = t"")
  :   List[Namespace] raises GrpcError raises Http2Error raises AsyncError raises ProtobufError =

    val request = ListNamespacesRequest(filter)

    channel.unary[ListNamespacesRequest, ListNamespacesResponse]
      (Containerd.listNamespacesMethod, request).namespaces

  // Create a namespace, returning it as stored (`Namespaces.Create`).
  def createNamespace(namespace: Namespace)
  :   Namespace raises GrpcError raises Http2Error raises AsyncError raises ProtobufError =

    val request = CreateNamespaceRequest(namespace)

    channel.unary[CreateNamespaceRequest, CreateNamespaceResponse]
      (Containerd.createNamespaceMethod, request).namespace

  // Remove a namespace by name (`Namespaces.Delete`).
  def deleteNamespace(name: Text)
  :   Unit raises GrpcError raises Http2Error raises AsyncError raises ProtobufError =

    val request = DeleteNamespaceRequest(name)
    val _ = channel.unary[DeleteNamespaceRequest, Empty](Containerd.deleteNamespaceMethod, request)

  // The images in the bound namespace (`Images.List`), optionally filtered.
  def images(filters: List[Text] = Nil)
  :   List[ImageRecord] raises GrpcError raises Http2Error raises AsyncError raises ProtobufError =

    val request = ListImagesRequest(filters)

    channel.unary[ListImagesRequest, ListImagesResponse]
      (Containerd.listImagesMethod, request).images

  // A single image by reference (`Images.Get`).
  def image(name: Text)
  :   ImageRecord raises GrpcError raises Http2Error raises AsyncError raises ProtobufError =

    val request = GetImageRequest(name)

    channel.unary[GetImageRequest, GetImageResponse]
      (Containerd.getImageMethod, request).image

  // Remove an image by reference (`Images.Delete`).
  def deleteImage(name: Text, sync: Boolean = false)
  :   Unit raises GrpcError raises Http2Error raises AsyncError raises ProtobufError =

    val request = DeleteImageRequest(name, sync)
    val _ = channel.unary[DeleteImageRequest, Empty](Containerd.deleteImageMethod, request)
