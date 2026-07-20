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

import soundness.*

import proscenium.compat.*

import providers.javaStdlibProvider
import alphabets.hexLowerCase
import charEncoders.utf8Encoder
import formatting.compactJsonFormatting
import strategies.throwUnsafely
import Http2.*

object Tests extends Suite(m"Embarcadero OCI Tests"):
  def run(): Unit =
    def fileEntry(name: Text, content: Text): Tar.Entry =
      Tar.Entry.File
       ( path  = name.as[Relative on Tar],
         mode  = UnixMode(),
         user  = UnixUser(0),
         group = UnixGroup(0),
         mtime = 0.bits.u32,
         data  = Progression(content.in[Data]) )

    def bytesOf(stream: Progression[Data]): Data = stream.foldLeft(IArray.empty[Byte])(_ ++ _)

    val layerTar = Tarfile(Progression(fileEntry(t"hello.txt", t"hello world\n")))
    val layer    = Layer(layerTar)
    val image    = Image(List(layer), config = ContainerConfig(Cmd = proscenium.List(t"/bin/sh")))

    suite(m"Layer digests"):
      val raw = layerTar.source[Data].memoize

      test(m"diff_id is the sha256 of the uncompressed tar"):
        layer.diffId
      . assert(_ == t"sha256:${raw.digest[Sha2[256]].serialize[Hex]}")

      test(m"descriptor digest is the sha256 of the gzipped blob"):
        layer.descriptor.digest
      . assert(_ == t"sha256:${layer.blob.digest[Sha2[256]].serialize[Hex]}")

      test(m"compressed digest differs from the diff_id"):
        layer.digest == layer.diffId
      . assert(_ == false)

      test(m"descriptor size matches the compressed blob length"):
        layer.descriptor.size
      . assert(_ == layer.blob.length.toLong)

      test(m"descriptor uses the gzipped-layer media type"):
        layer.descriptor.mediaType
      . assert(_ == media"application/vnd.oci.image.layer.v1.tar+gzip")

      test(m"the blob is a valid gzipped tar round-tripping to the original entry"):
        Tarfile.fromGzip(Progression(layer.blob)).map(_.entryName).stdlib.to(List)
      . assert(_ == List(t"hello.txt"))

    suite(m"Image config"):
      test(m"rootfs diff_ids list the layer diff_id in order"):
        image.imageConfig.rootfs.diff_ids
      . assert(_ == List(layer.diffId))

      test(m"rootfs type is 'layers'"):
        image.imageConfig.rootfs.`type`
      . assert(_ == t"layers")

      test(m"architecture and os default to amd64/linux"):
        (image.imageConfig.architecture, image.imageConfig.os)
      . assert(_ == (t"amd64", t"linux"))

      test(m"config blob JSON uses the snake_case diff_ids key"):
        image.imageConfig.in[Json].show.s.contains("\"diff_ids\"")
      . assert(_ == true)

      test(m"config blob JSON preserves the capitalised Cmd key"):
        image.imageConfig.in[Json].show.s.contains("\"Cmd\"")
      . assert(_ == true)

    suite(m"Manifest"):
      test(m"schemaVersion is 2"):
        image.manifest.schemaVersion
      . assert(_ == 2)

      test(m"manifest references the config descriptor by its media type"):
        image.manifest.config.mediaType
      . assert(_ == media"application/vnd.oci.image.config.v1+json")

      test(m"manifest lists the layer descriptor digest"):
        image.manifest.layers.map(_.digest)
      . assert(_ == List(layer.digest))

      test(m"config descriptor digest matches the config blob bytes"):
        image.configDescriptor.digest
      . assert(_ == t"sha256:${image.configBytes.digest[Sha2[256]].serialize[Hex]}")

      test(m"manifest JSON renders the exact OCI media-type string"):
        image.manifest.in[Json].show.s.contains("\"application/vnd.oci.image.manifest.v1+json\"")
      . assert(_ == true)

      test(m"manifest JSON round-trips through jacinta"):
        image.manifest.in[Json].as[Oci.Manifest]
      . assert(_ == image.manifest)

    suite(m"OCI archive"):
      val entries    = Tarfile.read(image.archive.source[Data]).stdlib.to(List)
      val names      = entries.map(_.entryName)
      val layoutData = entries.collect:
        case file: Tar.Entry.File if file.entryName == t"oci-layout" => bytesOf(file.data)

      test(m"archive contains the oci-layout marker and index.json"):
        (names.contains(t"oci-layout"), names.contains(t"index.json"))
      . assert(_ == (true, true))

      test(m"archive contains one blob per config, layer and manifest"):
        names.count(_.s.startsWith("blobs/sha256/"))
      . assert(_ == 3)

      test(m"the layer blob is stored under its digest path"):
        val hex = layer.digest.s.stripPrefix("sha256:")
        names.map(_.s).contains("blobs/sha256/"+hex)
      . assert(_ == true)

      test(m"oci-layout declares image layout version 1.0.0"):
        layoutData.map(bytes => bytes.to(List))
      . assert(_ == List(t"""{"imageLayoutVersion":"1.0.0"}""".in[Data].to(List)))

    suite(m"Opening an image archive"):
      val archiveData = image.archive.source[Data].memoize
      val layerBlobPath = t"blobs/sha256/${layer.digest.s.stripPrefix("sha256:").tt}"
      val manifestBlobPath =
        t"blobs/sha256/${image.manifestDescriptor.digest.s.stripPrefix("sha256:").tt}"

      // The archive with its entry list transformed: for exercising failure paths.
      def rebuilt(transform: List[Tar.Entry] => List[Tar.Entry]): Data =
        val entries = Tarfile.read(Progression(archiveData)).stdlib.to(List)
        Tarfile(Progression.from(transform(entries).stdlib)).source[Data].memoize

      def failure[result](body: => result): Optional[OciError.Reason] =
        try
          body
          Unset
        catch case error: OciError => error.reason

      test(m"the index round-trips through an opened archive"):
        archiveData.open[Image]() { handle ?=> handle.index }
      . assert(_ == image.index)

      test(m"the manifest round-trips, digest-verified"):
        archiveData.open[Image]() { handle ?=> handle.manifest }
      . assert(_ == image.manifest)

      test(m"the image config round-trips"):
        archiveData.open[Image]() { handle ?=> handle.imageConfig }
      . assert(_ == image.imageConfig)

      test(m"a layer's stored blob streams verbatim"):
        archiveData.open[Image]() { handle ?=> bytesOf(handle.compressed(image.manifest.layers.head)).to(List) }
      . assert(_ == layer.blob.to(List))

      test(m"a layer decompresses to the original tar bytes"):
        archiveData.open[Image]() { handle ?=> bytesOf(handle.layer(image.manifest.layers.head)).to(List) }
      . assert(_ == layer.raw.to(List))

      test(m"verified gathers a blob and confirms its digest and size"):
        archiveData.open[Image]() { handle ?=> handle.verified(image.manifest.layers.head).to(List) }
      . assert(_ == layer.blob.to(List))

      test(m"index JSON round-trips through jacinta"):
        image.index.in[Json].as[Index]
      . assert(_ == image.index)

      test(m"an archive without the oci-layout marker is rejected"):
        val data = rebuilt(_.filter(_.entryName != t"oci-layout"))
        failure(data.open[Image]() { handle ?=> handle.index })
      . assert(_ == OciError.Reason.MissingLayout)

      test(m"an archive without index.json is rejected"):
        val data = rebuilt(_.filter(_.entryName != t"index.json"))
        failure(data.open[Image]() { handle ?=> handle.index })
      . assert(_ == OciError.Reason.MissingIndex)

      test(m"a missing blob is reported by its digest"):
        val data = rebuilt(_.filter(_.entryName != layerBlobPath))
        failure(data.open[Image]() { handle ?=> handle.verified(image.manifest.layers.head) })
      . assert(_ == OciError.Reason.MissingBlob(layer.digest))

      test(m"a corrupted blob fails its digest check"):
        val data = rebuilt(_.map: entry =>
          if entry.entryName == manifestBlobPath then fileEntry(manifestBlobPath, t"{}")
          else entry)

        failure(data.open[Image]() { handle ?=> handle.manifest })
      . assert:
          case OciError.Reason.DigestMismatch(expected, _) =>
            expected == image.manifestDescriptor.digest

          case _ =>
            false

      test(m"opening an archive for writing is refused"):
        failure(archiveData.open[Image](Write) { handle ?=> () })
      . assert(_ == OciError.Reason.WriteUnsupported)

    suite(m"containerd over a gRPC loopback"):
      import threading.virtualThreading
      import probates.cancelProbate

      def pair(): (Duplex, Duplex) = Duplex.pair()

      // A fake containerd: completes the HTTP/2 handshake, records the namespace header
      // from the request, and replies to the `Version` call with a framed response.
      def runServer(serverSide: Duplex, namespace: Promise[Text], body: Data)
          ( using Monitor, Probate )
      :   Daemon =

        daemon:
          safely:
            serverSide.send(zephyrine.Stream(Frame.Settings(Nil, ack = false).serialize))
            // Skip the 24-byte client connection preface: consume exactly the
            // preface; anything after it stays in the endpoint's window.
            val source = serverSide.source
            var skipped = 0

            while skipped < 24 do source.refill(zephyrine.Credit(4096)) match
              case count: Int =>
                if count > 0 then
                  val take = count.min(24 - skipped)
                  source.skip(take)
                  skipped += take

              case _ =>
                skipped = 24

            val reader = FrameReader(source)
            val hpack = Hpack()
            var continue = true

            while continue do (reader.next(): @unchecked) match
              case Unset    => continue = false

              case f: Frame => f match
                case Frame.Settings(_, false) =>
                  serverSide.send(zephyrine.Stream(Frame.Settings(Nil, ack = true).serialize))

                case Frame.Headers(id, block, _, _) =>
                  val fields = hpack.decode(block)
                  fields.stdlib.find(_.name == t"containerd-namespace").foreach: entry =>
                    namespace.offer(entry.value)

                  val status = hpack.encode(List(HpackEntry(t":status", t"200"),
                      HpackEntry(t"content-type", t"application/grpc")))

                  val trailer = hpack.encode(List(HpackEntry(t"grpc-status", t"0")))
                  serverSide.send(zephyrine.Stream(Frame.Headers(id, status, false, true).serialize))
                  serverSide.send(zephyrine.Stream(Frame.Data(id, body, false).serialize))
                  serverSide.send(zephyrine.Stream(Frame.Headers(id, trailer, true, true).serialize))

                case _ => ()

      test(m"version() round-trips a VersionResponse and sends the namespace"):
        supervise:
          val (clientSide, serverSide) = pair()
          val namespace = Promise[Text]()
          val body = GrpcFraming.encode(VersionResponse(t"1.7.0", t"deadbeef").in[Protobuf].encode)
          runServer(serverSide, namespace, body)

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          val endpoint = Http2.Endpoint(Loopback(clientSide), t"localhost")
          val containerd = Containerd(endpoint, t"example")
          val response = containerd.version()
          (response.version, response.revision, namespace.await())
      . assert(_ == (t"1.7.0", t"deadbeef", t"example"))

      test(m"containers() decodes a repeated, labelled list"):
        supervise:
          val (clientSide, serverSide) = pair()
          val namespace = Promise[Text]()

          val list = ListContainersResponse(List(Container(t"alpha", Map(t"tier" -> t"db")),
              Container(t"beta")))

          val body = GrpcFraming.encode(list.in[Protobuf].encode)
          runServer(serverSide, namespace, body)

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          val endpoint = Http2.Endpoint(Loopback(clientSide), t"localhost")
          val containerd = Containerd(endpoint, t"example")
          containerd.containers().map(container => (container.id, container.labels))
      . assert(_ == List((t"alpha", Map(t"tier" -> t"db")), (t"beta", Map())))

      test(m"container(id) decodes a nested Container response"):
        supervise:
          val (clientSide, serverSide) = pair()
          val namespace = Promise[Text]()

          val response =
            GetContainerResponse(Container(t"gamma", Map(t"x" -> t"y"), image = t"img:1"))

          val body = GrpcFraming.encode(response.in[Protobuf].encode)
          runServer(serverSide, namespace, body)

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          val endpoint = Http2.Endpoint(Loopback(clientSide), t"localhost")
          val container = Containerd(endpoint, t"example").container(t"gamma")
          (container.id, container.labels, container.image)
      . assert(_ == (t"gamma", Map(t"x" -> t"y"), t"img:1"))

      test(m"namespaces() decodes the namespace list"):
        supervise:
          val (clientSide, serverSide) = pair()
          val namespace = Promise[Text]()

          val list = ListNamespacesResponse(List(Namespace(t"default"),
              Namespace(t"k8s.io", Map(t"managed" -> t"true"))))

          val body = GrpcFraming.encode(list.in[Protobuf].encode)
          runServer(serverSide, namespace, body)

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          val endpoint = Http2.Endpoint(Loopback(clientSide), t"localhost")
          Containerd(endpoint, t"example").namespaces().map(ns => (ns.name, ns.labels))
      . assert(_ == List((t"default", Map()), (t"k8s.io", Map(t"managed" -> t"true"))))

      test(m"images() decodes a list with nested descriptors and labels"):
        supervise:
          val (clientSide, serverSide) = pair()
          val namespace = Promise[Text]()

          val target = ContentDescriptor(t"application/vnd.oci.image.manifest.v1+json",
              t"sha256:abc", 1234L)

          val list = ListImagesResponse(List(ImageRecord(t"docker.io/library/alpine:latest",
              Map(t"arch" -> t"amd64"), target)))

          val body = GrpcFraming.encode(list.in[Protobuf].encode)
          runServer(serverSide, namespace, body)

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          val endpoint = Http2.Endpoint(Loopback(clientSide), t"localhost")

          Containerd(endpoint, t"example").images().map: image =>
            (image.name, image.labels, image.target.digest, image.target.size)
      . assert(_ == List((t"docker.io/library/alpine:latest", Map(t"arch" -> t"amd64"),
          t"sha256:abc", 1234L)))

      test(m"createContainer round-trips a container with an opaque spec"):
        supervise:
          val (clientSide, serverSide) = pair()
          val namespace = Promise[Text]()

          val container = Container(t"web", image = t"img:1",
              runtime = Runtime(t"io.containerd.runc.v2"),
              spec = AnyMessage(t"oci-spec", t"hello".in[Data]))

          val body = GrpcFraming.encode(CreateContainerResponse(container).in[Protobuf].encode)
          runServer(serverSide, namespace, body)

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          val endpoint = Http2.Endpoint(Loopback(clientSide), t"localhost")
          val created = Containerd(endpoint, t"example").createContainer(container)
          (created.id, created.runtime.name, created.spec.typeUrl, created.spec.value.to(List))
      . assert(_ == (t"web", t"io.containerd.runc.v2", t"oci-spec", t"hello".in[Data].to(List)))

      test(m"createTask sends rootfs mounts and returns the task pid"):
        supervise:
          val (clientSide, serverSide) = pair()
          val namespace = Promise[Text]()
          val body = GrpcFraming.encode(CreateTaskResponse(t"web", 4321).in[Protobuf].encode)
          runServer(serverSide, namespace, body)

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          val endpoint = Http2.Endpoint(Loopback(clientSide), t"localhost")
          val rootfs = List(Mount(t"overlay", t"overlay", t"/", List(t"lowerdir=/a")))
          Containerd(endpoint, t"example").createTask(t"web", rootfs).pid
      . assert(_ == 4321)

      test(m"tasks() decodes processes and maps the status code to ProcessStatus"):
        supervise:
          val (clientSide, serverSide) = pair()
          val namespace = Promise[Text]()

          val list =
            ListTasksResponse(List(Workload(t"web", t"", 4321, ProcessStatus.Running.code)))

          val body = GrpcFraming.encode(list.in[Protobuf].encode)
          runServer(serverSide, namespace, body)

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          val endpoint = Http2.Endpoint(Loopback(clientSide), t"localhost")
          Containerd(endpoint, t"example").tasks().map(task => (task.containerId, task.pid, task.state))
      . assert(_ == List((t"web", 4321, ProcessStatus.Running)))

    suite(m"workload lifecycle over a gRPC loopback"):
      import threading.virtualThreading
      import probates.cancelProbate

      val containersService = t"/containerd.services.containers.v1.Containers"
      val tasksService = t"/containerd.services.tasks.v1.Tasks"

      def pair(): (Duplex, Duplex) = Duplex.pair()

      // A routing fake containerd: like `runServer`, but it dispatches on the `:path`
      // pseudo-header, records the order of calls, and can fail chosen methods with a
      // non-zero grpc-status. Methods without a canned response reply with `Empty`.
      def runRouter
          ( serverSide: Duplex,
            calls:      scala.collection.mutable.ArrayBuffer[Text],
            responses:  Map[Text, Data],
            failures:   Set[Text] = Set() )
          ( using Monitor, Probate )
      :   Daemon =

        daemon:
          safely:
            serverSide.send(zephyrine.Stream(Frame.Settings(Nil, ack = false).serialize))
            val source = serverSide.source
            var skipped = 0

            while skipped < 24 do source.refill(zephyrine.Credit(4096)) match
              case count: Int =>
                if count > 0 then
                  val take = count.min(24 - skipped)
                  source.skip(take)
                  skipped += take

              case _ =>
                skipped = 24

            val reader = FrameReader(source)
            val hpack = Hpack()
            var continue = true

            while continue do (reader.next(): @unchecked) match
              case Unset    => continue = false

              case f: Frame => f match
                case Frame.Settings(_, false) =>
                  serverSide.send(zephyrine.Stream(Frame.Settings(Nil, ack = true).serialize))

                case Frame.Headers(id, block, _, _) =>
                  val fields = hpack.decode(block)
                  val path = fields.find(_.name == t":path").map(_.value).getOrElse(t"")
                  calls.synchronized(calls.append(path))

                  val status = hpack.encode(List(HpackEntry(t":status", t"200"),
                      HpackEntry(t"content-type", t"application/grpc")))

                  serverSide.send(zephyrine.Stream(Frame.Headers(id, status, false, true).serialize))

                  if failures.has(path) then
                    val trailer = hpack.encode(List(HpackEntry(t"grpc-status", t"3")))
                    serverSide.send(zephyrine.Stream(Frame.Headers(id, trailer, true, true).serialize))
                  else
                    val body =
                      responses.stdlib.getOrElse(path, GrpcFraming.encode(Empty().in[Protobuf].encode))

                    val trailer = hpack.encode(List(HpackEntry(t"grpc-status", t"0")))
                    serverSide.send(zephyrine.Stream(Frame.Data(id, body, false).serialize))
                    serverSide.send(zephyrine.Stream(Frame.Headers(id, trailer, true, true).serialize))

                case _ => ()

      def lifecycleResponses(startPid: Int): Map[Text, Data] =
        Map
          ( t"$containersService/Create" ->
              GrpcFraming.encode(CreateContainerResponse(Container(t"web")).in[Protobuf].encode),
            t"$tasksService/Create" ->
              GrpcFraming.encode(CreateTaskResponse(t"web", 4321).in[Protobuf].encode),
            t"$tasksService/Start" ->
              GrpcFraming.encode(StartResponse(startPid).in[Protobuf].encode),
            t"$tasksService/Wait" ->
              GrpcFraming.encode(WaitResponse(7).in[Protobuf].encode),
            t"$tasksService/Delete" ->
              GrpcFraming.encode(DeleteTaskResponse(t"web", 4321, 7).in[Protobuf].encode) )

      test(m"a Read-mode open creates container and task, never starts, then deletes"):
        supervise:
          val (clientSide, serverSide) = pair()
          val calls = scala.collection.mutable.ArrayBuffer[Text]()
          runRouter(serverSide, calls, lifecycleResponses(5678))

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          val endpoint = Http2.Endpoint(Loopback(clientSide), t"localhost")
          given containerd: (Containerd^) = Containerd(endpoint, t"example")
          val spec = Container(t"web", image = t"img:1")
          val pid = spec.open[Workload]() { workload ?=> workload.pid }
          (pid, calls.synchronized(calls.to(List)))
      . assert(_ == (4321, List(t"$containersService/Create", t"$tasksService/Create",
          t"$tasksService/Delete", t"$containersService/Delete")))

      test(m"a Run-mode open starts the task and awaits its exit"):
        supervise:
          val (clientSide, serverSide) = pair()
          val calls = scala.collection.mutable.ArrayBuffer[Text]()
          runRouter(serverSide, calls, lifecycleResponses(5678))

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          val endpoint = Http2.Endpoint(Loopback(clientSide), t"localhost")
          given containerd: (Containerd^) = Containerd(endpoint, t"example")
          val spec = Container(t"web", image = t"img:1")

          val (pid, exit) = spec.open[Workload](Read & Run & Signal): workload ?=>
            (workload.pid, workload.await().exitStatus)

          (pid, exit, calls.synchronized(calls.to(List)))
      . assert(_ == (5678, 7, List(t"$containersService/Create", t"$tasksService/Create",
          t"$tasksService/Start", t"$tasksService/Wait", t"$tasksService/Kill",
          t"$tasksService/Wait", t"$tasksService/Delete", t"$containersService/Delete")))

      test(m"teardown still runs when the block throws, and the error escapes"):
        supervise:
          val (clientSide, serverSide) = pair()
          val calls = scala.collection.mutable.ArrayBuffer[Text]()
          runRouter(serverSide, calls, lifecycleResponses(5678))

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          val endpoint = Http2.Endpoint(Loopback(clientSide), t"localhost")
          given containerd: (Containerd^) = Containerd(endpoint, t"example")
          val spec = Container(t"web", image = t"img:1")

          val outcome =
            try
              spec.open[Workload](Read & Run) { workload ?=> throw java.lang.IllegalStateException() }
              t"returned"
            catch case _: java.lang.IllegalStateException => t"escaped"

          (outcome, calls.synchronized(calls.to(List)))
      . assert(_ == (t"escaped", List(t"$containersService/Create", t"$tasksService/Create",
          t"$tasksService/Start", t"$tasksService/Kill", t"$tasksService/Wait",
          t"$tasksService/Delete", t"$containersService/Delete")))

      test(m"a failed container creation makes no further calls"):
        supervise:
          val (clientSide, serverSide) = pair()
          val calls = scala.collection.mutable.ArrayBuffer[Text]()
          runRouter(serverSide, calls, Map(), failures = Set(t"$containersService/Create"))

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          val endpoint = Http2.Endpoint(Loopback(clientSide), t"localhost")
          given containerd: (Containerd^) = Containerd(endpoint, t"example")
          val spec = Container(t"web", image = t"img:1")
          val result = safely(spec.open[Workload]() { workload ?=> () })
          (result.absent, calls.synchronized(calls.to(List)))
      . assert(_ == (true, List(t"$containersService/Create")))

    suite(m"containerd timestamps via the generic time abstraction"):
      // The `Long`-as-instant given lets us mint an Aviation `Instant` from epoch
      // millis; Aviation's own `Instant` abstractable/instantiable instances are found
      // via its companion, so `embarcadero` needs no dependency on Aviation.
      import abstractables.instantAbstractable
      import chronometries.unix
      val moment = Instant(1_700_000_001_000L)

      test(m"a Container timestamp round-trips and converts to an Aviation Instant"):
        val container = Container(t"svc", createdAt = embarcadero.Timestamp.of(moment))
        val restored = proscenium.Progression(container.in[Protobuf].encode).read[Container in Protobuf]
        restored.createdAt.instant[Instant over Unix]
      . assert(_ == moment)
