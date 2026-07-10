## Containers

### About

Soundness builds [OCI](https://opencontainers.org/) container images and talks to a
[containerd](https://containerd.io/) daemon. An image is assembled in memory from
filesystem layers and a configuration, and rendered as a standard OCI archive that any
container runtime can load, with its content digests computed correctly. A separate client
speaks to a running containerd daemon over gRPC, to list and create containers, images and
namespaces, and to start and stop the tasks that run inside containers.

An image and everything in it — its layers, its manifest, its configuration — are immutable
typed values, and each container, task and namespace on the daemon is likewise a typed
value rather than a bag of JSON. The two halves are independent: an image can be built
without a daemon, and a daemon can be managed without building an image.

### On containers

A container image is not a single file but a stack of filesystem
[layers](https://en.wikipedia.org/wiki/OverlayFS) plus a configuration saying how to run
them, and each piece is addressed by the [digest](https://en.wikipedia.org/wiki/Content-addressable_storage)
of its bytes. The format is exact: a layer has one digest for its uncompressed tar and
another for its compressed blob, and a manifest ties them together by those digests. Get a
digest wrong and the image is silently invalid.

Running such an image is the job of a daemon, and containerd — the runtime beneath Docker
and Kubernetes — is driven over a gRPC protocol on a local socket. Soundness represents the
image format as values whose digests it computes, and the daemon's protocol as methods that
exchange typed values. Building an image draws on the [archives](archives.md) module for its
tar layers; both halves come from the `soundness` package:

```scala
import soundness.*
import providers.javaStdlibProvider
import alphabets.hexLowerCase
import charEncoders.utf8Encoder
import formatting.compactJsonFormatting
import strategies.throwUnsafely
```

### Building an image

A layer is a tar archive of files. Given a `Tarfile`, `Layer` wraps it and computes the
digests the format requires:

```scala
def entry(name: Text, content: Text): Tar.Entry =
  Tar.Entry.File
   ( path  = name.decode[Relative on Tar],
     mode  = UnixMode(),
     user  = UnixUser(0),
     group = UnixGroup(0),
     mtime = 0.bits.u32,
     data  = LazyList(content.data) )

val layer = Layer(Tarfile(LazyList(entry(t"hello.txt", t"hello world\n"))))
```

An `Image` assembles one or more layers with a configuration — the command to run, the
environment, the working directory — into a complete image:

```scala
val image = Image(List(layer), config = ContainerConfig(Cmd = List(t"/bin/sh")))
```

### What an image contains

The assembled image exposes each part the format defines. A layer reports both of its
digests — `diffId` for the uncompressed tar and `digest` for the compressed blob — and the
image carries the manifest that references them:

```scala
layer.diffId       // sha256:… of the uncompressed layer
layer.digest       // sha256:… of the compressed blob
image.manifest     // the OCI manifest tying config and layers together
```

`image.archive` is the whole image as an OCI tar layout — the form a runtime imports. Read
as bytes and written to a file, it produces an image that `docker load` or containerd will
accept.

### Connecting to a daemon

A `Containerd` client connects to a running daemon over an HTTP/2 endpoint on its socket,
bound to a namespace. Because the connection holds background work, it lives inside a
supervised scope:

```scala
supervise:
  val client = Containerd(endpoint, namespace = t"default")
  client.version()
```

Here `endpoint` is an HTTP/2 endpoint over containerd's Unix socket, established through
Soundness's [HTTP](http-server.md) and socket support. Every call to the daemon may fail —
the socket, the protocol, or the request itself — so the calls draw on the error strategy
in scope.

### Containers, images and namespaces

The client lists and inspects what the daemon holds, and creates and deletes it. Each
result is a typed record:

```scala
val containers = client.containers()
val one = client.container(t"web")
(one.id, one.image, one.labels)

client.images()      // the images known to the daemon
client.namespaces()  // the namespaces on the daemon
```

`createContainer`, `deleteContainer`, `createNamespace` and their counterparts make the
corresponding changes.

### Tasks

A container is a definition; a *task* is a running instance of one. A task is created from a
container with the root filesystem to mount, then started, waited on, and killed:

```scala
val rootfs = List(Mount(t"overlay", t"overlay", t"/", List(t"lowerdir=/a")))

client.createTask(t"web", rootfs)
val pid = client.startTask(t"web")
client.waitTask(t"web")
client.killTask(t"web", signal = 15)
```

`client.task(t"web")` returns the task's current `Workload`, whose `state` reports whether
it is created, running, stopped or paused.
