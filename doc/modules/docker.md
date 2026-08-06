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
   ( path  = name.as[Relative on Tar],
     mode  = UnixMode(),
     user  = UnixUser(0),
     group = UnixGroup(0),
     mtime = 0.bits.u32,
     data  = TarBody(content.in[Data]) )

val layer = Layer(Tarfile(List(entry(t"hello.txt", t"hello world\n"))))
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

### WebAssembly workloads

A container image describes a filesystem, but `runc` — the thing that unpacks and runs one —
is not the only way to run a workload. A [Wasm](https://webassembly.org/) component needs no
rootfs at all: a runtime such as [wasmtime](https://wasmtime.dev/), reached through a
containerd shim like `io.containerd.wasmtime.v1`, instantiates it directly. What changes is
not the distribution format but what the configuration says the artifact *is*.

`Image.wasm` assembles that form — a
[Wasm OCI Artifact](https://tag-runtime.cncf.io/wgs/wasm/deliverables/wasm-oci-artifact/):

```scala
val artifact =
  Image.wasm
   ( component,
     exports = List(t"wasi:http/incoming-handler@0.2.0"),
     imports = List(t"wasi:io/streams@0.2.0"),
     target  = t"wasi:http/proxy@0.2.0" )
```

The manifest, index and archive are the ordinary ones, so the same registries and tools carry
it. Only two things differ: the config blob is typed `application/vnd.wasm.config.v0+json`
and describes a component rather than a filesystem, and the single layer is the component
itself — `application/wasm`, stored uncompressed, so its `diffId` and `digest` coincide.

```scala
artifact.wasmConfig.vouch.os            // wasip2 — the WASI generation
artifact.wasmConfig.vouch.architecture  // wasm
```

The `exports` and `imports` are the Component Model interfaces the workload offers and needs.
Recording them makes the image self-describing: a host can tell whether it can satisfy the
workload's capabilities before fetching the component. They are usually not written by hand
but read from the WIT world the component was linked against, which is the authoritative
statement of the same contract:

```scala
val world: WitDialect.World = WitDialect.worlds(source).stdlib(t"http")
Image.wasm(component, exports = world.exports, imports = world.imports)
```

Anthology does exactly this on its edge producing an `OciImage`, so a compilation can go from
source to a distributable artifact along one path, with nothing stating the contract twice.

### Reading an image

An existing OCI archive — a file or a block of bytes — is read by *opening* it, which makes
its contents available for the duration of a scope and no longer:

```scala
archive.open[Image](): handle ?=>
  handle.index                                    // the top-level index
  handle.manifest                                 // the manifest it names
  handle.imageConfig                              // the image configuration
  handle.verified(handle.manifest.layers.head)    // a layer, digest-checked
```

Reaching a layer three ways makes the cost explicit: `compressed` yields the stored bytes
untouched, `layer` decompresses them as a stream, and `verified` decompresses and checks the
content against the digest the manifest declares, raising an `OciError` if they disagree.

A reader that does not already know which kind of artifact it has opened asks for `config`,
which dispatches on the config descriptor's media type — so telling a component from a
filesystem is a question the archive answers, not one the caller has to have known:

```scala
archive.open[Image](): handle ?=>
  handle.config match
    case config: WasmConfig  => config.component  // a component: run it in a Wasm engine
    case config: ImageConfig => config.rootfs     // a filesystem: unpack and run it
```

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
