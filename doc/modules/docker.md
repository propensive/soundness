## Containers

### About

An [OCI](https://opencontainers.org/) container image is assembled in memory from filesystem
layers and a configuration, and rendered as a standard OCI archive which any container runtime
can load, with its content digests computed correctly. A separate client speaks to a running
[containerd](https://containerd.io/) daemon over gRPC. It lists and creates containers, images
and namespaces, and starts and stops the tasks which run inside containers.

An image and everything in it, its layers, its manifest and its configuration, are [immutable]
typed values. Each container, task and namespace on the daemon is a typed value too, and not a
bag of JSON. The two halves are independent. An image can be built without a daemon, and a
daemon can be managed without building an image.

### On containers

A container image is not a single file. It is a stack of filesystem
[layers](https://en.wikipedia.org/wiki/OverlayFS), plus a configuration which says how to run
them. Each piece is addressed by the
[digest](https://en.wikipedia.org/wiki/Content-addressable_storage) of its bytes. The format is
exact. A layer has one digest for its uncompressed tar and another for its compressed blob, and
a manifest ties them together by those digests. Get a digest wrong, and the image is silently
invalid.

Running such an image is the job of a daemon. containerd, the runtime beneath Docker and
Kubernetes, is driven over a gRPC protocol on a local socket.

Here the image format is a set of values whose digests are computed for you, and the protocol of
the daemon is a set of methods which exchange typed values. Building an image as a value, and not
by running a tool, is [direct style](../philosophy/direct-style.md): the steps are ordinary code,
checked as it compiles.

Building an image draws on [archives](archives.md) for its tar layers. Everything comes from the
`soundness` package, and the last import lets the samples print their results:

```scala
import soundness.*

import alphabets.hexLowerCase
import codepages.utf8Codepage
import formatting.compactJsonFormatting
import providers.javaBaseProvider
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Building an image

A layer is a tar archive of files. Given a `Tarfile`, `Layer` wraps it and computes the digests
which the format requires:

```scala
def entry(name: Text, content: Text): Tar.Entry =
  Tar.Entry.File
   ( path  = name.as[Relative on Tar],
     mode  = UnixMode(),
     user  = UnixUser(0),
     group = UnixGroup(0),
     mtime = 0.bits.u32,
     data  = Archive.Body(content.in[Data]) )

val layer = Layer(Tarfile(List(entry(t"hello.txt", t"hello world\n"))))
```

An `Image` assembles one or more layers with a configuration, which holds the command to run, the
environment and the working directory:

```scala
val image = Image(List(layer), config = ContainerConfig(Cmd = List(t"/bin/sh")))
```

### What an image contains

The assembled image exposes each part which the format defines. A layer reports both of its
digests. `diffId` is the digest of the uncompressed tar, and `digest` is the digest of the
compressed blob:

```scala fuel
run
##
Out.println(layer.diffId.show)
Out.println(layer.digest.show)
```

The image carries the manifest which references them, with the media type of each layer and of
the configuration:

```scala fuel
run
##
image.manifest.layers.each(descriptor => Out.println(descriptor.mediaType.show))
Out.println(image.manifest.config.mediaType.show)
```

`image.archive` is the whole image as an OCI tar layout, which is the form a runtime imports.
Read as bytes and written to a file, it produces an image which `docker load` or containerd
will accept.

### WebAssembly workloads

A container image describes a filesystem, but `runc`, which unpacks and runs one, is not the
only way to run a workload. A [Wasm](https://webassembly.org/) component needs no root
filesystem at all. A runtime such as [wasmtime](https://wasmtime.dev/), reached through a
containerd shim like `io.containerd.wasmtime.v1`, instantiates it directly. What changes is
not the distribution format, but what the configuration says the artifact *is*.

`Image.wasm` assembles that form, a
[Wasm OCI Artifact](https://tag-runtime.cncf.io/wgs/wasm/deliverables/wasm-oci-artifact/):

```scala
val component: Data = hex"0061736d01000000"

val artifact =
  Image.wasm
   ( component,
     exports = List(t"wasi:http/incoming-handler@0.2.0"),
     imports = List(t"wasi:io/streams@0.2.0"),
     target  = t"wasi:http/proxy@0.2.0" )
```

The manifest, index and archive are the ordinary ones, so the same registries and tools carry
it. Only two things differ:

```tabular
column  Part
column  Container image
column  Wasm artifact
row  config  a filesystem's run configuration  `application/vnd.wasm.config.v0+json`, describing a component
row  layers  compressed tars, one or more  the component itself, `application/wasm`
row  layer digests  `diffId` and `digest` differ  `diffId` and `digest` coincide, since it is stored uncompressed
```

The configuration describes the component, with the WASI generation as its `os`:

```scala fuel
run
##
Out.println(artifact.wasmConfig.let(_.os).inspect)
Out.println(artifact.wasmConfig.let(_.architecture).inspect)
```

The `exports` and `imports` are the Component Model interfaces which the workload offers and
needs. Recording them makes the image self-describing. A host can tell whether it can satisfy
the workload's capabilities before fetching the component.

They are usually not written by hand, but read from the WIT world which the component was linked
against, which is the authoritative statement of the same contract:

```scala
val wit = t"""package example:service;

world http {
  import wasi:io/streams@0.2.0;
  export wasi:http/incoming-handler@0.2.0;
}"""

WitDialect.worlds(wit)(t"http").let: world =>
  Image.wasm(component, exports = world.exports, imports = world.imports)
```

The [compiler](compiler.md#wasi-components) does exactly this on its edge which produces an OCI
image. A compilation can go from source to a distributable artifact along one path, and nothing
states the contract twice.

### Reading an image

An existing OCI archive, which is a file or a block of bytes, is read by *opening* it. Its
contents are available for the duration of a scope, and no longer:

```scala
val archive: Data = image.archive.source[Data].memoize
```

```scala fuel
run
##
archive.open[Image](): handle ?=>
  handle.index.manifests.each(descriptor => Out.println(descriptor.mediaType.show))
  handle.manifest.layers.each(descriptor => Out.println(descriptor.digest.show))
  Out.println(handle.imageConfig.config.let(_.Cmd).inspect)
```

`handle.verified(descriptor)` reads a layer and checks its digest. The three ways to reach a
layer make the cost explicit:

```tabular
column  Method
column  Yields
column  Checks the digest
row  `compressed`  the stored bytes, untouched, as a stream  no
row  `layer`  the uncompressed tar, as a stream  no
row  `verified`  the stored bytes, gathered eagerly  yes, and the size
```

`verified` raises an `Oci.Error` if the bytes disagree with the digest or the size which the
manifest declares. Checking a stream would mean draining it, which is why `verified` gathers
the bytes first.

A reader which does not already know which kind of artifact it has opened asks for `config`. It
dispatches on the media type of the config descriptor. Telling a component from a filesystem is
a question which the archive answers, and not one the caller has to have known:

```scala
archive.open[Image](): handle ?=>
  handle.config match
    case config: WasmConfig  => config.component
    case config: Image.Config => config.rootfs
```

### Connecting to a daemon

The rest of this page needs a running daemon, so its samples are compiled but not run.

A `Containerd` client connects to a daemon over an HTTP/2 endpoint on its socket, bound to a
namespace. The connection holds background work, so it lives inside a supervised scope:

```scala
import probates.awaitProbate
import threads.virtualThreads

def report[endpoint](endpoint: Http2.Endpoint[endpoint]): Unit = supervise:
  val client = Containerd(endpoint, t"default")
  client.version()
```

Here `endpoint` is an HTTP/2 endpoint over containerd's Unix socket, established through the
HTTP/2 and [socket](sockets.md) support, which the client speaks [gRPC](rpc.md) over. Every
call to the daemon may fail, in the socket, the protocol or the request itself, so the calls
draw on the error [strategy] in scope.

### Containers, images and namespaces

The client lists and inspects what the daemon holds, and creates and deletes it. Each result is
a typed record:

```scala
def inspect(client: Containerd)(using Monitor): Unit =
  val containers = client.containers()
  val one = client.container(t"web")
  (one.id, one.image, one.labels)

  client.images()
  client.namespaces()
```

These are the methods for each kind of resource:

```tabular
column  Resource
column  List
column  Inspect
column  Create
column  Delete
row  container  `containers()`  `container(id)`  `createContainer(container)`  `deleteContainer(id)`
row
  cell  namespace
  cell  `namespaces()`
  cell
  cell  `createNamespace(namespace)`
  cell  `deleteNamespace(name)`
row
  cell  image
  cell  `images()`
  cell  `image(name)`
  cell
  cell  `deleteImage(name)`
row  task  `tasks()`  `task(id)`  `createTask(id, rootfs)`  `deleteTask(id)`
```

### Tasks

A container is a definition. A *task* is a running instance of one. A task is created from a
container with the root filesystem to mount, then started, waited on and killed:

```scala
def run(client: Containerd)(using Monitor): Unit =
  val rootfs = List(Containerd.Mount(t"overlay", t"overlay", t"/", List(t"lowerdir=/a")))

  client.createTask(t"web", rootfs)
  val pid = client.startTask(t"web")
  client.waitTask(t"web")
  client.killTask(t"web", signal = 15)
```

`client.task(t"web")` returns the current `Workload` of the task, whose `state` reports whether
it is created, running, stopped or paused.

### Related

- [Archives](archives.md) describes the tar archives which layers are made of.
- [Compiler](compiler.md#wasi-components) produces the WASI components and OCI images which a
  toolchain links.
- [Hashing](hashing.md) describes the digests which address every part of an image.
- [RPC](rpc.md) describes the gRPC which the client speaks to containerd.
- [Sockets](sockets.md) describes the Unix socket which the daemon listens on.
- [Concurrency](concurrency.md) describes `supervise`, which the client lives inside.
- [Compression](compression.md) describes the gzip which layers are compressed with, and [processes](processes.md) the commands a task runs.
