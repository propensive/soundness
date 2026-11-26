# A client lives inside a scope

A connection to containerd keeps tasks running in the background to carry its requests. They
need a scope to belong to, so a client cannot be made outside one, and when the scope ends the
connection's tasks end with it.

```scala fuel
compile
frame
  label A client with nothing to supervise it
  errors show
  code
      import probates.awaitProbate, strategies.throwUnsafely

      def client[endpoint](endpoint: Http2.Endpoint[endpoint]) =
        Containerd(endpoint, t"default")
frame
  label A client, and its request, in a supervised scope
  highlight supervise  owns the connection's background work
  code
      import probates.awaitProbate, strategies.throwUnsafely, threads.virtualThreads

      def version[endpoint](endpoint: Http2.Endpoint[endpoint]): Text = supervise:
        val client = Containerd(endpoint, t"default")
        client.version().version
##
```

The example is compiled, and is not run, since it needs a running daemon.
[Connecting to a daemon](../../soundness/modules/docker.md#connecting-to-a-daemon) describes
the endpoint, and [structured concurrency](../../philosophy/structured-concurrency.md) the
rule that a task has an owner.
