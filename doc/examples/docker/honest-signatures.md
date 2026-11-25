# A call to the daemon may wait

Every method of a containerd client is a request over a socket, which suspends the task that
makes it. Each asks for a `Monitor`, so a method which calls the daemon must ask for one too,
and can only be called from inside a supervised scope.

```scala fuel
compile
frame
  label A request, in a method which cannot wait
  errors show
  code
      import strategies.throwUnsafely

      def names(client: Containerd): List[Text] = client.containers().map(_.id)
frame
  label A method which can
  highlight Monitor
  code
      import strategies.throwUnsafely

      def names(client: Containerd)(using Monitor): List[Text] =
        client.containers().map(_.id)
frame
  label Called from a supervised scope
  highlight supervise
  code
      import strategies.throwUnsafely, threads.virtualThreads

      def names(client: Containerd)(using Monitor): List[Text] =
        client.containers().map(_.id)

      def report(client: Containerd): List[Text] = supervise(names(client))
##
```

The example is compiled, and is not run, since it needs a running daemon.
[Containers, images and namespaces](../../soundness/modules/docker.md#containers-images-and-namespaces)
lists the requests, and [honest signatures](../../philosophy/honest-signatures.md) explains
why waiting is declared.
