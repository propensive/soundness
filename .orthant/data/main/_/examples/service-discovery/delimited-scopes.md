# An advertisement lasts as long as its block

Advertising a service is a loan. The instance is claimed on the network when the block begins,
is known by its claimed name inside it, and is withdrawn with a goodbye when the block ends,
however it ends.

```scala fuel
compile
frame
  label A service and a description of one instance
  code
      import socketBackends.javaBaseSockets, discoveryBackends.mdnsSockets, strategies.throwUnsafely
      import threads.virtualThreads, probates.awaitProbate, logging.silentLogging

      val fury = Discovery.Service(t"fury", Tcp)
      val elsinore = Discovery.Description(t"Elsinore", tcp"8443")
frame
  label The block for which it is advertised
  highlight fury.advertise(elsinore):  claimed here, and withdrawn where the block ends
  code
      import socketBackends.javaBaseSockets, discoveryBackends.mdnsSockets, strategies.throwUnsafely
      import threads.virtualThreads, probates.awaitProbate, logging.silentLogging

      val fury = Discovery.Service(t"fury", Tcp)
      val elsinore = Discovery.Description(t"Elsinore", tcp"8443")

      def serve(): Text = supervise:
        fury.advertise(elsinore):
          t"serving"
frame
  label The name it was claimed with
  highlight summon[Discovery.Advertisement]  exists only inside the block
  code
      import socketBackends.javaBaseSockets, discoveryBackends.mdnsSockets, strategies.throwUnsafely
      import threads.virtualThreads, probates.awaitProbate, logging.silentLogging

      val fury = Discovery.Service(t"fury", Tcp)
      val elsinore = Discovery.Description(t"Elsinore", tcp"8443")

      def serve(): Text = supervise:
        fury.advertise(elsinore):
          summon[Discovery.Advertisement].instance.dnsName.show
##
```

The example is compiled, and is not run, since it needs the network.
[Advertising](../../soundness/modules/service-discovery.md#advertising) explains why the
claimed name may differ, and [delimited scopes](../../philosophy/delimited-scopes.md) the shape.
