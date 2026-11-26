# The responder is chosen by an import

Browsing for a service says nothing of how the network is asked. That is the work of a
`Discovery.Backend`, chosen once by an import, and code with no backend in scope cannot browse.

```scala fuel
compile
frame
  label No backend is in scope
  errors show
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely
      import threads.virtualThreads, probates.awaitProbate, logging.silentLogging

      val fury = Discovery.Service(t"fury", Tcp)

      def daemons(): Set[Discovery.Instance] = supervise:
        fury.browse(summon[Discovery.Browser].instances)
frame
  label Multicast DNS, over sockets
  highlight discoveryBackends.mdnsSockets
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely
      import threads.virtualThreads, probates.awaitProbate, logging.silentLogging
      import discoveryBackends.mdnsSockets

      val fury = Discovery.Service(t"fury", Tcp)

      def daemons(): Set[Discovery.Instance] = supervise:
        fury.browse(summon[Discovery.Browser].instances)
##
```

The example is compiled, and is not run, since it needs the network.
[The responder](../../soundness/modules/service-discovery.md#the-responder) describes this
backend, and [declarative context](../../philosophy/declarative-context.md) the choice.
