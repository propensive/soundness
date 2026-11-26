# A nameserver, written as it is said

An endpoint is an address *on* a port, and each of the three parts of a DNS query is a literal
which the compiler has checked: the IP address, the name of the service, and the name which is
asked for.

```scala fuel
compile
frame
  label An address
  code
      val resolver = ip"9.9.9.9"
frame
  label On a port, named by its service
  highlight on  an Endpoint of a UDP port
  code
      val resolver = ip"9.9.9.9" on udp"domain"
frame
  label Asked for a name
  highlight dns"globe.example.com"
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely, logging.silentLogging
      import internetAccess.online

      val resolver = ip"9.9.9.9" on udp"domain"
      val addresses = resolver.ask(dns"globe.example.com").map(_.rdata)
##
```

The example is compiled, and is not run, since it would send a query.
[Datagram exchanges](../../soundness/modules/sockets.md#datagram-exchanges) describes `ask`.
