## Service Discovery

### About

A program which serves something on the local network, such as a build daemon, a printer or a
game, can be found without anyone being told its address.
[DNS-based service discovery](https://www.rfc-editor.org/rfc/rfc6763) names a service type,
such as `_fury._tcp`, and its instances, such as `Gondor._fury._tcp.local`.
[Multicast DNS](https://www.rfc-editor.org/rfc/rfc6762) carries those names on the link without
a nameserver. Every host answers for its own.

A service is advertised, others' services are browsed for, and an instance is resolved to its
host, port and addresses, through a `Discovery.Backend`. The backend provided here is an mDNS
responder over sockets. A later backend may delegate to a system responder such as Bonjour or
Avahi.

### On discovery

Everything comes from the `soundness` package. The second import chooses a
[strategy](errors.md#choosing-a-strategy) for errors, and the third lets the samples print their
results:

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

The samples which build names and records run here. Those which touch the network are compiled
but not run.

### Services and instances

A service type is a registered name of up to fifteen characters and a transport. An instance is
one UTF-8 label, spaces and dots included, under that type. Both compute their DNS-SD names when
they are constructed, under validation, so a name over the DNS limits is reported then, and not
on the wire:

```scala
val fury = Discovery.Service(t"fury", Tcp)
```

```scala fuel
run
##
Out.println(fury.dnsName.show)
Out.println(Discovery.Instance(t"Jon's Build Daemon", fury).dnsName.show)
```

A service name over fifteen characters is rejected:

```scala fuel
run
errors show
##
Discovery.Service(t"a-very-long-service-name", Tcp)
```

An instance says more about itself in a TXT record: `key=value` pairs, read without regard to
case, with the first occurrence of a key winning:

```scala
val txt = Discovery.Txt(t"fingerprint" -> t"ab12", t"version" -> t"0.71")
```

```scala fuel
run
##
Out.println(txt(t"Fingerprint").inspect)
Out.println(txt(t"missing").inspect)
```

### Advertising

An advertisement is a loan. The instance is probed for, claimed, and announced. If another host
holds the name, the instance is renamed, to `Gondor (2)`. It stays advertised for the block,
and a goodbye is sent when the block ends, or when the `Monitor` is canceled, which unwinds it:

```scala fuel
typecheck
##
import socketBackends.javaBaseSockets
import discoveryBackends.mdnsSockets
import threads.virtualThreads
import probates.awaitProbate
import logging.silentLogging

supervise:
  val description = Discovery.Description(t"Gondor", tcp"8443", txt)

  fury.advertise(description):
    summon[Discovery.Advertisement].instance.dnsName
```

The name inside the block is the name the instance was claimed with, which may not be the name
it asked for.

### Browsing and resolving

A browse is a loan too. It lends a `Discovery.Browser`, whose `events` are the instances
appearing on and leaving the link, as they happen, and whose `instances` are those currently
known. Resolving an instance gives its host name, port, TXT record and addresses, within a
timeout:

```scala fuel
typecheck
##
import abstractables.millisecondsAbstractable

val found: Promise[List[Endpoint[Port]]] = Promise()

supervise:
  fury.browse:
    summon[Discovery.Browser].events.each:
      case Discovery.Event.Found(instance) =>
        val resolution = instance.resolve(5000L)
        if resolution.txt(t"fingerprint") == t"ab12" then found.offer(resolution.endpoints)

      case Discovery.Event.Lost(instance) => ()
```

### The responder

`discoveryBackends.mdnsSockets` is the mDNS responder over multicast [sockets](sockets.md). It
joins `224.0.0.251:5353` and `[ff02::fb]:5353` on every interface which is up,
multicast-capable and has an address of the group's family, with address and port reuse, so it
runs beside the system's own responder. One responder per program is the intent, so bind the
given once, rather than summoning it at each use. It opens its sockets at the first loan, and
closes them after the last.

Two hosts which claim one name at once are settled as RFC 6762 prescribes. Each sees the other's
probe. The one proposing the lexicographically lower records yields, and when it probes again a
second later, the winner defends the name, so the loser is renamed. A host which turns up later,
asserting a name which this responder has established, makes it probe for the name afresh, and
rename if the probe is answered. That is why `Advertisement.instance` is read live, and not
copied.

What the responder does for a loan, which is probing, yielding, a conflict, claiming,
withdrawing, browsing and resolving, is logged as `Discovery.Activity` events through the
`Loggable` in scope at the loan. That is silent unless a [log sink](logging.md) is. Its
background work between loans is not logged.

For a program under test, `Mdns.Transport.Bus` is an in-memory link. Responders joined to one
bus hear one another and nothing else, so the protocol, with its probing, renaming and goodbyes,
runs deterministically without a network.

### Errors

Validation, a name which cannot be claimed, a resolution which times out, and a responder which
cannot start are each a `Discovery.Error`, whose reason says which. The responder's own failure
to join the group is an `Mdns.Error`.

### Related

- [Sockets](sockets.md) describes the multicast sockets which the responder uses.
- [Network addresses](network-addresses.md) describes the ports and addresses which a
  resolution gives.
- [Daemons](daemons.md) describes the resident processes which are often advertised.
- [Concurrency](concurrency.md) describes `supervise`, `Promise` and the `Monitor` which
  unwinds a loan.
- [Logging](logging.md) receives the `Discovery.Activity` events.
