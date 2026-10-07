## Service Discovery

### About

A program that serves something on the local network — a build daemon, a printer, a game — can
be found without anyone being told its address. [DNS-based service
discovery](https://www.rfc-editor.org/rfc/rfc6763) names a service type (`_fury._tcp`) and its
instances (`Gondor._fury._tcp.local`), and [multicast DNS](https://www.rfc-editor.org/rfc/rfc6762)
carries those names on the link without a nameserver: every host answers for its own. The
`syndesis` library advertises a service, browses for others', and resolves an instance to its
host, port and addresses, through a `Discovery.Backend` — the socket-based mDNS responder
provided here, or a system responder (Bonjour, Avahi) a later backend may delegate to.

### Services and instances

A service type is a registered name of up to fifteen characters and a transport; an instance is
one UTF-8 label — spaces and dots included — under that type. Both compute their DNS-SD names
when constructed, under validation, so a name over the DNS limits is reported then rather than
on the wire:

```scala
val fury = Discovery.Service(t"fury", Tcp)
fury.dnsName
Discovery.Instance(t"Jon's Build Daemon", fury).dnsName
```

An instance says more about itself in a TXT record: `key=value` pairs, read case-insensitively,
the first occurrence of a key winning:

```scala
val txt = Discovery.Txt(t"fingerprint" -> t"ab12", t"version" -> t"0.71")
txt(t"Fingerprint")
```

### Advertising

An advertisement is a loan: the instance is probed for, claimed (renamed `Gondor (2)` if another
host holds the name), and announced; it stays advertised for the block, and a goodbye is sent
when the block ends — or when the `Monitor` is cancelled, which unwinds it.

<!-- doccheck: skip -->
```scala
import socketBackends.javaBaseSockets
import discoveryBackends.mdnsSockets

supervise:
  val description = Discovery.Description(t"Gondor", tcp"8443", txt)

  fury.advertise(description):
    serve()   // the instance is `summon[Discovery.Advertisement].instance`
```

### Browsing and resolving

A browse is a loan too, lending a `Discovery.Browser` whose `events` are the instances appearing
and leaving the link, as they happen, and whose `instances` are those currently known. Resolving
an instance yields its host name, port, TXT record and addresses, within a timeout:

<!-- doccheck: skip -->
```scala
supervise:
  fury.browse:
    summon[Discovery.Browser].events.each:
      case Discovery.Event.Found(instance) =>
        val resolution = instance.resolve(5000L)
        if resolution.txt(t"fingerprint") == t"ab12" then connect(resolution.endpoints)

      case Discovery.Event.Lost(instance) => ()
```

### The responder

`discoveryBackends.mdnsSockets` is the mDNS responder over the multicast sockets of `coaxial`:
it joins `224.0.0.251:5353` and `[ff02::fb]:5353` on every interface that is up,
multicast-capable and has an address of the group's family, with address and port reuse, so it
runs beside the system's own responder. One responder per program is the intent, so bind the
given once rather than summoning it at each use; it opens its sockets at the first loan and
closes them after the last.

Two hosts claiming one name at once is settled as RFC 6762 prescribes: each sees the other's
probe, the one proposing the lexicographically lower records yields, and when it probes again a
second later the winner defends the name, so the loser is renamed. A host that turns up later
asserting a name this responder has established makes it probe for the name afresh, and rename
if the probe is answered — which is why `Advertisement.instance` is read live rather than
copied.

For a program under test, `Mdns.Transport.Bus` is an in-memory link: responders joined to one
bus hear one another and nothing else, so the protocol — probing, renaming, goodbyes — runs
deterministically without a network.

### Errors

Validation, a name that cannot be claimed, a resolution that times out, and a responder that
cannot start are a `Discovery.Error`, whose reason says which; the responder's own failure to
join the group is an `Mdns.Error`.
