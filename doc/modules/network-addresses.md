## Network Addresses

### About

The identifiers of a network — [URLs](https://en.wikipedia.org/wiki/URL),
[hostnames](https://en.wikipedia.org/wiki/Hostname),
[IP addresses](https://en.wikipedia.org/wiki/IP_address),
[email addresses](https://en.wikipedia.org/wiki/Email_address), ports and
[MAC addresses](https://en.wikipedia.org/wiki/MAC_address) — each have their own type in Soundness.
A literal is validated as the code compiles, so a malformed one is a compile error where it is
written; and text parses into the same types at runtime, reporting a typed error when it does not
conform.

### On network identifiers

A URL, an IP address and an email address are, in most code, all just strings. The distinctions
between them exist only in the programmer's head, and a value that is not really a well-formed address
travels through the program until something tries to use it and fails — a validation done late, if at
all, and far from where the bad value entered.

An address that cannot be constructed invalid is [safety by construction](../philosophy/safety-by-construction.md) in its simplest form.

Soundness gives each identifier its own type, and validates it at the earliest possible moment: a
literal as it compiles, and text on the instant it is decoded. A `Hostname`, an `EmailAddress`, a
`Port` are known to be well-formed, and their parts are typed too — a URL's port is a number, its host
a hostname or an IP address. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### URLs

The `url"…"` interpolator writes a URL and checks it as the code compiles, and text decodes to an
`HttpUrl` at runtime. A URL's parts — its scheme, host, port, path, query and fragment — are available
as typed members:

```scala
url"https://example.com:8080/path?query=1#top"

t"https://example.com/".as[HttpUrl]
```

A hole in the interpolator substitutes a value in the right place — a number becomes the port, a text
value is URL-encoded into the path.

### Hostnames and IP addresses

A hostname and an IPv4 or IPv6 address are written with `host"…"` and `ip"…"`, each validated as it
compiles. An IP address yields a subnet, which renders in the usual slash notation, and `subnet"…"`
writes one directly:

```scala
host"www.example.com"
ip"192.168.0.1"
ip"2001:db8::1"

ip"255.123.143.0".subnet(12).show   // t"255.112.0.0/12"
```

An address that is not valid does not compile:

```scala
ip"192.168.0.0.0.1"   // does not compile: too many groups
```

The `ip"…"` interpolator serves both address families, taking its type from what it is given, and
the checks are the full ones for each. For IPv4: exactly four groups, each a number in the range
0–255, with nothing but digits and dots. For IPv6: at most eight colon-separated groups, at most
one `::`, and one to four hexadecimal characters in each group. A subnet prefix is checked against
its family's range too — 0–32 or 0–128 — so an impossible mask is caught with the address.

### Email addresses

An email address is written with `email"…"` and parsed from text with `as`, validated against the
rules for a well-formed address:

```scala
email"test@example.com"
t"simple@example.com".as[EmailAddress]
```

### Ports

A port is written with `tcp"…"` or `udp"…"`, and its type records both the transport and, for a
literal, the number. A port may be named by its [IANA service name](https://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xhtml)
rather than its number, resolved as the code compiles:

```scala
tcp"smtp"   // the TCP port 25
tcp"443"
```

An unused ephemeral port is obtained from the operating system with `Port[Tcp]()`.

### Subnets

A subnet is written with its prefix length, and checked as the code compiles. Host bits below the
prefix are masked away, so a subnet written carelessly is corrected rather than misinterpreted:

```scala
subnet"192.168.0.0/24"
subnet"255.123.143.0/12".show   // t"255.112.0.0/12" — the host bits masked
subnet"2001:db8::/32"
```

A subnet answers whether an address falls within it, which is what an access rule or a
routing decision needs, and it is a value rather than a pair of strings to compare by hand.

### Named services

The well-known services have names, and those names are checked against the service registry —
including which transport each is registered for. Asking for a service over the wrong transport
does not compile:

```scala
tcp"smtp"      // Port[Tcp](25)
udp"docker"    // does not compile: Docker is registered over TCP
```

### Interfaces and ephemeral ports

The machine's own network interfaces enumerate as typed values, each reporting whether it is a
loopback, and each addressable by name:

```scala
NetworkInterface.all()
NetworkInterface.all().exists(_.loopback)
```

`Port[Tcp]()` with no number allocates an unused port, which is what a test server or a
dynamically-bound service needs — and the port it returns is one that can actually be bound,
rather than a guess that may race with another process.

### MAC addresses

A MAC address is written with `mac"…"` and decoded from text, validated as six hexadecimal groups:

```scala
mac"01-23-45-ab-cd-ef"
```

### Parsing at runtime

Every identifier that has a literal form also decodes from text with `as`, naming the target type.
A value that does not conform raises a typed error — an `Hostname.Error`, an `IpAddress.Error`, an
`EmailAddress.Error` — that names precisely what was wrong, so a program validating user input can
report the fault rather than merely rejecting the value.
