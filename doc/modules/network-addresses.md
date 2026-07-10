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

t"https://example.com/".decode[HttpUrl]
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

### Email addresses

An email address is written with `email"…"` and parsed from text with `decode`, validated against the
rules for a well-formed address:

```scala
email"test@example.com"
t"simple@example.com".decode[EmailAddress]
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

### MAC addresses

A MAC address is written with `mac"…"` and decoded from text, validated as six hexadecimal groups:

```scala
mac"01-23-45-ab-cd-ef"
```

### Parsing at runtime

Every identifier that has a literal form also decodes from text with `decode`, naming the target type.
A value that does not conform raises a typed error — an `HostnameError`, an `IpAddressError`, an
`EmailAddressError` — that names precisely what was wrong, so a program validating user input can
report the fault rather than merely rejecting the value.
