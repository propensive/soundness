## Network Addresses

### About

The identifiers of a network each have a type of their own:
[URLs](https://en.wikipedia.org/wiki/URL),
[hostnames](https://en.wikipedia.org/wiki/Hostname),
[IP addresses](https://en.wikipedia.org/wiki/IP_address),
[email addresses](https://en.wikipedia.org/wiki/Email_address), ports and
[MAC addresses](https://en.wikipedia.org/wiki/MAC_address).

A literal is validated at [compiletime], so a malformed one is a compile error where it is
written. Text is parsed into the same types at [runtime], and a typed error is reported when
it does not conform.

### On network identifiers

In most code, a URL, an IP address and an email address are all just strings. The
distinctions between them exist only in the programmer's head. A value which is not really a
well-formed address travels through the program until something tries to use it, and fails.
The validation is done late, if at all, and far from where the bad value came in.

Here each identifier has a type of its own, and is validated at the earliest possible
moment: a literal as it compiles, and text at the instant it is decoded. A `Hostname`, an
`EmailAddress` and a `Port` are known to be well-formed. Their parts are typed too: a URL's
port is a number, and its host is a hostname or an IP address. An address which cannot be
constructed invalid is [safety by construction](../philosophy/safety-by-construction.md) in
its simplest form.

Everything comes from the `soundness` package. The samples throw any error as an exception,
and the last import lets them print their results:

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### URLs

The `url"…"` [interpolator] writes a URL, and checks it at compiletime:

```scala
val address = url"https://example.com:8080/path?query=1#top"
```

A URL's parts are typed members: its scheme, host, port, path, query and fragment. A
substitution puts a value in the right place. A number becomes the port, and a text value is
URL-encoded into the path.

A malformed URL does not compile:

```scala fuel
compile
errors show
##
url"https://example.com:port/"
```

Text is decoded to an `HttpUrl` at runtime, with `as`:

```scala fuel
run
##
Out.println(t"https://example.com/".as[HttpUrl].show)
```

The [HTTP client](http-client.md) sends requests to URLs, and [Paths](paths.md) describes
the hierarchy which a URL's path shares with filesystem paths.

### Hostnames and IP addresses

A hostname is written with `host"…"`, and an IPv4 or IPv6 address with `ip"…"`. Each is
validated as it compiles:

```scala
val host = host"www.example.com"
val ipv4 = ip"192.168.0.1"
val ipv6 = ip"2001:db8::1"
```

An address which is not valid does not compile:

```scala fuel
compile
errors show
##
ip"192.168.0.0.0.1"
```

The `ip"…"` interpolator serves both families of address. It takes its type, `Ipv4` or
`Ipv6`, from what it is given, and the checks are the full ones for each. The prefix of a
subnet is checked against its family's range too, so an impossible mask is caught with the
address:

```tabular
column  Rule
column  IPv4
column  IPv6
row  groups  exactly four, separated by dots  at most eight, separated by colons
row  each group  a number in the range 0–255  one to four hexadecimal characters
row  abbreviation  none; nothing but digits and dots  at most one `::`
row  subnet prefix  0–32  0–128
```

### Subnets

An IP address gives a subnet, which is written out in the usual slash notation, and
`subnet"…"` writes one directly, with its prefix length, checked at compiletime. The host bits
below the prefix are masked away, so a subnet which is written carelessly is corrected, not
misinterpreted:

```scala fuel
run
##
Out.println(ip"255.123.143.0".subnet(12).show)
Out.println(subnet"255.123.143.0/12".show)
Out.println(subnet"192.168.0.0/24".show)
Out.println(subnet"2001:db8::/32".show)
```

A subnet answers whether an address falls within it, which is what an access rule or a
routing decision needs. It is a value, not a pair of strings to compare by hand.

### Email addresses

An email address is written with `email"…"`, and is parsed from text with `as`. Both validate
it against the rules for a well-formed address:

```scala
val sender = email"test@example.com"
```

```scala fuel
run
##
Out.println(t"simple@example.com".as[EmailAddress].show)
```

```scala fuel
run
errors show
##
t"not an address".as[EmailAddress]
```

[Email](email.md) sends messages to such addresses.

### Ports

A port is written with `tcp"…"` or `udp"…"`. Its type records the transport and, for a
literal, the number. A port may be named by its
[IANA service name](https://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xhtml)
instead of its number, and the name is resolved at compiletime:

```scala fuel
run
##
Out.println(tcp"smtp".inspect)
Out.println(tcp"443".inspect)
```

The names are checked against the registry of services, including which transport each is
registered for. Asking for a service over the wrong transport does not compile:

```scala fuel
compile
errors show
##
udp"docker"
```

`Port[Tcp]()`, with no number, asks the operating system for a port which is not in use. That
is what a test server needs, or a service which binds a port dynamically. The port which it
returns is one which can actually be bound, not a guess which may race with another process.
[Sockets](sockets.md) binds and connects to ports.

### Interfaces

The machine's own network interfaces are listed as typed values. Each reports whether it is a
loopback, and each can be addressed by name:

```scala fuel
typecheck
##
val interfaces = NetworkInterface.all()
val loopback: Boolean = NetworkInterface.all().exists(_.loopback)
```

### MAC addresses

A MAC address is written with `mac"…"`, or decoded from text. It is validated as six
hexadecimal groups:

```scala
val card = mac"01-23-45-ab-cd-ef"
```

```scala fuel
compile
errors show
##
mac"01-23-45-ab-cd"
```

### DNS names and messages

The DNS has names of its own. Any label may hold any UTF-8, including dots and spaces, which
is what the instance names of DNS-SD services, such as `Jon's Printer._ipp._tcp.local`, rely
on. A `Dns.Name` is written with `dns"…"`, and checked at compiletime. Equality ignores the
case of ASCII letters, as the DNS itself compares names, while the labels keep their
spelling:

```scala fuel
run
##
Out.println((dns"Example.COM" == dns"example.com").inspect)
Out.println(dns"_fury._tcp.local".labels.inspect)
```

The messages of RFC 1035 are questions, and records which hold typed data over the addresses
above. They are encoded to `Data` and decoded from it with the usual `in` and `as`. Names are
compressed on the way out, and strictly validated on the way in. A record's data is one of the
recognized types, or an `Unknown` which holds its bytes:

```scala
val record = Dns.Record(dns"example.com", 3600, Dns.Rdata.A(ip"93.184.216.34"))
val query = Dns.Message.query(1, List(Dns.Question(dns"example.com", Dns.Type.A))).in[Data]
```

mDNS adds two bits: a question which accepts a unicast response, and a record which flushes a
receiver's cache. They are plain fields, `unicast` and `flush`, which default to off.
`Dns.resolve` reaches the platform's resolver, and
[Service discovery](service-discovery.md) builds on these messages.

### Parsing at runtime

Every identifier which has a literal form is also decoded from text with
[`as`](decoding.md#decoding-a-value), naming the target type. A value which does not conform
raises a typed error, which names precisely what was wrong. A program which validates a user's
input can therefore report the fault, and need not merely reject the value:

```tabular
column  Identifier
column  Literal
column  Type
column  Error
row  URL  `url"…"`  `Url`, or `HttpUrl` for HTTP  `Url.Error`
row  hostname  `host"…"`  `Hostname`  `Hostname.Error`
row  IP address  `ip"…"`  `Ipv4` or `Ipv6`  `IpAddress.Error`
row  subnet  `subnet"…"`  `Ipv4Subnet` or `Ipv6Subnet`  `IpAddress.Error`
row  email address  `email"…"`  `EmailAddress`  `EmailAddress.Error`
row  port  `tcp"…"`, `udp"…"`  `Port`  `Port.Error`
row  MAC address  `mac"…"`  `MacAddress`  `MacAddress.Error`
row  DNS name  `dns"…"`  `Dns.Name`  `Dns.Error`
```

```scala fuel
run
errors show
##
t"256.1.1.1".as[Ipv4]
```

A field of one of these types in a case class is decoded in the same way, so a `Hostname` in a
configuration file is validated as the [JSON](json.md) or [YAML](yaml.md) is read.

### Related

- [Forms](forms.md), [geography](geography.md), [HTML](html.md) and [interpolation](interpolation.md) check and carry URLs and addresses.
