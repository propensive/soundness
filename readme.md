[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/nettlesome/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/nettlesome/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Nettlesome

__Typesafe representations of network concepts__

We need to work with a variety of different types of entity when working with
networking technologies. These include URLs, IP addresses, email address and
MAC addresses. Parsing and serializing these entities without loss of
generality is typically nontrivial, and it is for this reason that _Nettlesome_
exists.

## Features

- Represent URLs, MAC addresses and IP addresses with immutable datatypes
- IPv4 addresses and MAC addresses are represented efficiently as opaque primitives
- Compile-time parsing of URLs, MAC addresses and IP addresses with string interpolators
- Runtime parsing and serialization, with parsing errors handled using capabilities


## Availability

Nettlesome has not yet been published as a binary.

## Getting Started

### Entity types

Nettlesome is capable of representing all of the following:
- IPv4 addresses as `Ipv4`s
- IPv6 addresses as `Ipv6`s
- URLs as `Url`s
- hostnames as `Hostname`s
- MAC addresses as `MacAddress`s

Email addresses will be added later.

### IP Addresses

#### Construction

To create an IP address value, either an `Ipv4` or `Ipv6`, from a known string, such as `192.168.1.12`
or `2000:abcd:1234::5:12`, simply write it inside quotes prefixed with `ip`, like so:
```scala
val address1: Ipv4 = ip"192.168.1.12"
val address2: Ipv6 = ip"2000:abcd:1234::5:12"
```

The same `ip` interpolator is used for both types, and the return type will adapt to the content.

A mistake in the content will result in a compile error, for example `ip"192.168.1.2.3"` will fail to
compile because there are five `.`-separated integers, rather than four.

In particular, for IPv4 addresses, the parser checks for,
 - exactly four groups
 - only integers in the range 0-255
 - no non-integer characters, other than `.`

and for IPv6 addresses,
 - a maximum of eight `:`-separated groups
 - at most one `::` separator
 - between one and four hexadecimal characters for each group

#### Parsing

The same parser can be used at runtime, throwing an `IpAddressError` in the event of an incorrect address.

To parse an IP address from a `Text` value, use the `parse` method of either the `Ipv4` or `Ipv6` objects,
for example,
```scala
val address1: Ipv4 = Ipv4.parse(t"192.168.1.12")
val address2: Ipv6 = Ipv6.parse(ipAddressText)
```

#### Serializing

Both `Ipv4` and `Ipv6` addresses have `Show` instances, which will serialize
these addresse to `Text` values.  IPv4 addresses will be serialized,
unsurprisingly, as four integers interspersed with `.` characters, and IPv6
addresses will be serialized according to the
[IETF's recommended canonical representation](https://datatracker.ietf.org/doc/html/rfc5952),
which includes various simplifications.

### URLs

#### Construction

A URL instance can be defined using the `url""` interpolator, which will check
the URL's syntax at compiletime, reporting any parsing issues as compile errors. For example:
```scala
val url = url"https://github.com/propensive/nettlesom/"
```

It is also possible to construct a `Url` instance directly using the various
immutable datatypes from which it is composed, but it's rare that this would be
preferable.

#### Parsing

If the URL is not known at compiletime, but is available at runtime as a `Text`
string, it can be parsed. For example,
```scala
val url = Url.parse(urlText)
```

If an invalid value is passed to the `parse` method, then an error will be
raised. If using [Perforate](https://github.com/propensive/perforate/) then
there are a variety of ways in which such errors can be handled.




## Status

Nettlesome is classified as __embryonic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Nettlesome is designed to be _small_. Its entire source code currently consists
of 937 lines of code.

## Building

Nettlesome can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Nettlesome are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/nettlesome/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Nettlesome easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Nettlesome was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

_Nettlesome_ provides the slightly irritating (or _nettlesome_) task of working with _network_ addresses,
which it alludes to in its first three letters.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo shows a nettle leaf.

## License

Nettlesome is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
