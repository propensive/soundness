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




