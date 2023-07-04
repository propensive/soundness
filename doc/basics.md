### Creating an IP address

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

### Parsing an IP address

The same parser can be used at runtime, throwing an `IpAddressError` in the event of an incorrect address.

To parse an IP address from a `Text` value, use the `parse` method of either the `Ipv4` or `Ipv6` objects,
for example,
```scala
val address1: Ipv4 = Ipv4.parse(t"192.168.1.12")
val address2: Ipv6 = Ipv6.parse(ipAddressText)
```

### Serializing IP addresses

Both `Ipv4` and `Ipv6` addresses have `Show` instances, which will serialize these addresse to `Text` values.
IPv4 addresses will be serialized, unsurprisingly, as four integers interspersed with `.` characters, and
IPv6 addresses will be serialized according to the
[IETF's recommended canonical representation](https://datatracker.ietf.org/doc/html/rfc5952), which includes
various simplifications.

