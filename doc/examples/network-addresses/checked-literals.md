# An address with too many parts

An IP address written with `ip"…"` is parsed by the compiler. Its type is `Ipv4` or `Ipv6`,
according to what was written, and text which is neither does not compile.

```scala fuel
compile
frame
  label Six numbers
  errors show
  code
      val gateway = ip"192.168.0.0.0.1"
frame
  label Four numbers
  code
      val gateway = ip"192.168.0.1"
frame
  label The type which was inferred
  highlight Ipv4
  code
      val gateway: Ipv4 = ip"192.168.0.1"
##
```

[Network addresses](../../soundness/modules/network-addresses.md) has the same for URLs, MAC
addresses, hostnames and ports.
