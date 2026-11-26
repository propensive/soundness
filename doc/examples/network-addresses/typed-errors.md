# Each identifier fails in its own way

Decoding a hostname raises a `Hostname.Error`, and decoding an IP address an
`IpAddress.Error`. A method which does both declares both, and its caller knows exactly what
may go wrong.

```scala fuel
compile
frame
  label Nothing is declared
  errors show
  code
      def gateway(address: Text): Ipv4 = address.as[Ipv4]
frame
  label The address may be malformed
  highlight raises..Error
  code
      def gateway(address: Text): Ipv4 raises IpAddress.Error = address.as[Ipv4]
frame
  label And so may the hostname
  highlight raises..Error
    occurrence 2
  code
      def gateway(address: Text): Ipv4 raises IpAddress.Error = address.as[Ipv4]

      def route(host: Text, address: Text)
      :   (Hostname, Ipv4) raises Hostname.Error raises IpAddress.Error =
        (host.as[Hostname], gateway(address))
##
```

[Parsing at runtime](../../soundness/modules/network-addresses.md#parsing-at-runtime) has the
table of types and their errors.
