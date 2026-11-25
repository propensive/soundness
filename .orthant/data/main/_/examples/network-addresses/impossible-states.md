# A subnet has no host bits

A subnet is an address and a prefix length. The length is checked against the family of the
address, and the bits below the prefix are cleared, so a subnet which was written carelessly
is the subnet that was meant.

```scala fuel
compile
frame
  label A prefix longer than an IPv4 address
  errors show
  code
      val network = subnet"10.0.16.0/33"
frame
  label A prefix within the range
  highlight /20
  code
      val network = subnet"10.0.16.0/20"
frame
  label An address within the subnet
  highlight 16.77  the last twelve bits are masked away
  code
      val network = subnet"10.0.16.77/20"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(network.show)
```

[Subnets](../../soundness/modules/network-addresses.md#subnets) has the IPv6 form.
