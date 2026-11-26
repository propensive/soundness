# A port's number is in its type

A port literal has a type which records its transport and its number, as `Port over Tcp of
443`. A service name is resolved to that number as the code compiles, so the type knows what
`https` means.

```scala fuel
compile
frame
  label The type of a numbered port
  highlight Port..443
  code
      val secure: Port over Tcp of 443 = tcp"443"
frame
  label The same type, from a service name
  highlight tcp"https"  resolved in the registry of services
  code
      val secure: Port over Tcp of 443 = tcp"https"
frame
  label A number which is not that service's
  errors show
  code
      val secure: Port over Tcp of 80 = tcp"https"
##
```

[Ports](../../soundness/modules/network-addresses.md#ports) describes the literals.
