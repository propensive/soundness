# A service on the wrong transport

A port may be written by the name of its service, which the compiler looks up in the IANA
registry. The registry also says which transport a service uses, and Docker is registered for
TCP alone.

```scala fuel
compile
frame
  label Docker, over UDP
  errors show
  code
      val docker = udp"docker"
frame
  label Docker, over TCP
  highlight tcp
  code
      val docker = tcp"docker"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(docker.inspect)
```

[Ports](../../soundness/modules/network-addresses.md#ports) has more, and
[safety by construction](../../philosophy/safety-by-construction.md) the reasons.
