# The reason an address was refused

Text which is not an IP address raises an `IpAddress.Error`, and the error is a value. Its
reason is a case of an enumeration which holds the offending part, and its message is made
from that.

```scala fuel
compile
frame
  label The error, captured
  highlight capture[IpAddress.Error]
  code
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      val error = capture[IpAddress.Error](t"192.168.256.1".as[Ipv4])
frame
  label Its reason, as a value
  highlight Ipv4ByteOutOfRange(byte)  the number which was out of range
  code
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      val error = capture[IpAddress.Error](t"192.168.256.1".as[Ipv4])

      val excess: Int = error.reason match
        case IpAddress.Error.Reason.Ipv4ByteOutOfRange(byte) => byte - 255
        case _                                               => 0
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(error.message.show)
Out.println(excess.show)
```

[Parsing at runtime](../../soundness/modules/network-addresses.md#parsing-at-runtime) lists
the error of each identifier, and
[expressive errors](../../philosophy/expressive-errors.md) says why they are written so.
