# A service name may be refused

A service type's name is checked against the limits of DNS when the `Discovery.Service` is
made, so making one can fail. The failure is a `Discovery.Error` in the constructor's type, and
a method which names a service must handle it or pass it on.

```scala fuel
compile
frame
  label The refusal is not accounted for
  errors show
  code
      def service(name: Text): Discovery.Service = Discovery.Service(name, Tcp)
frame
  label The refusal is declared
  highlight raises..Error
  code
      def service(name: Text): Discovery.Service raises Discovery.Error =
        Discovery.Service(name, Tcp)
frame
  label Or it is an absent service
  highlight safely
  code
      def service(name: Text): Optional[Discovery.Service] =
        safely(Discovery.Service(name, Tcp))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(service(t"fury").let(_.dnsName.show).or(t"refused"))
Out.println(service(t"a-very-long-service-name").let(_.dnsName.show).or(t"refused"))
```

[Services and instances](../../soundness/modules/service-discovery.md#services-and-instances)
has the limits, and [error handling](../../philosophy/error-handling.md) the reasons.
