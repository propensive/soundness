# A lifetime is a time

A `Cache` holds a value for a lifetime, and the lifetime is a physical quantity. The cache
knows nothing of units, and quantities know nothing of caches, but only a quantity which is a
time can be given as a lifetime.

```scala fuel
compile
frame
  label Three hundred metres
  errors show
  code
      import durationInterfaces.aviationDuration

      val prophecy: Cache[Text] = Cache(300.0*Metre)
frame
  label Three hundred seconds
  highlight Second  a unit of time
  code
      import durationInterfaces.aviationDuration

      val prophecy: Cache[Text] = Cache(300.0*Second)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(prophecy.establish(t"Beware the ides of March"))
Out.println(prophecy.establish(t"Beware the kalends of April"))
```

The second call is within the lifetime, so it gives the value which the first one stored.
[An expiring value](../../soundness/modules/caching.md#an-expiring-value) has the details, and
[composability](../../philosophy/composability.md) the reason the two libraries fit.
