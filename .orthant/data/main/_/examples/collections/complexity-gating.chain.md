# The size of something which may never end

A `Chain` is lazy, and may be infinite. Counting it forces all of it, so `size` does not
compile until an import admits that the count may never finish. Taking a prefix needs no such
admission.

```scala fuel
compile
frame
  label Counting the natural numbers
  errors show
  code
      val naturals: Chain[Int] = Chain.iterate(0)(_ + 1)
      val count = naturals.size
frame
  label Taking the first five
  highlight keep(5)  forces five elements and no more
  code
      val naturals: Chain[Int] = Chain.iterate(0)(_ + 1)
      val first = naturals.keep(5).to[List]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(first.inspect)
```

[Lazy chains](../../soundness/modules/collections.md#lazy-chains) has the operations which
leave the rest of a chain alone.
