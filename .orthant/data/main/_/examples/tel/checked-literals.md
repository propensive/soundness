# A path which must say how it is divided

The first character of a TEL path is its delimiter, and the rest is read by it. A `telp"…"`
literal is parsed by the compiler, so a path which begins with a keyword does not compile, and
two paths which differ only in their delimiter are equal.

```scala fuel
compile
frame
  label A path with no delimiter
  errors show
  code
      val city = telp"castle/city"
frame
  label A path divided by slashes
  highlight /  the delimiter
  code
      val city = telp"/castle/city"
frame
  label The same path, divided by dots
  highlight .castle.city  equal to the first
  code
      val city = telp"/castle/city"
      val same = city == telp".castle.city"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(same.inspect)
```

[Paths](../../soundness/modules/tel.md#paths) says what else a component of a path may select.
