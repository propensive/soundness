# A log statement which is not read is not evaluated

The argument of a log method is passed by name, and is evaluated only if a logger will record
it. With no logger in scope, the message is never built, so the method which would have
described the scene is never called.

```scala fuel
compile
frame
  label A message which is costly to build
  highlight ${describe()}  never evaluated
  code
      import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

      def describe(): Text =
        Out.println(t"describing the scene")
        t"a heath, in a storm"

      def rehearse(): Text =
        Log.fine(m"rehearsing ${describe()}")
        t"rehearsed"
##
```

```scala fuel
run
##
Out.println(rehearse())
```

Only `rehearsed` is printed.
[Cost when disabled](../../soundness/modules/logging.md#cost-when-disabled) explains what
follows for the design of an event, and [zero cost](../../philosophy/zero-cost.md) the
principle.
