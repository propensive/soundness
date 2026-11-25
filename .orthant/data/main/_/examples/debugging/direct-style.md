# The main loop of a debugger is a traversal

The VM reports breakpoints, steps and exits as events. They are not delivered to callbacks
which were registered earlier: `debug.events` is a stream, and the loop which handles them is
an ordinary `each` over it, with a pattern for each kind of event.

```scala fuel
compile
frame
  label Each batch of events, as it arrives
  highlight debug.events.each  blocks until the VM has something to say
  code
      import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

      def trace()(using Debug): Unit =
        debug.events.each: composite =>
          Out.println(t"the VM reported")
frame
  label A breakpoint among them
  highlight Jdwp.Event.Breakpoint(request,..location)  a typed value, taken apart
  code
      import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

      def trace()(using Debug): Unit =
        debug.events.each: composite =>
          composite.events.each:
            case Jdwp.Event.Breakpoint(request, thread, location) =>
              Out.println(t"stopped at ${location.index}")
            case other => ()
frame
  label And a step
  highlight Jdwp.Event.SingleStep(request,..location)  another case of the same enumeration
  code
      import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

      def trace()(using Debug): Unit =
        debug.events.each: composite =>
          composite.events.each:
            case Jdwp.Event.Breakpoint(request, thread, location) =>
              Out.println(t"stopped at ${location.index}")
            case Jdwp.Event.SingleStep(request, thread, location) =>
              Out.println(t"stepped to ${location.index}")
            case other => ()
##
```

The example is compiled, and is not run, since it needs another JVM to debug.
[Events](../../soundness/modules/debugging.md#events) describes the stream, and
[direct style](../../philosophy/direct-style.md) why it is not a set of callbacks.
