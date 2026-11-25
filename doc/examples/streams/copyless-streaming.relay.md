# One reader for many producers

A `Relay` carries records from the parts of a program which produce them to the one part which
reads them. Any number of producers `put`, and none of them ever blocks; a single reader owns
the stream, and each refill takes every record which has arrived since the last, in one window.

```scala fuel
compile
frame
  label Records put by their producers
  highlight relay.put
  code
      val relay = Relay[Text]()
      relay.put(t"Enter three Witches")
      relay.put(t"Exeunt")
      relay.stop()
frame
  label The single stream which reads them
  highlight relay.stream
  code
      val relay = Relay[Text]()
      relay.put(t"Enter three Witches")
      relay.put(t"Exeunt")
      relay.stop()

      val directions = relay.stream.records.to[List]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(directions.inspect)
```

[Producing a stream over time](../../soundness/modules/streams.md#producing-a-stream-over-time)
describes relays, and conduits, which do apply backpressure.
