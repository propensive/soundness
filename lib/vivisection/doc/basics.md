### The JDWP protocol vocabulary

The `Jdwp` object holds a Scala-native model of the Java Debug Wire Protocol: identifier types
(`Jdwp.ObjectId`, `Jdwp.ThreadId`, …, all tagged views of the same opaque `Jdwp.Ref`), a
`Jdwp.Location`, tagged `Jdwp.Value`s, event-request `Jdwp.Modifier`s, and the composite
`Jdwp.Event`s a suspended VM sends back. `Jdwp.Reader` and `Jdwp.Writer` are hand-written
big-endian codecs, aware of the identifier sizes negotiated with the VM at the start of a session.
