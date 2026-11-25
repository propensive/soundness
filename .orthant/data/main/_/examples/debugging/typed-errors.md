# A request to the VM can be refused

The VM may answer any request with an error: the thread has died, the frame is gone, the
connection has dropped. Every method of a session `raises Debugger.Error`, so a method which
asks the VM something either declares that error or decides what to do about it.

```scala fuel
compile
frame
  label The refusal is not accounted for
  errors show
  code
      def title(thread: ThreadId)(using Debug): Text = debug.name(thread)
frame
  label It is declared
  highlight raises..Error  passed on to the caller
  code
      def title(thread: ThreadId)(using Debug): Text raises Debugger.Error =
        debug.name(thread)
frame
  label Or a thread which cannot be named is given a name
  highlight safely  an Optional result in place of the error
  highlight or(t"exeunt")  what its absence means here
  code
      def title(thread: ThreadId)(using Debug): Text =
        safely(debug.name(thread)).or(t"exeunt")
##
```

The example is compiled, and is not run, since it needs another JVM to debug.
[Threads and frames](../../soundness/modules/debugging.md#threads-and-frames) has the requests,
and [errors](../../soundness/modules/errors.md#choosing-a-strategy) the strategies which meet a
`Debugger.Error`.
