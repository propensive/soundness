# A serializer which belongs to its scope

`Json.emit` serializes a document on a fiber, and hands out the text through an iterator as
the consumer asks for it. A fiber must have a supervisor, so the call does not compile outside
a `supervise` block, and the block does not end while the fiber runs.

```scala fuel
compile
frame
  label A fiber with nothing to supervise it
  errors show
  code
      import threads.virtualThreads, probates.cancelProbate
      import strategies.throwUnsafely, formatting.compactJsonFormatting

      val play: Json = j"""{"title": "Pericles", "acts": 5}"""
      val blocks: List[Text] = Json.emit(play).to[List]
frame
  label The same fiber, in a scope
  highlight supervise  the fiber cannot outlive this block
  code
      import threads.virtualThreads, probates.cancelProbate
      import strategies.throwUnsafely, formatting.compactJsonFormatting

      val play: Json = j"""{"title": "Pericles", "acts": 5}"""
      val blocks: List[Text] = supervise(Json.emit(play).to[List])
##
```

[Formatting](../../soundness/modules/json.md#formatting) compares the forms of output, and
[structured concurrency](../../philosophy/structured-concurrency.md) explains supervision.
