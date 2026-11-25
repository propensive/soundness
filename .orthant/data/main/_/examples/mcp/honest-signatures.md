# A tool which speaks to its client asks for one

Most tools compute a result and return it. One which reports progress, or asks the user a
question while it runs, needs the client, and names it as a contextual parameter. The client
is not part of the input schema, so the model is never asked to supply it.

```scala fuel
compile
frame
  label A tool which only answers
  focus @tool..play"
  code
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      object Almanac extends Mcp.Server():
        class Session() extends Mcp.Session
        def initialize(): Session = new Session()
        def name: Text = t"almanac"
        def description: Text = t"The plays and their casts"
        def version: Semver = v"1.0.0"
        def prompts: List[Mcp.Prompt] = Nil

        @tool
        def stage(play: Text): Text = t"staged $play"
frame
  label A tool which reports as it works
  focus @tool..play"
  highlight (using..Client)  supplied by the server for each call
  highlight client.log  a progress notification, sent during the call
  code
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      object Almanac extends Mcp.Server():
        class Session() extends Mcp.Session
        def initialize(): Session = new Session()
        def name: Text = t"almanac"
        def description: Text = t"The plays and their casts"
        def version: Semver = v"1.0.0"
        def prompts: List[Mcp.Prompt] = Nil

        @tool
        def stage(play: Text)(using client: Mcp.Client): Text =
          client.log(t"Raising the curtain on $play")
          t"staged $play"
##
```

The example is compiled, and is not run, since a server speaks to a client over HTTP.
[Talking back to the client](../../soundness/modules/mcp.md#talking-back-to-the-client) has
`log` and `elicit`, and [honest signatures](../../philosophy/honest-signatures.md) the
principle.
