# One method, a tool for MCP and for a model

An MCP server offers tools to whichever assistant connects. A `Toolkit` offers them to a model
which this program is talking to itself. The same method serves both: it is marked for each,
and described once, since `@about` is shared between the two libraries.

```scala fuel
compile
frame
  label A tool of an MCP server
  focus @tool..5
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
        @about(t"Count the acts of a play")
        def acts(play: Text): Int = 5
frame
  label The same method, as an ability of a model
  focus @tool..(Almanac)
  highlight @ability  marks it for a Toolkit
  highlight Toolkit(Almanac)  built from the server object itself
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
        @ability
        @about(t"Count the acts of a play")
        def acts(play: Text): Int = 5

      val toolkit: Toolkit = Toolkit(Almanac)
##
```

The example is compiled, and is not run, since a server speaks to a client over HTTP.
[Tools](../../soundness/modules/llm.md#tools) describes a toolkit, and
[composability](../../philosophy/composability.md) the idea of libraries which meet without an
adapter.
