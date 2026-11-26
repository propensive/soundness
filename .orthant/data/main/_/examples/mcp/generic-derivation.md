# A method is a tool, and its signature is the schema

An MCP server describes each tool to its client with a JSON schema for the input and another
for the result. Neither is written here. A method marked `@tool` is the tool, and both schemas
are derived from its parameter and result types, case classes included.

```scala fuel
compile
frame
  label A tool which takes text and returns a number
  focus @tool..5
  highlight (play:..Int  the input schema and the result schema
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
  label A tool which returns a case class
  focus case..Hamlet")
  highlight Role  its schema is derived from its fields
  code
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      case class Role(name: Text, lines: Int)

      object Almanac extends Mcp.Server():
        class Session() extends Mcp.Session
        def initialize(): Session = new Session()
        def name: Text = t"almanac"
        def description: Text = t"The plays and their casts"
        def version: Semver = v"1.0.0"
        def prompts: List[Mcp.Prompt] = Nil

        @tool
        @about(t"Name the longest part in a play")
        def lead(play: Text): Role = Role(t"Hamlet", 1506)
##
```

The example is compiled, and is not run, since a server speaks to a client over HTTP.
[A server](../../soundness/modules/mcp.md#a-server) describes the annotations, and
[JSON](../../soundness/modules/json.md#schemas) the schemas which are derived.
