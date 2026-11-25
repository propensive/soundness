# A parameter the model may leave out

A tool's input schema says which parameters are required. That too comes from the signature: a
parameter is required unless its type is an `Optional` or it has a default. The tool then sees
`Unset`, or the default, and says what an absent value means.

```scala fuel
compile
frame
  label Both parameters are required
  focus @tool..Nil
  highlight act:..Int  the model must supply it
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
        @about(t"List the scenes of a play")
        def scenes(play: Text, act: Int): List[Text] = Nil
frame
  label The act may be omitted
  focus @tool..Nil
  highlight Optional[Int]  no longer required by the schema
  highlight act.absent  the tool decides what omission means
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
        @about(t"List the scenes of a play")
        def scenes(play: Text, act: Optional[Int], limit: Int = 20): List[Text] =
          if act.absent then List(t"every scene of $play") else Nil
##
```

The example is compiled, and is not run, since a server speaks to a client over HTTP.
[Talking back to the client](../../soundness/modules/mcp.md#talking-back-to-the-client)
describes required parameters, and [optionality](../../philosophy/optionality.md) the type
which marks the others.
