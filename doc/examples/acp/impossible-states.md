# A client offers what it has handlers for

An ACP client tells the agent what it can provide: files to read, files to write, terminals.
That declaration is not written here. It is derived from the handlers which are registered, so
an agent is never invited to ask for something the client cannot answer.

```scala fuel
compile
frame
  label A client which only watches
  highlight Acp.updated:  needs no capability
  code
      import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

      def handlers(script: Text)(using Acp.Registry): Unit =
        Acp.updated:
          Acp.update match
            case Acp.Plan(entries) => entries.each { entry => Out.println(entry.content) }
            case other             => ()
frame
  label It can be asked for a file, and says so
  highlight Acp.readFile:  the client now reports that it reads files
  code
      import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

      def handlers(script: Text)(using Acp.Registry): Unit =
        Acp.updated:
          Acp.update match
            case Acp.Plan(entries) => entries.each { entry => Out.println(entry.content) }
            case other             => ()

        Acp.readFile:
          if Acp.path == t"/globe/hamlet.txt" then script else t""
##
```

The example is compiled, and is not run, since the handlers serve an agent in another process.
[Connecting](../../soundness/modules/acp.md#connecting) says how the declaration is derived, and
[impossible states](../../philosophy/impossible-states.md) why it is not written by hand.
