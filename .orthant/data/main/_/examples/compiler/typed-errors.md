# A format the toolchain cannot reach

A toolchain is a graph of formats, and producing an artifact is a search for a path through
it. The search can fail, and `path` says so in its type: the question "can this be built?" is
asked, and answered, before any tool runs.

```scala fuel
compile
frame
  label A route which may not exist
  errors show
  code
      def route(target: Artifact): List[Text] =
        Toolchain(jarEdges(), dexEdges(), apkEdges())
        . path(Toolchain.Universe.Classfile, target)
        . map(_.target.id)
frame
  label Declared
  highlight raises..Link.Error  no path, two paths, a duplicate edge or a cycle
  code
      def route(target: Artifact): List[Text] raises Link.Error =
        Toolchain(jarEdges(), dexEdges(), apkEdges())
        . path(Toolchain.Universe.Classfile, target)
        . map(_.target.id)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

Out.println(route(Apk).inspect)
Out.println(capture[Link.Error](route(Toolchain.Universe.Sjsir)).message.show)
```

An `Apk` is two edges from classfiles, and nothing in the request says so.
[Deliverables and paths](../../soundness/modules/compiler.md#deliverables-and-paths) describes
the search, and [error handling](../../philosophy/error-handling.md) the declaration.
