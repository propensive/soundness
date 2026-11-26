# Frames resolved to their source, by an import

Demangling tidies a compiled name, but cannot say which lambda `λ₃` was. A resolver finds the
definition behind each frame from the positions the compiler recorded, and costs a file read
for each class, so it is chosen by an import and is not automatic.

```scala fuel
compile
frame
  label Frames as the JVM names them, demangled
  highlight displayMethod  the compiled name, tidied
  code
      val trace: StackTrace = Exception("Double, double toil and trouble").stackTrace

      val method: Optional[Text] = trace.frames.head.let(_.displayMethod)
frame
  label Frames as the source names them
  highlight tastyStackResolution  every trace captured in this scope is resolved
  highlight displayMethod  the name in the source
  code
      import stackResolutions.tastyStackResolution
      import classloaders.threadContextClassloader
      import workingDirectories.javaBaseWorkingDirectory

      val trace: StackTrace = Exception("Double, double toil and trouble").stackTrace

      val method: Optional[Text] = trace.frames.head.let(_.displayMethod)
##
```

The example is compiled, and is not run, since the classes of a fragment of documentation are
not on a classpath which a resolver can read.
[Resolving frames to their source](../../soundness/modules/stack-traces.md#resolving-frames-to-their-source)
describes what a resolved frame holds, and
[declarative context](../../philosophy/declarative-context.md) why the choice is an import.
