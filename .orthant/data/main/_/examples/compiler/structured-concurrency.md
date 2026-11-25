# A compilation which belongs to a scope

A compilation runs beside the code which started it, reporting its notices as it goes. It is
therefore a task, and a task needs a supervisor: outside `supervise` the compiler cannot be
started, and inside it the block does not end while the compiler is running.

```scala fuel
compile
frame
  label A compiler started from nowhere
  errors show
  code
      import environments.javaBaseEnvironment, probates.awaitProbate
      import strategies.throwUnsafely, systems.javaBaseSystem, threads.virtualThreads

      val compiler = Scalac[3.8](List(scalacOptions.newSyntax))

      def build(classpath: LocalClasspath, source: Text, out: Path on Local): Compiler.Result =
        compiler(classpath)(Map(t"hello.scala" -> source), out).complete()
frame
  label Supervised
  highlight supervise  the compilation cannot outlive this block
  code
      import environments.javaBaseEnvironment, probates.awaitProbate
      import strategies.throwUnsafely, systems.javaBaseSystem, threads.virtualThreads

      val compiler = Scalac[3.8](List(scalacOptions.newSyntax))

      def build(classpath: LocalClasspath, source: Text, out: Path on Local): Compiler.Result =
        supervise:
          compiler(classpath)(Map(t"hello.scala" -> source), out).complete()
##
```

The example is compiled, and is not run, since it starts a compiler.
[Compiling](../../soundness/modules/compiler.md#compiling) reads the notices of the process, and
[structured concurrency](../../philosophy/structured-concurrency.md) explains the scope.
