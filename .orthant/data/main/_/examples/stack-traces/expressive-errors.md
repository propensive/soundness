# Compiled names, read as they were written

A JVM stack trace names what the compiler produced: `$plus$plus` for an operator, `$anonfun$3`
for a lambda, `$extension` for an extension method. `StackTrace.rewrite` turns each back toward
the Scala which was written, and every frame of a `StackTrace` has been through it.

```scala fuel
compile
frame
  label An operator
  highlight "$plus$plus"  the JVM's spelling of ++
  code
      val operator: Text = StackTrace.rewrite("$plus$plus")
frame
  label A lambda
  highlight "$anonfun$3"  the third lambda of its method
  code
      val operator: Text = StackTrace.rewrite("$plus$plus")
      val lambda: Text = StackTrace.rewrite("$anonfun$3", method = true)
frame
  label An extension method
  highlight "soliloquy$extension"  an extension method, as a class file names it
  code
      val operator: Text = StackTrace.rewrite("$plus$plus")
      val lambda: Text = StackTrace.rewrite("$anonfun$3", method = true)
      val extension: Text = StackTrace.rewrite("soliloquy$extension", method = true)
frame
  label An object
  highlight "elsinore.Hamlet$"  the class behind an object
  code
      val operator: Text = StackTrace.rewrite("$plus$plus")
      val lambda: Text = StackTrace.rewrite("$anonfun$3", method = true)
      val extension: Text = StackTrace.rewrite("soliloquy$extension", method = true)
      val module: Text = StackTrace.rewrite("elsinore.Hamlet$")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(operator)
Out.println(lambda)
Out.println(extension)
Out.println(module)
```

[Demangling](../../soundness/modules/stack-traces.md#demangling) lists every glyph, and
[expressive errors](../../philosophy/expressive-errors.md) says why a diagnostic should use the
programmer's words.
