# An option the compiler version does not have

A `Scalac` carries its language version as a type, and each option carries the versions which
accept it. An option passed to a compiler too new or too old for it is a type error in the
build tool, found before any compiler is started.

```scala fuel
compile
frame
  label An option which only 3.6 has
  errors show
  code
      val compiler = Scalac[3.8](List(scalacOptions.language.experimental.betterFors))
frame
  label The compiler it belongs to
  highlight 3.6  the version, as a type
  code
      val compiler = Scalac[3.6](List(scalacOptions.language.experimental.betterFors))
frame
  label With an option which every version has
  highlight scalacOptions.newSyntax
  code
      val compiler = Scalac[3.6]
        ( List(scalacOptions.language.experimental.betterFors, scalacOptions.newSyntax) )
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(compiler.commandLineArguments.inspect)
```

[Configuring a compiler](../../soundness/modules/compiler.md#configuring-a-compiler) describes
the options.
