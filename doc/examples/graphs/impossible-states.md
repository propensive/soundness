# A graph which cannot hold a cycle

A `Dag` is acyclic by its type, and an operation which could close a cycle does not give one.
`+` adds any edge at all, so its result is a `Digraph`, and the way back to a `Dag` is the one
conversion which checks.

```scala fuel
compile
frame
  label An edge added, and a `Dag` expected
  errors show
  code
      import strategies.throwUnsafely

      val plot = Dag(t"revenge" -> t"ghost", t"ghost" -> t"murder")
      val sequel: Dag[Text] = plot + (t"murder" -> t"ambition")
frame
  label The type it really has
  highlight Digraph[Text]  a cycle is possible
  code
      import strategies.throwUnsafely

      val plot = Dag(t"revenge" -> t"ghost", t"ghost" -> t"murder")
      val sequel: Digraph[Text] = plot + (t"murder" -> t"ambition")
frame
  label Checked, and a `Dag` again
  highlight acyclic  the checked conversion
  code
      import strategies.throwUnsafely

      val plot = Dag(t"revenge" -> t"ghost", t"ghost" -> t"murder")
      val sequel: Dag[Text] = (plot + (t"murder" -> t"ambition")).acyclic
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(sequel.linearized.inspect)
```

[Editing](../../soundness/modules/graphs.md#editing) has the table of which edit gives which
type, and [impossible states](../../philosophy/impossible-states.md) the principle.
