# An edge which may close a cycle

A `Dag` never holds a cycle, so adding a dependency to one can fail: the new edge may lead back
to where it began. `add` declares a `Dag.Error`, and a method which calls it must handle the
error or declare it too.

```scala fuel
compile
frame
  label A dependency, added on trust
  errors show
  code
      def follow(plot: Dag[Text], effect: Text, cause: Text): Dag[Text] =
        plot.add(effect, cause)
frame
  label The failure, in the return type
  highlight raises..Dag.Error  the caller must answer for it
  code
      def follow(plot: Dag[Text], effect: Text, cause: Text): Dag[Text] raises Dag.Error =
        plot.add(effect, cause)
##
```

[Editing](../../soundness/modules/graphs.md#editing) lists the edits which can fail and those
which cannot, and [error handling](../../philosophy/error-handling.md) explains `raises`.
