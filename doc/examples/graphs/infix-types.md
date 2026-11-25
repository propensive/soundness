# A method for anything which is a graph

A method need not choose between a `Dag`, a `Digraph` and a `Map`. It asks for a type which
`is Nodal by Text`, a graph whose nodes are text, and the vocabulary of graphs is then
available on a value of that type, whatever it is.

```scala fuel
compile
frame
  label For a `Dag` only
  code
      def endings(plot: Dag[Text]): Set[Text] = plot.sinks
frame
  label For any graph of text
  highlight graph..Text  the type, described in a phrase
    line 1
    occurrence 2
  code
      def endings[graph](plot: graph)(using graph is Nodal by Text): Set[Text] = plot.sinks
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(endings(Digraph(t"Romeo" -> t"Juliet", t"Paris" -> t"Juliet")).inspect)
Out.println(endings(Map(t"Viola" -> Set(t"Orsino"), t"Orsino" -> Set())).inspect)
```

[Graphs](../../soundness/modules/graphs.md#about) introduces `Nodal`, and
[infix types](../../philosophy/infix-types.md) explains how such a type is read.
