# A cycle, or none

A `Digraph` may hold a cycle or may not, and `cycle` answers with an `Optional`: the nodes of
a cycle, or `Unset`. Neither answer is an error, and the absent one cannot be mistaken for a
list.

```scala fuel
compile
frame
  label A feud which goes round
  code
      val feud = Digraph(t"Tybalt" -> t"Mercutio", t"Romeo" -> t"Tybalt", t"Mercutio" -> t"Romeo")
      val circle: Optional[List[Text]] = feud.cycle
frame
  label What each answer means
  highlight let  the cycle, joined into a line
  highlight or(t"no..cycle")  and the absence of one
  code
      val feud = Digraph(t"Tybalt" -> t"Mercutio", t"Romeo" -> t"Tybalt", t"Mercutio" -> t"Romeo")
      val circle: Optional[List[Text]] = feud.cycle
      val verdict: Text = circle.let(_.join(t" → ")).or(t"no cycle")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(verdict)
Out.println(Digraph(t"Romeo" -> t"Juliet").cycle.inspect)
```

[Building a graph](../../soundness/modules/graphs.md#building-a-graph) introduces `cycle`, and
[optionality](../../philosophy/optionality.md) explains `let` and `or`.
