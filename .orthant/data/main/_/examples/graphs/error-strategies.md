# A cycle, and what to do about it

Building a `Dag` from edges checks them for a cycle, and raises a `Dag.Error` when it finds
one. What follows is for the caller to say, and the same construction serves each answer.

```scala fuel
compile
alternative Declared
  highlight raises..Dag.Error  the caller must answer for it
  code
      def plan(first: Text, second: Text, third: Text): Dag[Text] raises Dag.Error =
        Dag(third -> second, second -> first)
alternative Absent
  highlight safely  a cycle becomes `Unset`
  code
      def plan(first: Text, second: Text, third: Text): Optional[Dag[Text]] =
        safely(Dag(third -> second, second -> first))
alternative Replaced
  highlight recover  the same edges, as a graph which may be cyclic
  code
      def plan(first: Text, second: Text, third: Text): Digraph[Text] =
        recover:
          case Dag.Error(_) => Digraph(third -> second, second -> first)
        . protect(Dag(third -> second, second -> first).digraph)
alternative Thrown
  highlight unsafely  a cycle is thrown as an exception
  code
      def plan(first: Text, second: Text, third: Text): Dag[Text] =
        unsafely(Dag(third -> second, second -> first))
##
```

[Building a graph](../../soundness/modules/graphs.md#building-a-graph) shows the error, and
[choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) the full set of
answers.
