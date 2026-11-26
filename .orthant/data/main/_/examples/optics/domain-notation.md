# A path through a collection

A path to a field is written as the field would be selected, and a step in parentheses goes
into a collection: one element, every element, or those which satisfy a predicate. The update
is still an assignment.

```scala fuel
compile
frame
  label The first scene
  highlight (Prim)  one element
  code
      case class Scene(place: Text, number: Int)
      case class Act(name: Text, scenes: List[Scene])

      def revise(act: Act): Act =
        act.lens(_.scenes(Prim).place = t"A desert place")
frame
  label Every scene
  highlight (Each)  every element
  code
      case class Scene(place: Text, number: Int)
      case class Act(name: Text, scenes: List[Scene])

      def revise(act: Act): Act =
        act.lens(_.scenes(Each).place = t"A desert place")
frame
  label The scenes after the first
  highlight (Filter[Scene](_.number..1))  the elements which match
  code
      case class Scene(place: Text, number: Int)
      case class Act(name: Text, scenes: List[Scene])

      def revise(act: Act): Act =
        act.lens(_.scenes(Filter[Scene](_.number > 1)).place = t"A desert place")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

val act = Act(t"Act I", List(Scene(t"A heath", 1), Scene(t"Forres", 2)))

Out.println(revise(act).scenes.inspect)
```

[Reaching into collections](../../soundness/modules/optics.md#reaching-into-collections) lists
the steps a path may take.
