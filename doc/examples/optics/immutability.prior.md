# An update computed from the old value

An assignment in a `lens` replaces a value, and within it `prior` is the value being replaced.
A change to every element of a list is therefore one expression, with no loop and nothing
mutated.

```scala fuel
compile
frame
  label Every scene, renumbered to the same value
  code
      case class Scene(place: Text, number: Int)
      case class Act(name: Text, scenes: List[Scene])

      def renumber(act: Act): Act = act.lens(_.scenes(Each).number = 10)
frame
  label Every scene, renumbered from its own value
  highlight prior  the number this scene had
  code
      case class Scene(place: Text, number: Int)
      case class Act(name: Text, scenes: List[Scene])

      def renumber(act: Act): Act = act.lens(_.scenes(Each).number = prior + 10)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

val act = Act(t"Act I", List(Scene(t"A heath", 1), Scene(t"Forres", 2)))

Out.println(renumber(act).scenes.inspect)
Out.println(act.scenes.inspect)
```

The second line is the original, which is unchanged.
[Updating from the old value](../../soundness/modules/optics.md#updating-from-the-old-value)
has more, and [immutability](../../philosophy/immutability.md) the reasons.
