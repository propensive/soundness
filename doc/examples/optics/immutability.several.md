# Several updates, one new value

A `lens` takes any number of assignments and applies them together. Updates which share the
start of a path rebuild the structure they share once, and the original is still as it was.

```scala fuel
compile
frame
  label One assignment
  code
      case class Act(name: Text, scenes: Int)
      case class Play(opening: Act, title: Text)

      def revise(play: Play): Play = play.lens(_.opening.name = t"Act One")
frame
  label Three, two of them inside the same act
  highlight _.opening.scenes  shares a prefix with the first
  code
      case class Act(name: Text, scenes: Int)
      case class Play(opening: Act, title: Text)

      def revise(play: Play): Play = play.lens
        ( _.opening.name = t"Act One",
          _.opening.scenes = 7,
          _.title = t"The Tragedy of Macbeth" )
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

val play = Play(Act(t"Act I", 5), t"Macbeth")

Out.println(revise(play).inspect)
Out.println(play.inspect)
```

[Updating a field](../../soundness/modules/optics.md#updating-a-field) has the details, and
[immutability](../../philosophy/immutability.md) explains why the second line is unchanged.
