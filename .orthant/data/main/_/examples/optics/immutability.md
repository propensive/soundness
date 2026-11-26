# An update deep inside a value

A case class never changes, so an update is a new value. `lens` writes one as an assignment to
a path, and the compiler checks the path against the fields.

```scala fuel
compile
frame
  label Copying by hand
  code
      case class Scene(name: Text, lines: Int)
      case class Play(title: Text, opening: Scene)

      def retitle(play: Play): Play =
        play.copy(opening = play.opening.copy(name = t"A desert place"))
frame
  label The same update, as a path
  highlight _.opening.name..place"  a path, and its new value
  code
      case class Scene(name: Text, lines: Int)
      case class Play(title: Text, opening: Scene)

      def retitle(play: Play): Play =
        play.lens(_.opening.name = t"A desert place")
frame
  label A field which does not exist
  errors show
  code
      case class Scene(name: Text, lines: Int)
      case class Play(title: Text, opening: Scene)

      def retitle(play: Play): Play =
        play.lens(_.opening.setting = t"A desert place")
##
```

[Optics](../../soundness/modules/optics.md) reaches into collections and optional values in
the same way.
