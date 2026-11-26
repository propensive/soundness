# A lens, as a value

Beneath the path syntax a lens is an ordinary value, and its type says what it is: the field
`"name"` is a lens from an `Act`. It is summoned by that type, and a field which the type does
not have is not found.

```scala fuel
compile
frame
  label A lens onto a field
  highlight "name"..Act  the type of the lens
  code
      case class Act(name: Text, scenes: Int)

      val lens = summon["name" is Lens from Act]
      val loud: Act = lens.modify(Act(t"Act One", 7))(_.upper)
frame
  label A field which an act does not have
  errors show
  code
      case class Act(name: Text, scenes: Int)

      val lens = summon["title" is Lens from Act]
##
```

[Lenses as values](../../soundness/modules/optics.md#lenses-as-values) shows how two lenses
compose, and [infix types](../../philosophy/infix-types.md) explains `is` and `from`.
