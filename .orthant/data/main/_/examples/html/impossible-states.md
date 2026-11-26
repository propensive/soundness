# A list which can only hold items

Each HTML element is a value whose type says what it may contain, as the specification does.
A `<ul>` takes `<li>` children and no text, so a malformed page cannot be built.

```scala fuel
compile
frame
  label Text directly inside a list
  errors show
  code
      import htmlDoms.whatwg.*

      val cast = Ul("Oberon")
frame
  label Text inside an item
  highlight Li
  code
      import htmlDoms.whatwg.*

      val cast = Ul(Li("Oberon"), Li("Titania"))
##
```

[Compiletime checking](../../soundness/modules/html.md#compiletime-checking) shows what else
the element types rule out.
