# The type of a piece of markup

The type of an HTML value can say which tag it is, what it may contain and which specification
governs it, as a phrase: flow content, or an image element of the WHATWG standard.

```scala fuel
compile
frame
  label Any flow content
  highlight Html..Flow  a tag from the specification's flow content
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      val body: List[Html of Flow] = List(Hr, P("Enter Puck"), Br)
frame
  label One tag, under one specification
  highlight Html.Element..Whatwg  a tag, and the specification which defines it
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      val body: List[Html of Flow] = List(Hr, P("Enter Puck"), Br)
      val image: Html.Element of "img" in Whatwg = Img(alt = t"Puck")
##
```

[Precise types](../../soundness/modules/html.md#precise-types) has the three refinements, `of`,
`over` and `in`, and the content models.
