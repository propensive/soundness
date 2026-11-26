# A list which knows where it belongs

Any type can stand where HTML is expected, once it says what kind of content it becomes. A list
of names which renders as list items may be placed in a `<ul>`, and nowhere that items may not
go.

```scala fuel
compile
frame
  label Names, rendered as list items
  highlight Renderable..li"  the content this type becomes
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      given castRenderable: (List[Text] is Renderable in "li") =
        cast => Html.Fragment(cast.map(Li(_))*)

      val fairies = Ul(List(t"Peaseblossom", t"Cobweb", t"Moth"))
frame
  label The same names, where items may not go
  errors show
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      given castRenderable: (List[Text] is Renderable in "li") =
        cast => Html.Fragment(cast.map(Li(_))*)

      val fairies = Div(List(t"Peaseblossom", t"Cobweb", t"Moth"))
##
```

[Rendering other types as HTML](../../soundness/modules/html.md#rendering-other-types-as-html)
describes the typeclass.
