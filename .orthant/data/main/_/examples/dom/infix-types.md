# Where a foreign value comes from

The type of a value from the page says which interface it has and which ecosystem defines that
interface, as a phrase. Each step of a navigation has such a type, given by the declaration of
the member that was selected.

```scala fuel
compile
frame
  label The page's document
  highlight Foreign..Browser  an interface, and the ecosystem it belongs to
  code
      val page: Foreign of "Document" from Browser = document
frame
  label An element found in it
  highlight "HTMLElement"  what `getElementById` is declared to return
  code
      val page: Foreign of "Document" from Browser = document
      val title: Foreign of "HTMLElement" from Browser = page.getElementById(t"title")
##
```

[The page's globals](../../soundness/modules/dom.md#the-pages-globals) introduces `document` and
`window`.
