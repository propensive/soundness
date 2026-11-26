# A page assembled from features

Each feature of a page is a trait: a masthead, a footer, a headline. Mixing one in adds its
markup, its styles and its metadata together, and the page fills the slot it introduces.

```scala fuel
compile
frame
  label Content alone
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      class Playbill extends Archetype:
        def content: Html of (? <: Flow) = P("Tonight: Twelfth Night")
frame
  label A headline, which is also the document's title
  highlight Headline(t"Illyria")  one trait, an `<h1>` and a `<title>`
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      class Playbill extends Archetype, Headline(t"Illyria"):
        def content: Html of (? <: Flow) = P("Tonight: Twelfth Night")
frame
  label A masthead, and the slot it introduces
  highlight masthead  the slot, filled by overriding it
    line 5
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      class Playbill extends Archetype, Headline(t"Illyria"), Masthead:
        def content: Html of (? <: Flow) = P("Tonight: Twelfth Night")
        override def masthead: Html of (? <: Flow) = Strong("The Globe")
##
```

[Framing order](../../soundness/modules/pages.md#framing-order) explains how the order of the
traits decides what wraps what.
