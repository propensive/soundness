# A slot takes what belongs in it

The slots of a page have the types which govern all HTML. A masthead holds flow content, so a
list item, which belongs only in a list, cannot be put there.

```scala fuel
compile
frame
  label A list item in the masthead
  errors show
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      class Playbill extends Archetype, Masthead:
        def content: Html of (? <: Flow) = P("Tonight: Twelfth Night")
        override def masthead: Html of (? <: Flow) = Li("The Globe")
frame
  label Flow content in the masthead
  highlight Strong("The..Globe")  phrasing content, which is flow content
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      class Playbill extends Archetype, Masthead:
        def content: Html of (? <: Flow) = P("Tonight: Twelfth Night")
        override def masthead: Html of (? <: Flow) = Strong("The Globe")
##
```

[Defining a page](../../soundness/modules/pages.md#defining-a-page) names each slot and its
feature.
