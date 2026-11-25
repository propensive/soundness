# A page must have content

A page is a class which mixes in the features it wants. The one thing every page owes is its
content, and a page without it is a class with an abstract member, which does not compile.

```scala fuel
compile
frame
  label A masthead, and nothing beneath it
  errors show
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      class Playbill extends Archetype, Masthead
frame
  label The content supplied
  highlight content  the one obligation of a page
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      class Playbill extends Archetype, Masthead:
        def content: Html of (? <: Flow) = P("Tonight: Twelfth Night")
##
```

[Defining a page](../../soundness/modules/pages.md#defining-a-page) lists the features and the
slots they add.
