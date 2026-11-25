# A panel as wide as a quantity

A feature's dimensions are hooks which the page overrides. The width of a side panel is a
length with its unit, as [quantities](../../soundness/modules/quantities.md) are everywhere
else, and not a string of CSS.

```scala fuel
compile
frame
  label A side panel
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      class Playbill extends Archetype, VersoPanel:
        def content: Html of (? <: Flow) = P("Tonight: Twelfth Night")
        override def verso: Html of (? <: Flow) = P("Viola, Orsino, Olivia")
frame
  label Its width, in root ems
  highlight 20.0*Rem  a length, checked as one
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      class Playbill extends Archetype, VersoPanel:
        def content: Html of (? <: Flow) = P("Tonight: Twelfth Night")
        override def verso: Html of (? <: Flow) = P("Viola, Orsino, Olivia")
        override def versoWidth = 20.0*Rem
frame
  label A duration is not a width
  errors show
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      class Playbill extends Archetype, VersoPanel:
        def content: Html of (? <: Flow) = P("Tonight: Twelfth Night")
        override def verso: Html of (? <: Flow) = P("Viola, Orsino, Olivia")
        override def versoWidth = 20.0*Second
##
```

[Framing order](../../soundness/modules/pages.md#framing-order) describes the panels and their
hooks.
