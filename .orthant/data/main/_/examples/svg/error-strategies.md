# A drawing which may not be one

Text which is read as a drawing may not be XML, or may be XML which is not SVG. `read[Svg]`
does not decide what follows from that. The method which calls it does, and its return type
says which choice it made.

```scala fuel
compile
alternative An absent drawing
  highlight safely  any failure becomes an absent value
  code
      def drawing(source: Text): Optional[Svg] = safely(source.read[Svg])
alternative A blank drawing
  highlight or(Svg(40,..))  what absence means here
  code
      def drawing(source: Text): Svg = safely(source.read[Svg]).or(Svg(40, 40))
alternative The caller's decision
  highlight raises..Svg.Error  each way the text can be wrong
  code
      def drawing(source: Text): Svg raises Parse.Error raises Xml.Error raises Svg.Error =
        source.read[Svg]
##
```

[Documents](../../soundness/modules/svg.md#documents) describes what is read, and
[choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) the other choices.
