# One stylesheet, written two ways

How a stylesheet is written out is not an argument to `show`. It is a choice in scope: indented
for a reader, or compact for a browser.

```scala fuel
compile
alternative Indented
  highlight indentedCssFormatting  for reading
  code
      import formatting.indentedCssFormatting

      val sheet = css"a { color: red; width: 4px }".show
alternative Compact
  highlight compactCssFormatting  for serving
  code
      import formatting.compactCssFormatting

      val sheet = css"a { color: red; width: 4px }".show
##
```

[Rendering](../../soundness/modules/css.md#rendering) shows the output of each.
