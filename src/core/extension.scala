package litterateur

extension (inline stringContext: StringContext)
  transparent inline def md(inline parts: Any*): Markdown.Document =
    ${MdInterpolator.expand('MdInterpolator, 'stringContext, 'parts)}