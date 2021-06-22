package litterateur

extension (inline stringContext: StringContext)
  transparent inline def md(inline parts: Any*): Markdown.Root[Markdown] =
    ${MdInterpolator.expand('MdInterpolator, 'stringContext, 'parts)}