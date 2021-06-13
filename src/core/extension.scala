package litterateur

extension (inline stringContext: StringContext)
  transparent inline def md(inline parts: Any*): MdNode.Document =
    ${MdInterpolator.expand('MdInterpolator, 'stringContext, 'parts)}