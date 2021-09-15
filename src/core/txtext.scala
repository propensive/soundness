package gossamer

extension (inline ctx: StringContext)
  transparent inline def txt(inline parts: Any*): Text =
    ${Txt.TxtInterpolator.expand('{Txt.TxtInterpolator}, 'ctx, 'parts)}