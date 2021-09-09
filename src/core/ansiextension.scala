package escapade

extension (inline ctx: StringContext)
  transparent inline def ansi(inline parts: Any*): AnsiString =
    ${Ansi.Interpolator.expand('{Ansi.Interpolator}, 'ctx, 'parts)}