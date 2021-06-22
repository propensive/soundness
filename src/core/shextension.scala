package guillotine

extension (inline stringContext: StringContext)
  transparent inline def sh(inline parts: Any*): Command = ${Sh.expand('Sh, 'stringContext, 'parts)}
