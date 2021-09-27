package gesticulate

extension (inline ctx: StringContext)
  transparent inline def media(inline parts: String*): MediaType =
    ${Media.Prefix.expand('{Media.Prefix}, 'ctx, 'parts)}