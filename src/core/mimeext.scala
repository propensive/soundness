package gesticulate

extension (inline ctx: StringContext)
  transparent inline def mime(inline parts: String*): MimeType =
    ${Mime.Prefix.expand('{Mime.Prefix}, 'ctx, 'parts)}