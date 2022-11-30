package cellulose

extension (inline ctx: StringContext)
  transparent inline def codl(inline parts: Any*): Doc = ${Codl.Prefix.expand('{Codl.Prefix}, 'ctx, 'parts)}

extension [T](value: T)(using codec: Codec[T])
  def codl: Doc = Doc(IArray.from(codec.serialize(value).flatten), codec.schema, 0)