package acyclicity

extension (inline stringContext: StringContext)
  transparent inline def ref(inline parts: Any*): Dot.Ref =
    ${NodeParser.expand('NodeParser, 'stringContext, 'parts)}

extension (stringContext: StringContext)
  def id(): Dot.Id = Dot.Id(stringContext.parts.head)