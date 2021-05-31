package acyclicity

extension (inline stringContext: StringContext)
  transparent inline def node(inline parts: Any*): NodeId =
    ${NodeParser.expand('NodeParser, 'stringContext, 'parts)}

extension (stringContext: StringContext)
  def id(): Id = Id(stringContext.parts.head)