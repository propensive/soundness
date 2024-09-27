package nomenclature

export Nomenclature.Name

extension (inline context: StringContext)
  transparent inline def n: Any = ${Nomenclature2.extractor('context)}
