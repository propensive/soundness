package nomenclature

import scala.quoted.*

export Nomenclature.Name

extension (context: StringContext)
  transparent inline def n[PlatformType]()(using erased nominative: PlatformType is Nominative)
          : Name[nominative.Constraint] =
    ${Nomenclature.parse[PlatformType, nominative.Constraint]('context)}
