package nomenclature

import prepositional.*

import scala.quoted.*

export Nomenclature.Name

extension (context: StringContext)
  transparent inline def n[PlatformType]()(using inline nominative: PlatformType is Nominative)
          : Name on PlatformType =
    ${Nomenclature.parse('context, 'nominative)}
