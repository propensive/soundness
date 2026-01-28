package gnossienne

import adversaria.*
import prepositional.*

extension [entity](entity: entity)(using resolvable: entity is Resolvable)
  transparent inline def ref(using dereferenceable: entity is Dereferenceable to resolvable.Operand)
  : Reference to entity =
      Reference(dereferenceable.select(entity, resolvable.field))
