package parasite

object Parasite:
  opaque type Stale[ValueType] = ValueType

  extension [ValueType](stale: Stale[ValueType]) def apply(): ValueType = stale

  object Stale:
    def apply[ValueType](value: ValueType): Stale[ValueType] = value
