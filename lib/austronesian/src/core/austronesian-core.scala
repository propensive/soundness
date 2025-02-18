package austronesian

import anticipation.*
import prepositional.*

export Austronesian.Stdlib

extension [ValueType: Encodable in Stdlib](value: ValueType)
  def stdlib: Stdlib = ValueType.encoded(value)
