package austronesian

import anticipation.*
import prepositional.*

export Austronesian.Java

extension [ValueType: Encodable in Java](value: ValueType) def java = ValueType.encoded(value)
