package prepositional

infix type by   [Type <: { type Operand }, OperandType] = Type { type Operand = OperandType }
infix type in   [Type <: { type Format },  FormatType]  = Type { type Format  = FormatType }
infix type into [Type <: { type Result },  ResultType]  = Type { type Result  = ResultType }
infix type of   [Type <: { type Value },   ValueType]   = Type { type Value   = ValueType }
infix type onto [Type <: { type Target },  TargetType]  = Type { type Target  = TargetType }
infix type over [Type <: { type Transit }, TransitType] = Type { type Transit = TransitType }

