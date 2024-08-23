package prepositional

infix type by   [Type <: { type Operand },  OperandType]  = Type { type Operand  = OperandType }
infix type from [Type <: { type Source },   SourceType]   = Type { type Source   = SourceType }
infix type in   [Type <: { type Format },   FormatType]   = Type { type Format   = FormatType }
infix type into [Type <: { type Result },   ResultType]   = Type { type Result   = ResultType }
infix type of   [Type <: { type Subject },  SubjectType]  = Type { type Subject  = SubjectType }
infix type on   [Type <: { type Platform }, PlatformType] = Type { type Platform = PlatformType }
infix type onto [Type <: { type Target },   TargetType]   = Type { type Target   = TargetType }
infix type over [Type <: { type Carrier },  CarrierType]  = Type { type Carrier  = CarrierType }
