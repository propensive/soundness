package rudiments

import language.experimental.captureChecking

class Recurrence[ValueType](fn: -> ValueType -> ValueType):
  inline def apply(value: ValueType): ValueType = fn(value)

def fix[ValueType](fn: Recurrence[ValueType] ?-> (ValueType -> ValueType)): (ValueType -> ValueType) =
  fn(using Recurrence(fix(fn)))

def recur[ValueType](value: ValueType)(using recurrence: Recurrence[ValueType]): ValueType = recurrence(value)
