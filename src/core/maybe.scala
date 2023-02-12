package rudiments

import language.experimental.captureChecking

case class UnsetValueError()
extends Error(ErrorMessage[EmptyTuple](List(Text("the value was not set")), EmptyTuple))

extension [ValueType](opt: Maybe[ValueType])
  def unset: Boolean = opt == Unset
  
  def or(value: {*}-> ValueType): {value} ValueType = opt match
    case Unset                       => value
    case other: ValueType @unchecked => other

  def presume(using default: Default[ValueType]): ValueType = or(default())
  def assume(using unsetValue: CanThrow[UnsetValueError]): {unsetValue} ValueType = or(throw UnsetValueError())

  def fm[ValueType2](default: -> ValueType2)(fn: ValueType -> ValueType2) = opt match
    case Unset                       => default
    case value: ValueType @unchecked => fn(value)

  def mm[ValueType2](fn: ValueType -> ValueType2): Maybe[ValueType2] = opt match
    case Unset                       => Unset
    case value: ValueType @unchecked => fn(value)

  def option: Option[ValueType] = opt match
    case Unset                       => None
    case other: ValueType @unchecked => Some(other)

extension [ValueType](opt: Option[ValueType])
  def maybe: Unset.type | ValueType = opt.getOrElse(Unset)
  def presume(using default: Default[ValueType]) = opt.getOrElse(default())
