package rudiments

import language.experimental.captureChecking

object Unset:
  override def toString(): String = "[unset]"

type Maybe[ValueType] = Unset.type | ValueType

case class UnsetValueError() extends Error(ErrorMessage(List(Text("the value was not set")), Nil))

extension [ValueType](opt: Maybe[ValueType])
  def unset: Boolean = opt == Unset
  def cast(using Unsafe.type): ValueType = opt.asInstanceOf[ValueType]
  def or(value: => ValueType): ValueType^{value} = if unset then value else cast(using Unsafe)
  def presume(using default: => Default[ValueType]): ValueType^{default} = or(default())
  
  def assume(using unsetValue: CanThrow[UnsetValueError]): ValueType^{unsetValue} =
    or(throw UnsetValueError())
  
  def option: Option[ValueType] = if unset then None else Some(cast(using Unsafe))

  def fm
      [ValueType2](default: => ValueType2)(fn: ValueType => ValueType2)
      : ValueType2^{default, fn} =
    if unset then default else fn(cast(using Unsafe))

  def mm[ValueType2](fn: ValueType => ValueType2): Maybe[ValueType2]^{fn} =
    if unset then Unset else fn(cast(using Unsafe))

extension [ValueType](opt: Option[ValueType])
  def maybe: Unset.type | ValueType = opt.getOrElse(Unset)
  def presume(using default: Default[ValueType]) = opt.getOrElse(default())