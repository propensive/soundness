package rudiments

import language.experimental.captureChecking

object Unset:
  override def toString(): String = "——"

type Maybe[T] = Unset.type | T

case class UnsetValueError()
extends Error(ErrorMessage[EmptyTuple](List(Text("the value was not set")), EmptyTuple))

extension [T](opt: Maybe[T])
  def unset: Boolean = opt == Unset
  
  def or(value: {*}-> T): {value} T = opt match
    case Unset               => value
    case other: T @unchecked => other

  def presume(using default: Default[T]): T = or(default())
  def assume(using th: CanThrow[UnsetValueError]): {th} T = or(throw UnsetValueError())

  def fm[S](default: -> S)(fn: T -> S) = opt match
    case Unset               => default
    case value: T @unchecked => fn(value)

  def mm[S](fn: T -> S): Maybe[S] = opt match
    case Unset               => Unset
    case value: T @unchecked => fn(value)

  def option: Option[T] = opt match
    case Unset               => None
    case other: T @unchecked => Some(other)

extension [T](opt: Option[T])
  def maybe: Unset.type | T = opt.getOrElse(Unset)
  def presume(using default: Default[T]) = opt.getOrElse(default())
