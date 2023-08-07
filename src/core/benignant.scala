package perforate

import rudiments.*
import digression.*
import fulminate.*

import language.experimental.captureChecking

@capability
trait ErrorHandler[ErrorType <: Error](val flow: ControlFlow):
  def record(error: ErrorType): Unit
  def apply(): Unit

@capability
class Aggregator[ErrorType <: Error](flow: ControlFlow)(using CanThrow[AggregateError[ErrorType]])
extends ErrorHandler[ErrorType](flow):
  private var collected: List[ErrorType] = Nil
  def record(error: ErrorType): Unit = collected ::= error
  def apply(): Unit = if !collected.isEmpty then throw AggregateError[ErrorType](collected.reverse)

@capability
class Thrower[ErrorType <: Error: CanThrow](flow: ControlFlow) extends ErrorHandler[ErrorType](flow):
  def record(error: ErrorType): Unit = throw error
  def apply(): Unit = ()

trait Recovery[-ErrorType <: Error, +ResultType]:
  def recover(error: ErrorType): ResultType

object Benign:
  opaque type ControlFlow = boundary.Label[?]

  object ControlFlow:
    def apply(label: boundary.Label[?]): ControlFlow = label
  
  extension (flow: ControlFlow) def label: boundary.Label[?] = flow

export Benign.ControlFlow

def raise
    [ResultType, ErrorType <: Error]
    (using handler: ErrorHandler[ErrorType])
    (error: ErrorType)
    (using recovery: Recovery[error.type, ResultType])
    : ResultType =
  handler.record(error)
  recovery.recover(error)

def validate
    [ErrorType <: Error]
    (using aggregate: CanThrow[AggregateError[ErrorType]])
    [ResultType]
    (fn: ErrorHandler[ErrorType] ?=> ResultType)
    : ResultType =
  
  boundary: label ?=>
    val aggregator = Aggregator[ErrorType](ControlFlow(label))
    fn(using aggregator).tap(aggregator().waive)

def attempt
    [ErrorType <: Error]
    (using error: CanThrow[ErrorType])
    [ResultType]
    (fn: ErrorHandler[ErrorType] ?=> ResultType)
    : ResultType =
  
  boundary: label ?=>
    val thrower = Thrower[ErrorType](ControlFlow(label))
    fn(using thrower).tap(thrower().waive)

inline def controlFlow(using inline flow: ControlFlow): ControlFlow = flow
