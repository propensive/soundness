package parasite

import anticipation.*
import rudiments.*
import perforate.*

import scala.annotation.*
import scala.collection.mutable as scm

import java.util.concurrent.atomic as juca

import language.experimental.captureChecking

import AsyncState.*

@capability
sealed trait Monitor(val name: List[Text], trigger: Trigger):
  private val children: scm.HashMap[Text, AnyRef] = scm.HashMap()
  def id: Text = Text(name.reverse.map(_.s).mkString(" / "))

  def cancel(): Unit =
    trigger.cancel()
    children.foreach: (id, child) =>
      child match
        case child: Monitor => child.cancel()
        case _              => ()

  def terminate(): Unit = this match
    case Supervisor                                     => Supervisor.cancel()
    case monitor@Submonitor(id, parent, state, promise) => monitor.terminate()

  def sleep(duration: Long): Unit = Thread.sleep(duration)

  def child
      [ResultType2]
      (id: Text, state: juca.AtomicReference[AsyncState[ResultType2]], trigger: Trigger)
      (using label: boundary.Label[Unit])
      : Submonitor[ResultType2] =
    
    val monitor = Submonitor[ResultType2](id, this, state, trigger)
    
    synchronized:
      children(id) = monitor
    
    monitor

case object Supervisor extends Monitor(Nil, Trigger())

def supervise
    [ResultType]
    (fn: Monitor ?=> ResultType)(using cancel: Raises[CancelError])
    : ResultType =
  fn(using Supervisor)

@capability
case class Submonitor
    [ResultType]
    (identifier: Text, parent: Monitor, stateRef: juca.AtomicReference[AsyncState[ResultType]], trigger: Trigger)
    (using label: boundary.Label[Unit])
extends Monitor(identifier :: parent.name, trigger):

  def state(): AsyncState[ResultType] = stateRef.get().nn
  
  def complete(value: ResultType): Nothing =
    stateRef.set(Completed(value))
    trigger()
    boundary.break()
  
  def acquiesce(): Unit = synchronized:
    stateRef.get().nn match
      case Active            => ()
      case Suspended(_)      => wait()
      case Completed(value)  => trigger()
                                boundary.break()
      case Cancelled         => trigger()
                                boundary.break()
      case Failed(error)     => trigger()
                                boundary.break()
