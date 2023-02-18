package parasitism

import anticipation.*
import rudiments.*

@capability
sealed trait Monitor:
  def id: Text
  def virtualThreads: Boolean
  def daemon: Boolean

  def fork(runnable: Runnable, id: Text): Thread =
    if virtualThreads then Thread.ofVirtual.nn.name(id.s).nn.start(runnable).nn
    else Thread(runnable, id.s).nn.tap(_.setDaemon(daemon)).tap(_.start())
  
  def child(childId: Text): TaskMonitor = TaskMonitor(Text(id.s+"/"+childId.s), this)

@capability
case class TaskMonitor(id: Text, parent: Monitor) extends Monitor:
  def virtualThreads: Boolean = parent.virtualThreads
  def daemon: Boolean = parent.daemon

@capability
case class Supervisor(virtualThreads: Boolean, daemon: Boolean) extends Monitor:
  def id: Text = Text("")

object Task:
  def apply[ResultType](name: Text)(eval: Monitor => ResultType)(using monitor: Monitor): Task[ResultType] =
    new Task[ResultType](name, eval, monitor.child(name))

class Task
        [+ResultType]
        (val id: Text, val evaluate: Monitor => ResultType, monitor: Monitor):
  private final val promise: Promise[ResultType] = Promise()

  private val runnable: Runnable = new Runnable():
    def run(): Unit =
      try promise.supply(evaluate(monitor))
      catch
        case err: AlreadyCompleteError => ()
        case err: CancelError          => ()

  val thread: Thread = Thread(runnable, id.s).nn.tap(_.start())
  
  def attend()(using cancel: CanThrow[CancelError]): Unit = thread.join()
  
  def await()(using cancel: CanThrow[CancelError]): ResultType =
    promise.await().tap(thread.join().waive)
  
  def cancel(): Unit = thread.interrupt().tap:
    (try promise.cancel() catch case err: CancelError => ()).waive

def supervise[ResultType]
      (virtualThreads: Boolean = false, daemon: Boolean = false)(fn: Supervisor ?=> ResultType)
      : ResultType =
  val supervisor = Supervisor(virtualThreads, daemon)
  fn(using supervisor)