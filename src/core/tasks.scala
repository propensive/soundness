package rudiments

import scala.util.*

case class CancelError()(using Codepoint) extends Error(err"the operation was cancelled")(pos)
case class IncompleteError()(using Codepoint) extends Error(err"the task was not completed")(pos)
case class AlreadyCompleteError()(using Codepoint) extends Error(err"the promise was already completed")(pos)

object Supervisor extends Monitor:
  @volatile
  private var interrupted: Boolean = false

  def name: Text = Text("task://main")
  def id: Text = Text("task://main")
  def continue = !interrupted
  def cancel(): Unit = interrupted = true

trait Monitor:
  def id: Text
  def name: Text
  def continue: Boolean
  def cancel(): Unit
  
  final def affirm(): Unit =
    import unsafeExceptions.canThrowAny
    if !continue then throw CancelError()

  def child(id: Text, check: => Boolean, abort: => Unit): Monitor =
    TaskMonitor(id, () => check, () => abort, this)

case class TaskMonitor(id: Text, interrupted: () => Boolean, stop: () => Unit, parent: Monitor) extends Monitor:
  def name: Text = Text(parent.name.s+"/"+id)
  def continue: Boolean = !interrupted() && parent.continue
  def cancel(): Unit = stop()

object Task:
  def apply[T](id: Text)(fn: Monitor ?=> T)(using monitor: Monitor): Task[T] =
    new Task(id, mon => fn(using mon))(using monitor)

case class Promise[T]():
  private var value: Option[T throws CancelError] = None

  def ready: Boolean = synchronized(!value.isEmpty)
  
  def supply(v: T): Unit throws AlreadyCompleteError = synchronized:
    if value.isEmpty then
      value = Some(v)
      notifyAll()
    else throw AlreadyCompleteError()

  def await(): T throws CancelError = synchronized:
    while value.isEmpty do wait()
    value.get

  def cancel(): Unit = synchronized:
    value = Some(throw CancelError())

  def get: T throws IncompleteError | CancelError = value.getOrElse(throw IncompleteError())

  override def toString: String =
    try value.fold("[incomplete]")(_.toString) catch case err: CancelError => "[canceled]"

extension [T](xs: Iterable[Task[T]])
  transparent inline def sequence(using mon: Monitor): Task[Iterable[T]] =
    Task(Text("sequence"))(unsafely(xs.map(_.await())))

class Task[T](id: Text, calc: Monitor => T)(using mon: Monitor):
  def name = Text(mon.name.s+"/"+id)
  def active: Boolean = !result.ready
  def await(): T throws CancelError = synchronized(apply().await().tap(thread.join().waive))
  
  def apply(): Promise[T] = synchronized:
    if !thread.isAlive then thread.start()
    result

  def cancel(): Unit =
    ctx.cancel()
    result.cancel()

  def map[S](fn: T => S)(using mon: Monitor): Task[S] = new Task(id, ctx => fn(calc(ctx)))
  
  def flatMap[S](fn: T => Task[S])(using mon: Monitor): Task[S] =
    new Task(id, ctx => unsafely(fn(calc(ctx)).await()))
  
  private val result: Promise[T] = Promise()
  private lazy val thread: Thread = Thread(runnable, ctx.name.s)
  private lazy val ctx = mon.child(id, thread.isInterrupted, thread.interrupt())
  private def runnable: Runnable = () => safely(result.supply(calc(ctx)))
  override def toString: String = s"Task(${result.toString})"

def affirm()(using mon: Monitor): Unit =
  import unsafeExceptions.canThrowAny
  mon.affirm()