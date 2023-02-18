package parasitism

import anticipation.*

case class Promise[ValueType]():
  @volatile
  private var value: Option[ValueType throws CancelError] = None

  def ready: Boolean = !value.isEmpty

  def supply(suppliedValue: => ValueType): Unit throws AlreadyCompleteError = synchronized:
    if value.isEmpty then
      value = Some(suppliedValue)
      notifyAll()
    else throw AlreadyCompleteError()

  def await(): ValueType throws CancelError = synchronized:
    while value.isEmpty do wait()
    value.get

  def cancel(): Unit = synchronized:
    value = Some(throw CancelError())
    notifyAll()

  def await
        [DurationType](duration: DurationType)(using GenericDuration[DurationType])
        : ValueType throws CancelError | TimeoutError =
    synchronized:
      if ready then value.get else
        wait(readDuration(duration))
        if !ready then throw TimeoutError() else value.get