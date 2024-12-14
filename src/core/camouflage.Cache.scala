package camouflage

import anticipation.*
import parasite.*
import rudiments.*
import vacuous.*

object Cache:
  def apply[DurationType: GenericDuration, ValueType](duration: DurationType): Cache[ValueType] =
    new Cache[ValueType](duration.milliseconds)

  def apply[ValueType](): Cache[ValueType] = new Cache(Unset)

class Cache[ValueType](lifetime: Optional[Long]):
  private var expiry: Long = Long.MaxValue
  private var value: Promise[ValueType] = Promise()

  def establish(block: => ValueType): ValueType = value.synchronized:
    if expiry < System.currentTimeMillis then value = Promise()

    if value.ready then value().vouch(using Unsafe) else block.tap: result =>
      value.offer(result)
      expiry = lifetime.lay(Long.MaxValue)(_ + System.currentTimeMillis)
