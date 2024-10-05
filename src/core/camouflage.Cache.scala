package camouflage

import parasite.*
import rudiments.*
import contingency.*
import vacuous.*

class Cache[ValueType]():
  private val value: Promise[ValueType] = Promise()

  def establish(block: => ValueType): ValueType raises ConcurrencyError = value.synchronized:
    if value.ready then value().vouch(using Unsafe) else block.tap(value.fulfill(_))
