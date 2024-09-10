package parasite

import anticipation.*
import contingency.*
import denominative.*
import vacuous.*

trait Tenacity:
  def delay(attempt: Ordinal): Optional[Long]

  def limit(n: Int): Tenacity raises RetryError = attempt =>
    if attempt.n0 > n then abort(RetryError(attempt.n0)) else delay(attempt)

object Tenacity:
  def exponential[DurationType: GenericDuration](initial: DurationType, base: Double): Tenacity =
    attempt => (initial.milliseconds*math.pow(base, attempt.n0)).toLong

  def fixed[DurationType: GenericDuration](duration: DurationType): Tenacity =
    attempt => duration.milliseconds
