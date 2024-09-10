package parasite

import anticipation.*
import contingency.*
import denominative.*
import vacuous.*

trait Tenacity:
  private inline def tenacity: this.type = this
  def delay(attempt: Ordinal): Optional[Long] raises RetryError

  def limit(n: Int): Tenacity = new:
    def delay(attempt: Ordinal): Optional[Long] raises RetryError =
      if attempt.n1 > n then abort(RetryError(attempt.n1 - 1)) else tenacity.delay(attempt)

object Tenacity:
  def exponential[DurationType: GenericDuration](initial: DurationType, base: Double): Tenacity =
    new:
      def delay(attempt: Ordinal): Optional[Long] raises RetryError =
        if attempt == Prim then 0L else (initial.milliseconds*math.pow(base, attempt.n1)).toLong

  def fixed[DurationType: GenericDuration](duration: DurationType): Tenacity = new:
    def delay(attempt: Ordinal): Optional[Long] raises RetryError =
      if attempt == Prim then 0L else duration.milliseconds
