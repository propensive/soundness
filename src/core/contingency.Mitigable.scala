package contingency

import fulminate.*

trait Mitigable:
  type Self <: Error
  type Result <: Error
  def mitigate(errant: Self): Result
