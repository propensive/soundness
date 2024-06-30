package contingency

import fulminate.*

trait Mitigator[ErrorType <: Error, ErrorType2 <: Error]:
  def mitigate(errant: ErrorType): ErrorType2
