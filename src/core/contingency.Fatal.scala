package contingency

import rudiments.*

trait Fatal:
  type Self
  def status(error: Self): ExitStatus
