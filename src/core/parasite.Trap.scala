package parasite

import rudiments.*

class Trap(lambda: Throwable ~> Transgression, monitor: Monitor):
  def within[ResultType](block: Monitor ?=> ResultType): ResultType =
    block(using monitor.intercept(lambda))
