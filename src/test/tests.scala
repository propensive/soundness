package hyperbole

import gossamer.*
import rudiments.*

object Tests:
  val three = 3
  Macros.inspect:
    three match { case x: (Int | String) => x + 1 }
