// P1 borrow negative: the lent storage must not escape the borrow. `Array` is treated as
// Mutable under separation checking, so returning it out of the lambda should be rejected
// (the borrow result would hide/alias the stream's internal exclusive state).
//EXPECT: error
import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.Mutable

class Stream extends Mutable:
  private var storage: Array[Byte] = new Array[Byte](16)
  private var start: Int = 0
  private var limit: Int = 16

  update def refill(): Int = { start = 0; limit }

  def reading[result](lambda: (Array[Byte], Int, Int) ->{caps.any, this.rd} result): result =
    lambda(storage, start, limit)

def leak(stream: Stream^): Array[Byte] =
  stream.reading { (storage, start, limit) => storage }
