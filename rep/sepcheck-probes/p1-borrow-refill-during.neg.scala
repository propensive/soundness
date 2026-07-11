// P1 borrow negative: a lambda that captures the exclusive stream (and could therefore
// refill/compact the very storage it was lent) must be rejected at the application —
// the closure's hidden exclusive capability interferes with the borrow's read-only prefix.
// This is the encoding of "no refill while the window is borrowed".
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

def bad(stream: Stream^): Int =
  stream.reading { (storage, start, limit) =>
    stream.refill()
    storage(start) & 0xff
  }
