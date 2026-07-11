// P1 borrow: the scoped CPS window-borrow shape proposed for `Stream.reading` /
// `Intake.writing` — a well-behaved lambda that only reads the storage during the borrow.
import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.Mutable

class Stream extends Mutable:
  private var storage: Array[Byte] = new Array[Byte](16)
  private var start: Int = 0
  private var limit: Int = 16

  update def refill(): Int = { start = 0; limit }
  update def skip(count: Int): Unit = start += count

  def reading[result](lambda: (Array[Byte], Int, Int) ->{caps.any, this.rd} result): result =
    lambda(storage, start, limit)

def checksum(stream: Stream^): Int =
  var total = 0
  var count = stream.refill()
  while count > 0 do
    total += stream.reading { (storage, start, limit) =>
      var sum = 0
      var index = start
      while index < limit do { sum += storage(index) & 0xff; index += 1 }
      sum
    }
    stream.skip(count)
    count = 0
  total
