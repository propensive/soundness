// P8 negative: pulling from a stream that has already been piped into a downstream stage
// must be rejected — one-shot-ness becomes typed (what LazyList only hid via memoization).
//EXPECT: error
import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.Mutable

trait Source extends Mutable:
  update def next(): Int

class Counter(size: Int) extends Source:
  private var index: Int = 0
  update def next(): Int = if index < size then { index += 1; index } else 0

class Mapped(consume upstream: Source^, transform: Int -> Int) extends Source:
  update def next(): Int = transform(upstream.next())

def bad(): Int =
  val range = Counter(10)
  val mapped = Mapped(range, _ * 2)
  range.next()
  mapped.next()
