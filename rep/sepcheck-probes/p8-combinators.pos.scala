// P8: the safe mutable front-end shape — consume-typed pipeline combinators chaining on
// an exclusive stream (each stage adopts its upstream and returns a fresh stage), with a
// borrow-based terminal fold. This is what would replace the LazyList views.
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

extension (consume source: Source^)
  def mapped(transform: Int -> Int): Source^ = Mapped(source, transform)
  def fold(initial: Int)(combine: (Int, Int) -> Int): Int =
    var total = initial
    var value = source.next()
    while value != 0 do
      total = combine(total, value)
      value = source.next()
    total

def pipeline(): Int =
  Counter(10).mapped(_ * 2).fold(0)(_ + _)
