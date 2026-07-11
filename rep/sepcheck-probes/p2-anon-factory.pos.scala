// P2: the zephyrine factory shape — an anonymous Mutable subclass with private vars,
// returned fresh from a factory, driven through update methods in a loop and a closure.
// This is the probe expected to expose the missing SepCheck application fix (562b513a4e)
// on the 3.9 row: green on 3.10, possibly red on 3.9.
import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.Mutable

trait Source extends Mutable:
  update def refill(): Int
  update def close(): Unit

def make(chunks: Int): Source^ = new Source:
  private var remaining: Int = chunks
  private var open: Boolean = true
  update def refill(): Int =
    if remaining > 0 then { remaining -= 1; 4096 } else 0
  update def close(): Unit = open = false

def drive(): Int =
  val source = make(3)
  var total = 0
  var count = source.refill()
  while count > 0 do
    total += count
    count = source.refill()
  val finish = () => source.close()
  finish()
  total
