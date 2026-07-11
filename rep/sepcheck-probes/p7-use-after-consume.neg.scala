// P7 negative: using a stream after a consume factory adopted it must be rejected —
// "bypass the cursor and refill its stream" becomes a compile error.
//EXPECT: error
import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.Mutable

class Feed extends Mutable:
  private var position: Int = 0
  update def refill(): Int = { position += 1; position }

class Cursor(consume stream: Feed^) extends Mutable:
  update def advance(): Int = stream.refill()

def bad(): Int =
  val stream = Feed()
  val cursor = Cursor(stream)
  stream.refill()
  cursor.advance()
