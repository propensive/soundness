// P7: consume-based linear ownership transfer — a factory adopts an exclusive stream
// (the Cursor(stream) shape) and a consume method returns this^ for fluent chaining.
import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.Mutable

class Feed extends Mutable:
  private var position: Int = 0
  update def refill(): Int = { position += 1; position }

class Cursor(consume stream: Feed^) extends Mutable:
  update def advance(): Int = stream.refill()

def adopt(consume stream: Feed^): Cursor^ = Cursor(stream)

class Builder extends Mutable:
  private var total: Int = 0
  // GOTCHA: normal members must precede consume methods — a consume method's Builder^
  // result hides Builder.this for all LATER members in the template (class bodies are
  // treated as statement sequences), so declaring `sum` after `add` is a separation
  // failure ("Illegal access to {Builder.this} which is hidden").
  def sum: Int = total
  consume def add(value: Int): Builder^ = { total += value; this }

def use(): Int =
  val stream = Feed()
  val cursor = adopt(stream)
  cursor.advance()
  Builder().add(1).add(2).sum
