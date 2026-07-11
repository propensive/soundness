// P1: the base Mutable/update model — a stateful class with an update method, used
// through an exclusive reference (full access) and a bare reference (read-only).
import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.Mutable

class Ref extends Mutable:
  private var value: Int = 0
  def get: Int = value
  update def set(x: Int): Unit = value = x

def exclusive(r: Ref^): Int = { r.set(1); r.get }
def readonly(r: Ref): Int = r.get

@main def run(): Unit =
  val r = Ref()
  r.set(2)
  println(exclusive(r) + readonly(r))
