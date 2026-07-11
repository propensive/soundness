// P5 positive: the ensureCapacity shape — a Mutable class owning an exclusive CONCRETE
// array field that is reallocated and swapped in an update method — plus freeze on a
// locally built array. Concrete arrays support the full kernel repertoire.
import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.Mutable

class Growable extends Mutable:
  private var arr: Array[Byte]^ = new Array[Byte](16)
  private var size: Int = 0
  def at(index: Int): Byte = arr(index)
  update def add(value: Byte): Unit =
    if size == arr.length then
      val bigger = new Array[Byte](arr.length * 2)
      System.arraycopy(arr, 0, bigger, 0, size)
      arr = bigger
    arr(size) = value
    size += 1

def snapshot(): Array[Byte]^{} =
  val array = new Array[Byte](4)
  array(0) = 1
  caps.freeze(array)

def use(): Byte =
  val growable = Growable()
  growable.add(1)
  growable.at(0)
