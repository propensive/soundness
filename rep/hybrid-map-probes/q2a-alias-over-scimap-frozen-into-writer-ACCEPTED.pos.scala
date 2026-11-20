// Q2a: with the alias over the (pure) stdlib Map, is a frozen map rejected by a writer taking Map^?
import language.experimental.captureChecking
import language.experimental.separationChecking

import scala.collection.immutable as sci

object lib:
  opaque type Map[key, +value] = sci.Map[key, value]
  def of[key, value](map: sci.Map[key, value]): Map[key, value] = map

import lib.*

// Outside the defining scope the alias is abstract, so a capture set may be written on it.
object ops:
  def allocate[key, value](): Map[key, value]^ = of(sci.Map.empty[key, value])
  def freeze[key, value](consume map: Map[key, value]^): Map[key, value]^{} = caps.unsafe.unsafeAssumePure(map)
  def put[key, value](map: Map[key, value]^, key: key, value: value): Unit = ()

def bad(): Unit =
  val map: Map[Int, Int]^ = ops.allocate()
  val frozen: Map[Int, Int]^{} = ops.freeze(map)
  ops.put(frozen, 1, 2)
