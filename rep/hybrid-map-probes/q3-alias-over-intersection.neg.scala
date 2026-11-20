// Q3: can the alias borrow mutable classification by intersecting the stdlib Map with caps.Mutable?
//EXPECT: pure type
import language.experimental.captureChecking
import language.experimental.separationChecking

import scala.collection.immutable as sci

object lib:
  opaque type Map[key, +value] = sci.Map[key, value] & caps.Mutable
  object Map:
    def allocate[key, value](): Map[key, value]^ = sci.Map.empty[key, value].asInstanceOf[Map[key, value]^]
    def freeze[key, value](consume map: Map[key, value]^): Map[key, value]^{} = caps.unsafe.unsafeAssumePure(map)

import lib.*

def good(): Map[Int, Int]^{} =
  val map: Map[Int, Int]^ = Map.allocate()
  Map.freeze(map)
