// Q5b: mitigation: an OPAQUE second name for the frozen form (as Ascii is over a frozen array).
import language.experimental.captureChecking
import language.experimental.separationChecking

import scala.collection.immutable as sci

abstract class Carrier[key, +value] extends caps.Mutable:
  def read(key: key): Option[value]
  update def write(key: key, value: Any): Unit

final class Flat[key, value] extends Carrier[key, value]:
  private val table: java.util.HashMap[key, value] = new java.util.HashMap()
  def read(key: key): Option[value] = if table.containsKey(key) then Some(table.get(key)) else None
  update def write(key: key, value: Any): Unit = table.put(key, value.asInstanceOf[value])

final class Tree[key, +value](map: sci.Map[key, value]) extends Carrier[key, value]:
  def read(key: key): Option[value] = map.get(key)
  update def write(key: key, value: Any): Unit = ()

object lib:
  opaque type Map[key, +value] = Carrier[key, value]
  object Map:
    def allocate[key, value](): Map[key, value]^ = new Flat[key, value]()
    def freeze[key, value](consume map: Map[key, value]^): Map[key, value]^{} = caps.unsafe.unsafeAssumePure(map)
    def put[key, value](map: Map[key, value]^, key: key, value: value): Unit = map.write(key, value)
    def at[key, value](map: Map[key, value], key: key): Option[value] = map.read(key)

import lib.*

object frozen:
  opaque type Frozen[key, +value] = Map[key, value]^{}
  def freeze[key, value](consume map: Map[key, value]^): Frozen[key, value] = Map.freeze(map)
  def at[key, value](map: Frozen[key, value], key: key): Option[value] = Map.at(map, key)

import frozen.Frozen

case class Holder(map: Frozen[Int, Int])
def make(map: Frozen[Int, Int]): Holder = Holder(map)
def keep(map: Frozen[Int, Int]): () -> Option[Int] = () => frozen.at(map, 1)
def generic[collection](value: collection): collection = value
def build(): Holder =
  val map: Map[Int, Int]^ = Map.allocate()
  Map.put(map, 1, 2)
  make(generic(frozen.freeze(map)))
