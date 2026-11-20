// Q1b: the flat table as a pure stdlib immutable Map: a java.util.HashMap inside, the tree
// cached lazily behind updated/removed. Mutation is invisible to the checker.
import language.experimental.captureChecking
import language.experimental.separationChecking

import scala.collection.immutable as sci

final class Flat[key, +value](table: java.util.HashMap[key, value @scala.annotation.unchecked.uncheckedVariance])
extends sci.AbstractMap[key, value]:
  @caps.unsafe.untrackedCaptures private var tree: sci.Map[key, value] | Null = null

  private def persistent: sci.Map[key, value] =
    val cached = tree
    if cached != null then cached else
      val builder = sci.HashMap.newBuilder[key, value]
      table.forEach((key, value) => builder.addOne((key, value)))
      val built = builder.result()
      tree = built
      built

  override def size: Int = table.size
  override def contains(key: key): Boolean = table.containsKey(key)
  def get(key: key): Option[value] = if table.containsKey(key) then Some(table.get(key)) else None
  def iterator: Iterator[(key, value)] = persistent.iterator
  def removed(key: key): sci.Map[key, value] = persistent.removed(key)
  def updated[value2 >: value](key: key, value: value2): sci.Map[key, value2] = persistent.updated(key, value)

object lib:
  opaque type Map[key, +value] = sci.Map[key, value]
  object Map:
    def at[key, value](map: Map[key, value], key: key): Option[value] = map.get(key)
    def define[key, value](map: Map[key, value], key: key, value: value): Map[key, value] = map.updated(key, value)

  // The lent builder: the only exclusive thing, wrapping the table the result will own.
  final class Builder[key, value] private[lib] (private val table: java.util.HashMap[key, value])
  extends caps.Mutable:
    update def update(key: key, value: value): Unit = table.put(key, value)

  def build[key, value](lambda: Builder[key, value]^ => Unit): Map[key, value] =
    val table = new java.util.HashMap[key, value]()
    lambda(new Builder(table))
    new Flat(table)

import lib.*

case class Holder(map: Map[Int, Int])

def use(): () -> Option[Int] =
  val map: Map[Int, Int] = build[Int, Int]: builder =>
    builder(1) = 2
    builder(3) = 4
  val holder = Holder(Map.define(map, 5, 6))
  () => Map.at(holder.map, 1)
