// Q1a: a flat table cannot be BOTH a stdlib immutable Map and a caps.Mutable class.
//EXPECT: pure type
import language.experimental.captureChecking
import language.experimental.separationChecking

import scala.collection.immutable as sci

final class Flat[key, value] extends sci.AbstractMap[key, value], caps.Mutable:
  private val table: java.util.HashMap[key, value] = new java.util.HashMap()
  update def put(key: key, value: value): Unit = table.put(key, value)
  override def size: Int = table.size
  def get(key: key): Option[value] = if table.containsKey(key) then Some(table.get(key)) else None
  def iterator: Iterator[(key, value)] = sci.Map.empty[key, value].iterator
  def removed(key: key): sci.Map[key, value] = sci.Map.empty
  def updated[value2 >: value](key: key, value: value2): sci.Map[key, value2] = sci.Map.empty

def build(): Int =
  val flat: Flat[Int, Int]^ = new Flat()
  flat.put(1, 2)
  flat.size
