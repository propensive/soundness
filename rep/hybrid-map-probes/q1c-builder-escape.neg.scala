// Q1c: the lent builder must not leak from the build block.
//EXPECT: Error
import language.experimental.captureChecking
import language.experimental.separationChecking

import scala.collection.immutable as sci

object lib:
  opaque type Map[key, +value] = sci.Map[key, value]
  final class Builder[key, value] private[lib] (private val table: java.util.HashMap[key, value])
  extends caps.Mutable:
    update def update(key: key, value: value): Unit = table.put(key, value)

  def build[key, value](lambda: Builder[key, value]^ => Unit): Map[key, value] =
    val table = new java.util.HashMap[key, value]()
    lambda(new Builder(table))
    sci.HashMap.from(scala.jdk.CollectionConverters.MapHasAsScala(table).asScala)

import lib.*

def leak(): Unit =
  var later: () => Unit = () => ()
  val map = build[Int, Int]: builder =>
    later = () => builder(1) = 2
  later()
