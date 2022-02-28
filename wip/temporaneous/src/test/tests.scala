package temporaneous

import probably.*
import gossamer.*
import rudiments.*
import eucalyptus.*
import escapade.*

given Log()

object Tests extends Suite("Temporaneous tests"):
  def run(using Runner): Unit =
    for area <- List("antarctica", "europe", "africa", "asia", "northamerica", "australasia",
        "southamerica", "backzone", "etcetera", "factory", "leapseconds", "backward") do
      test(str"Parse $area file") {
        val file = scala.io.Source.fromFile(str"/home/propensive/one/res/temporaneous/$area")
        Tzdb.parse(area, file.getLines.to(LazyList))
      }.assert(_ => true)
