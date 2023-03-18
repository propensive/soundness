package mercator

import probably.*
import gossamer.*
import larceny.*

object Tests extends Suite(t"Mercator tests"):
  def run(): Unit =
    test(t"Point for Option"):
      summon[Point[Option]].point(1)
    .assert(_ == Some(1))
    
    test(t"Point for List"):
      summon[Point[List]].point(1)
    .assert(_ == List(1))
    
    test(t"Point for Either does not exist"):
      captureCompileErrors:
        summon[Point[[ParamType] =>> Either[?, ParamType]]].point(1)
      .map(_.message)
    .assert(_ == List("mercator: the companion object has no candidate apply methods"))