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
    
    test(t"Point for Set"):
      summon[Point[Set]].point(1)
    .assert(_ == Set(1))
    
    test(t"Point for Vector"):
      summon[Point[Vector]].point(1)
    .assert(_ == Vector(1))
    
    test(t"Point for Try"):
      summon[Point[scala.util.Try]].point(1)
    .assert(_ == scala.util.Success(1))
    
    test(t"Point for Either"):
      summon[Point[[T] =>> Either[Any, T]]].point(1)
    .assert(_ == Right(1))
    
    test(t"Point for Ordering does not exist"):
      captureCompileErrors:
        summon[Point[Ordering]].point(1)
      .map(_.message)
    .assert(_ == List("mercator: the companion object Ordering has no candidate apply methods"))
