package mosquito

import gossamer.*
import rudiments.*
import probably.*
import larceny.*

object Tests extends Suite(t"Mosquito tests"):
  def run(): Unit =
    test(t"Create a Euclidean of Ints"):
      Euclidean(1, 2, 3)
    .assert(_ == Euclidean(1, 2, 3))
    
    test(t"A Euclidean of Ints infers the correct size"):
      demilitarize:
        val vector: Euclidean[Int, 3] = Euclidean(1, 3, 4)
      .map(_.message)
    .assert(_ == Nil)
    
    test(t"Type error if size is incorrect"):
      demilitarize:
        val vector: Euclidean[Int, 2] = Euclidean(1, 3, 4)
      .map(_.errorId)
    .assert(_ == List(ErrorId.TypeMismatchID))
    
    test(t"Type error if type is incorrect"):
      demilitarize:
        val vector: Euclidean[String, 3] = Euclidean(1, 3, 4)
      .map(_.errorId)
    .assert(_ == List(ErrorId.TypeMismatchID))

    test(t"Calculate integer dot-product"):
      Euclidean(1, 2, 3).dot(Euclidean(4, 3, 7))
    .assert(_ == 31)
    
    test(t"Calculate Double dot-product"):
      Euclidean(0.1, 0.2, 0.3).dot(Euclidean(0.4, 0.3, 0.7))
    .assert(_ meets 0.31 +/- 0.000001)
    
    test(t"Calculate integer cross-product"):
      Euclidean(1, 2, 3).cross(Euclidean(4, 3, 7))
    .assert(_ == Euclidean(5, 5, -5))
    
    test(t"Calculate Double cross-product"):
      Euclidean(1.4, 2.4, 3.8).cross(Euclidean(4.9, 3.6, 0.7))
    .assert(_ == Euclidean(-12.0, 17.64, -6.72))
    