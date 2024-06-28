package metamorphose

import scala.annotation.*

object Factorial:
  def apply(n: Int): BigInt =
    @tailrec
    def recur(i: Int, result: BigInt): BigInt = if i == 0 then result else recur(i - 1, result*i)

    recur(n, 1)

  def magnitude(n: BigInt): Int =
    @tailrec
    def recur(i: Int, result: BigInt): Int = if result > n then i else recur(i + 1, result*i)

    recur(1, 1)

  def sequence(n: Int): List[BigInt] =
    @tailrec
    def recur(i: Int, result: List[BigInt]): List[BigInt] =
      if i == n then result else recur(i + 1, result.head*i :: result)

    recur(1, List(1))
