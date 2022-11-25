package quagmire

import eucalyptus.*
import gossamer.*
import rudiments.*
import annotation.*

import logging.stdout

import language.experimental.pureFunctions

enum Change[+T]:
  case Ins(right: Int, value: T)
  case Del(left: Int, value: T)
  case Keep(left: Int, right: Int, value: T)

import Change.*

case class Diff[T](changes: Change[T]*)

given Show[(Int, Int)] =
  case (x, y) => t"($x,$y)"

object Diff:

  def diff[T: Show](a: IArray[T], b: IArray[T], compare: (T, T) -> Boolean = { (a: T, b: T) => a == b }): List[IArray[(Int, Int)]] =
    val n = a.size
    val m = b.size
    Log.info(t"Diffing $a and $b")
    
    @tailrec
    def distance(i: Int = 2, trace: List[IArray[(Int, Int)]] = List(IArray((0, 0)))): List[IArray[(Int, Int)]] =
      //Log.info(t"distance($i, $trace)")
      val array = Array.fill[(Int, Int)](i)((0, 0))
      def last(x: Int): (Int, Int) = (trace.head(x)(0), trace.head(x)(0) - 2 + i - 2*x)

      @tailrec
      def count(x: Int, y: Int): (Int, Int) = if x >= n || y >= m || a(x) != b(y) then (x, y) else count(x + 1, y + 1)
      
      array.indices.foreach: j =>
        Log.info(t"array.index $j")
        array(j) =
          if j == 0 then
            val c = last(j)
            Log.info(t"${c} vs ${c(0) - 2 + i - 2*j}")
            count(c(0), c(1) + 1)
          else if j == i - 1 then
            val c = last(j - 1)
            Log.info(t"${c} vs ${c(0) - 2 + i - 2*(j - 1)}")
            count(c(0) + 1, c(1))
          else
            val c1 = last(j - 1)
            Log.info(t"${c1} vs ${c1(0) - 2 + i - 2*(j - 1)}")
            val d1 = count(c1(0) + 1, c1(1))
  
            val c2 = last(j)
            Log.info(t"${c2} vs ${c2(0) - 2 + i - 2*j}")
            val d2 = count(c2(0), c2(1) + 1)
            
            if d1(0) > d2(0) then d1 else d2
      
      Log.info(t"Array ${array}")
      Log.info(t"Looking for ${(n, m)}")
      array.indexOf((n, m)) match
        case -1 =>
          distance(i + 1, array.immutable(using Unsafe) :: trace)
        case p  =>
          Log.info(t"p = $p")
          array.immutable(using Unsafe) :: trace

    distance()

given Realm = Realm(t"quagmire")

/*

   |      0     1     2     3     4     5
----+-------------------------------    7,6
    |                                 / 
 4  |                             7,3   5,6
    |                           /
 3  |                       5,2   7,5
    |                     /
 2  |                 3,1   5,4   5,5
    |               /     \     /     \
 1  |           1,0   2,2   4,5   4,6   
    |         /     \           \
 0  |     0,0   0,1   2,4   3,6   ...

          y = x - 2*i + j

*/        