package merino

import annotation.*

trait Parser[T]:
  def parse(state: State, offset: Int): State

class State(lazyList: LazyList[IArray[Byte]]):
  var head: IArray[Byte] = lazyList.head
  var tail: LazyList[IArray[Byte]] = lazyList.tail
  var longValue: Long = 0

  @tailrec
  final def drop(n: Int): Unit = if n > 0 then
    head = tail.head
    tail = tail.tail
    drop(n - 1)

object Parser:

  def parseInt(


  given Parser[Int] with

    def parse(state: State, offset: Int): State =
      val value: Long = getInt(state.head, state.tail, offset)
      state.drop((value >> 32).toInt)

      state.longValue = value
      state

    def getInt(bytes: IArray[Byte], stream: LazyList[IArray[Byte]], start: Int): Long =
      var buf = bytes
      var cont = stream
      var cur: Int = start
      var stop: Boolean = false
      var drop: Long = 0L
      val negative = bytes(start) == 45
      var acc: Int = 0

      if negative then cur += 1

      while !stop do
        val b = bytes(cur)
        if (b & -8) == 48 || b == 56 || b == 57 then acc = acc*10 + (b - 48) else stop = true
        if acc < 0 then throw Exception("number too big")
        cur += 1

        if cur == bytes.length then
          drop += 4294967296L
          if cont.isEmpty then stop = true //throw Exception("premature end")
          else
            buf = cont.head
            cont = cont.tail
            cur = 0

      drop + cur - start
  
  def apply(stream: LazyList[IArray[Byte]]): Int =
    val state = State(stream)
    summon[Parser[Int]].parse(state, 0).longValue.toInt

    