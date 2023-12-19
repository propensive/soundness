package denominative

import rudiments.*

final val Prim = Ordinal.fromOne(1)
final val Sec  = Ordinal.fromOne(2)
final val Ter  = Ordinal.fromOne(3)
final val Quat = Ordinal.fromOne(4)
final val Quin = Ordinal.fromOne(5)
final val Sen  = Ordinal.fromOne(6)
final val Sept = Ordinal.fromOne(7)
final val Oct  = Ordinal.fromOne(8)
final val Non  = Ordinal.fromOne(9)
final val Den  = Ordinal.fromOne(10)

object Denominative:
  opaque type Ordinal = Int
  opaque type Interval = Long

  extension (ordinal: Ordinal)
    @targetName("fromLast")
    inline def `unary_~`: Ordinal = -ordinal

    @targetName("plus")
    inline def +(inline cardinal: Int): Optional[Ordinal] =
      inline val number = ordinal + cardinal
      inline if number < 0 then Unset else number

    @targetName("minus")
    inline def -(inline cardinal: Int): Optional[Ordinal] = +(-cardinal)

    @targetName("lessThanOrEqualTo")
    inline def <=(inline right: Ordinal): Boolean = ordinal <= right
    
    @targetName("greaterThanOrEqualTo")
    inline def >=(inline right: Ordinal): Boolean = ordinal >= right

    inline def next: Ordinal = if ordinal < 0 then ordinal - 1 else ordinal + 1
    
    inline def previous: Optional[Ordinal] =
      inline if ordinal < 0 then inline if ordinal == 1 then Unset else ordinal - 1
      else inline if ordinal == -1 then Unset else ordinal + 1

    inline def to(inline right: Ordinal): Interval = Interval(ordinal, right)

  object Ordinal:
    inline def fromZero(inline cardinal: Int): Ordinal = cardinal + 1
    inline def fromOne(inline cardinal: Int): Ordinal = cardinal

  extension (interval: Interval)
    inline def start: Ordinal = ((interval >> 32) & 0xffffffff).toInt
    inline def end: Ordinal = (interval & 0xffffffff).toInt
    inline def contains(ordinal: Ordinal): Boolean = start <= ordinal && ordinal <= end

    inline def iterate(inline fn: Ordinal => Unit): Unit =
      var i: Ordinal = start
      while i <= end do fn(i).also(i += 1)
  
  object Interval:
    inline def apply(inline start: Ordinal, inline end: Ordinal): Interval = (start & 0xffffffffL) << 32 | end & 0xffffffffL

export Denominative.{Ordinal, Interval}