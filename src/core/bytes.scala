package rudiments

import language.experimental.captureChecking

type Bytes = IArray[Byte]

object Bytes:
  def apply(xs: Byte*): Bytes = IArray(xs*)
  def apply(long: Long): Bytes = IArray((56 to 0 by -8).map(long >> _).map(_.toByte)*)
  def empty: Bytes = IArray()
