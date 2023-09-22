package aviation

import rudiments.*

object LeapSeconds:
  // Bits represent leap seconds in years from 1972 (MSB) to 2035 (LSB). Leap seconds will be abolished after
  // 2035, so a 64-bit integer is just sufficient to store all possible leap seconds, including those which have
  // not yet been determined.
  //                               1972     1980     1988     1996     2004     2012     2020     2028   2035
  private var june: Long     = bin"10000000 01110100 00001110 01000000 00000000 10010000 00000000 00000000"
  private var december: Long = bin"11111111 00000001 01100001 00100000 01001000 00001000 00000000 00000000"
  
  def addLeapSecond(year: Int, midYear: Boolean): Unit =
    if midYear then june |= (Long.MinValue >> (year - 1972)) else december |= (Long.MinValue >> (year - 1972))

  def before(year: Int, midYear: Boolean): Int =
    inline def ones(long: Long): Int = java.lang.Long.bitCount(long)
    val offset = 2036 - year
    val base = ones(december >>> offset) + 10
    
    if midYear then
      if year < 1972 then 10 else if year == 1972 then 11 else base + ones(june >>> (offset - 1))
    else
      if year <= 1972 then 10 else base + ones(june >>> offset)

