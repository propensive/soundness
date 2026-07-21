                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package aviation

import hypotenuse.*

object LeapSeconds:
  // Bits represent leap seconds in years from 1972 (MSB) to 2035 (LSB). Leap seconds will be
  // abolished after 2035, so a 64-bit integer is just sufficient to store all possible leap
  // seconds, including those which have not yet been determined.
  //
  // Edge behaviour outside that range. Before 1972 the table contributes nothing, so `before`/`tai`
  // report a constant TAI−UTC offset of +10 s; this is an approximation — the true historical
  // offset was sub-integer and drifting, which this integer-second model does not represent. After
  // 2035 the bits run out and the offset saturates at its final value (37 s), reflecting the
  // planned abolition of leap seconds: no further leaps are counted. Negative leap seconds (a
  // second removed, not inserted) have never occurred and cannot be encoded — `addLeapSecond` only
  // ORs bits in, so the offset is monotonically non-decreasing.
  //                            1972    1980    1988    1996    2004    2012    2020    2028   2035
  //                               ↓       ↓       ↓       ↓       ↓       ↓       ↓       ↓      ↓
  private var june:     Long = bin"1000000001110100000011100100000000000000100100000000000000000000"
  private var december: Long = bin"1111111100000001011000010010000001001000000010000000000000000000"

  def addLeapSecond(year: Year, midYear: Boolean): Unit =
    if midYear then june |= (Long.MinValue >> (year() - 1972))
    else december |= (Long.MinValue >> (year() - 1972))

  def during(year: Year, plusSixMonths: Boolean): Int =
    before((year() - 1972)*2 + (if plusSixMonths then 1 else 0))

  private def before(n: Int): Int =
    inline def ones(long: Long): Int = long.bits.ones.int

    val decemberShift = n.min(127)/2
    val juneShift = decemberShift + n%2

    10 + (if juneShift > 0 then ones(june >>> (64 - juneShift)) else 0) +
      (if decemberShift > 0 then ones(december >>> (64 - decemberShift)) else 0)

  private inline val juneToDecember = 15897600000L
  private inline val firstLeapSecond = 94694400000L
  private inline val dayLength = 86400000L
  private inline val yearLength = 31536000000L
  private inline val halfYear = 15778800000L
  private inline val firstOffset = 7*halfYear/2

  private def leapSecond(n: Int): Long =
    val year = (n + 1)/2
    val dec31 = firstLeapSecond + year*yearLength + (year/4)*dayLength
    if n%2 == 0 then dec31 else dec31 - juneToDecember

  def tai(unixTime: Long): Long =
    val n = ((unixTime - firstOffset)/halfYear).toInt
    unixTime + before(if unixTime >= leapSecond(n - 2) then n else n - 1)*1000L

  // Smeared TAI: rather than inserting a discrete leap second, spread it linearly over the 24 hours
  // centred on the leap instant (Google's "smear"), so the offset is continuous and there is no
  // 23:59:60. Outside a smear window this agrees with `tai`. Bisection locates the leap instant
  // where the discrete offset steps.
  def smearTai(unixTime: Long): Long =
    val window = 43200000L // 12 hours
    val low = tai(unixTime - window) - (unixTime - window)
    val high = tai(unixTime + window) - (unixTime + window)

    if low == high then unixTime + low else
      var lo = unixTime - window
      var hi = unixTime + window

      while hi - lo > 1 do
        val mid = (lo + hi)/2
        if tai(mid) - mid == low then lo = mid else hi = mid

      unixTime + low + (high - low)*(unixTime - (hi - window))/(2*window)

  // The inverse of `tai` (the discrete step): recover Unix time from a TAI value. `tai` is strictly
  // increasing but skips the inserted leap second, so a TAI value inside that second has no Unix
  // preimage; the closest-match binary search maps it to the following second by convention.
  def untai(taiMillis: Long): Long =
    var lo = taiMillis - 100000L
    var hi = taiMillis + 100000L

    while hi - lo > 1 do
      val mid = lo + (hi - lo)/2
      if tai(mid) < taiMillis then lo = mid else hi = mid

    if taiMillis - tai(lo) <= tai(hi) - taiMillis then lo else hi

  // The inverse of `smearTai`: recover the Unix time from a smeared-TAI value. `smearTai` is
  // strictly increasing (its slope is 1 ± 1/86400), so it is invertible; and the TAI−UTC offset is
  // well under 100 seconds (and fixed after 2035), so the Unix time lies in a narrow band around
  // `taiMillis`. Binary-search for it; the closest-match pick makes `unsmearTai(smearTai(u)) == u`
  // exact despite the integer truncation in the forward direction.
  def unsmearTai(taiMillis: Long): Long =
    var lo = taiMillis - 100000L
    var hi = taiMillis + 100000L

    while hi - lo > 1 do
      val mid = lo + (hi - lo)/2
      if smearTai(mid) < taiMillis then lo = mid else hi = mid

    if taiMillis - smearTai(lo) <= smearTai(hi) - taiMillis then lo else hi
