/*
    Aviation, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package aviation

import rudiments.*
import hypotenuse.*

object LeapSeconds:
  // Bits represent leap seconds in years from 1972 (MSB) to 2035 (LSB). Leap seconds will be abolished after
  // 2035, so a 64-bit integer is just sufficient to store all possible leap seconds, including those which have
  // not yet been determined.
  //                               1972     1980     1988     1996     2004     2012     2020     2028   2035
  private var june: Long     = bin"10000000 01110100 00001110 01000000 00000000 10010000 00000000 00000000"
  private var december: Long = bin"11111111 00000001 01100001 00100000 01001000 00001000 00000000 00000000"
  
  def addLeapSecond(year: Int, midYear: Boolean): Unit =
    if midYear then june |= (Long.MinValue >> (year - 1972)) else december |= (Long.MinValue >> (year - 1972))

  def before(year: Int, plusSixMonths: Boolean): Int =
    before((year - 1972)*2 + (if plusSixMonths then 1 else 0))

  private def before(n: Int): Int =
    inline def ones(long: Long): Int = long.countBits
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
    unixTime + before(if unixTime > leapSecond(n) then n else n - 1)*1000L
    