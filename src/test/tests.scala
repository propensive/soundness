/*
    Hypotenuse, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hypotenuse

import probably.*
import gossamer.*

object Tests extends Suite(t"Hypotenuse tests"):
  def run(): Unit =
    suite(t"Addition tests"):
      test(t"Construct an unsigned integer"):
        val left: U64 = 123
        
    suite(t"Inequality tests"):
      test(t"1.2 < x < 1.4"):
        List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.2 < _ < 1.4)
      .assert(_ == List(1.3))
      
      test(t"1.2 < x <= 1.4"):
        List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.2 < _ <= 1.4)
      .assert(_ == List(1.3, 1.4))
      
      test(t"1.2 <= x < 1.4"):
        List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.2 <= _ < 1.4)
      .assert(_ == List(1.2, 1.3))
      
      test(t"1.2 <= x <= 1.4"):
        List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.2 <= _ <= 1.4)
      .assert(_ == List(1.2, 1.3, 1.4))
      
      test(t"2 < x < 4"):
        List(1, 2, 3, 4, 5).filter(2 < _ < 4)
      .assert(_ == List(3))
      
      test(t"2 < x <= 4"):
        List(1, 2, 3, 4, 5).filter(2 < _ <= 4)
      .assert(_ == List(3, 4))
      
      test(t"2 <= x < 4"):
        List(1, 2, 3, 4, 5).filter(2 <= _ < 4)
      .assert(_ == List(2, 3))
      
      test(t"2 <= x <= 4"):
        List(1, 2, 3, 4, 5).filter(2 <= _ <= 4)
      .assert(_ == List(2, 3, 4))
      
      test(t"2L < x < 4L"):
        List(1L, 2L, 3L, 4L, 5L).filter(2L < _ < 4L)
      .assert(_ == List(3L))
      
      test(t"2L < x <= 4L"):
        List(1L, 2L, 3L, 4L, 5L).filter(2L < _ <= 4L)
      .assert(_ == List(3L, 4L))
      
      test(t"2L <= x < 4L"):
        List(1L, 2L, 3L, 4L, 5L).filter(2L <= _ < 4L)
      .assert(_ == List(2L, 3L))
      
      test(t"2L <= x <= 4L"):
        List(1L, 2L, 3L, 4L, 5L).filter(2L <= _ <= 4L)
      .assert(_ == List(2L, 3L, 4L))
      
      test(t"2F < x < 4F"):
        List(1F, 2F, 3F, 4F, 5F).filter(2F < _ < 4F)
      .assert(_ == List(3F))
      
      test(t"2F < x <= 4F"):
        List(1F, 2F, 3F, 4F, 5F).filter(2F < _ <= 4F)
      .assert(_ == List(3F, 4F))
      
      test(t"2F <= x < 4F"):
        List(1F, 2F, 3F, 4F, 5F).filter(2F <= _ < 4F)
      .assert(_ == List(2F, 3F))
      
      test(t"2F <= x <= 4F"):
        List(1F, 2F, 3F, 4F, 5F).filter(2F <= _ <= 4F)
      .assert(_ == List(2F, 3F, 4F))
      
      test(t"'2' < x < '4'"):
        List('1', '2', '3', '4', '5').filter('2' < _ < '4')
      .assert(_ == List('3'))
      
      test(t"'2' < x <= '4'"):
        List('1', '2', '3', '4', '5').filter('2' < _ <= '4')
      .assert(_ == List('3', '4'))
      
      test(t"'2' <= x < '4'"):
        List('1', '2', '3', '4', '5').filter('2' <= _ < '4')
      .assert(_ == List('2', '3'))
      
      test(t"'2' <= x <= '4'"):
        List('1', '2', '3', '4', '5').filter('2' <= _ <= '4')
      .assert(_ == List('2', '3', '4'))
      
      test(t"2.toByte < x < 4.toByte"):
        List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(2.toByte < _ < 4.toByte)
      .assert(_ == List(3.toByte))
      
      test(t"2.toByte < x <= 4.toByte"):
        List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(2.toByte < _ <= 4.toByte)
      .assert(_ == List(3.toByte, 4.toByte))
      
      test(t"2.toByte <= x < 4.toByte"):
        List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(2.toByte <= _ < 4.toByte)
      .assert(_ == List(2.toByte, 3.toByte))
      
      test(t"2.toByte <= x <= 4.toByte"):
        List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(2.toByte <= _ <= 4.toByte)
      .assert(_ == List(2.toByte, 3.toByte, 4.toByte))
      
      test(t"2.toShort < x < 4.toShort"):
        List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(2.toShort < _ < 4.toShort)
      .assert(_ == List(3.toShort))
      
      test(t"2.toShort < x <= 4.toShort"):
        List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(2.toShort < _ <= 4.toShort)
      .assert(_ == List(3.toShort, 4.toShort))
      
      test(t"2.toShort <= x < 4.toShort"):
        List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(2.toShort <= _ < 4.toShort)
      .assert(_ == List(2.toShort, 3.toShort))
      
      test(t"2.toShort <= x <= 4.toShort"):
        List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(2.toShort <= _ <= 4.toShort)
      .assert(_ == List(2.toShort, 3.toShort, 4.toShort))



      test(t"1.2 > x > 1.4"):
        List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.4 > _ > 1.2)
      .assert(_ == List(1.3))
      
      test(t"1.2 >= x > 1.4"):
        List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.4 >= _ > 1.2)
      .assert(_ == List(1.3, 1.4))
      
      test(t"1.2 > x >= 1.4"):
        List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.4 > _ >= 1.2)
      .assert(_ == List(1.2, 1.3))
      
      test(t"1.2 >= x >= 1.4"):
        List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.4 >= _ >= 1.2)
      .assert(_ == List(1.2, 1.3, 1.4))
      
      test(t"2 > x > 4"):
        List(1, 2, 3, 4, 5).filter(4 > _ > 2)
      .assert(_ == List(3))
      
      test(t"2 >= x > 4"):
        List(1, 2, 3, 4, 5).filter(4 >= _ > 2)
      .assert(_ == List(3, 4))
      
      test(t"2 > x >= 4"):
        List(1, 2, 3, 4, 5).filter(4 > _ >= 2)
      .assert(_ == List(2, 3))
      
      test(t"2 >= x >= 4"):
        List(1, 2, 3, 4, 5).filter(4 >= _ >= 2)
      .assert(_ == List(2, 3, 4))
      
      test(t"2L > x > 4L"):
        List(1L, 2L, 3L, 4L, 5L).filter(4L > _ > 2L)
      .assert(_ == List(3L))
      
      test(t"2L >= x > 4L"):
        List(1L, 2L, 3L, 4L, 5L).filter(4L >= _ > 2L)
      .assert(_ == List(3L, 4L))
      
      test(t"2L > x >= 4L"):
        List(1L, 2L, 3L, 4L, 5L).filter(4L > _ >= 2L)
      .assert(_ == List(2L, 3L))
      
      test(t"2L >= x >= 4L"):
        List(1L, 2L, 3L, 4L, 5L).filter(4L >= _ >= 2L)
      .assert(_ == List(2L, 3L, 4L))
      
      test(t"2F > x > 4F"):
        List(1F, 2F, 3F, 4F, 5F).filter(4F > _ > 2F)
      .assert(_ == List(3F))
      
      test(t"2F >= x > 4F"):
        List(1F, 2F, 3F, 4F, 5F).filter(4F >= _ > 2F)
      .assert(_ == List(3F, 4F))
      
      test(t"2F > x >= 4F"):
        List(1F, 2F, 3F, 4F, 5F).filter(4F > _ >= 2F)
      .assert(_ == List(2F, 3F))
      
      test(t"2F >= x >= 4F"):
        List(1F, 2F, 3F, 4F, 5F).filter(4F >= _ >= 2F)
      .assert(_ == List(2F, 3F, 4F))
      
      test(t"'2' > x > '4'"):
        List('1', '2', '3', '4', '5').filter('4' > _ > '2')
      .assert(_ == List('3'))
      
      test(t"'2' >= x > '4'"):
        List('1', '2', '3', '4', '5').filter('4' >=_ > '2')
      .assert(_ == List('3', '4'))
      
      test(t"'2' > x >= '4'"):
        List('1', '2', '3', '4', '5').filter('4' > _ >= '2')
      .assert(_ == List('2', '3'))
      
      test(t"'2' >= x >= '4'"):
        List('1', '2', '3', '4', '5').filter('4' >= _ >= '2')
      .assert(_ == List('2', '3', '4'))
      
      test(t"2.toByte > x > 4.toByte"):
        List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(4.toByte > _ > 2.toByte)
      .assert(_ == List(3.toByte))
      
      test(t"2.toByte >= x > 4.toByte"):
        List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(4.toByte >= _ > 2.toByte)
      .assert(_ == List(3.toByte, 4.toByte))
      
      test(t"2.toByte > x >= 4.toByte"):
        List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(4.toByte > _ >= 2.toByte)
      .assert(_ == List(2.toByte, 3.toByte))
      
      test(t"2.toByte >= x >= 4.toByte"):
        List(1.toByte, 2.toByte, 3.toByte, 4.toByte, 5.toByte).filter(4.toByte >= _ >= 2.toByte)
      .assert(_ == List(2.toByte, 3.toByte, 4.toByte))
      
      test(t"2.toShort > x > 4.toShort"):
        List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(4.toShort > _ > 2.toShort)
      .assert(_ == List(3.toShort))
      
      test(t"2.toShort >= x > 4.toShort"):
        List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(4.toShort >= _ > 2.toShort)
      .assert(_ == List(3.toShort, 4.toShort))
      
      test(t"2.toShort > x >= 4.toShort"):
        List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(4.toShort > _ >= 2.toShort)
      .assert(_ == List(2.toShort, 3.toShort))
      
      test(t"2.toShort >= x >= 4.toShort"):
        List(1.toShort, 2.toShort, 3.toShort, 4.toShort, 5.toShort).filter(4.toShort >= _ >= 2.toShort)
      .assert(_ == List(2.toShort, 3.toShort, 4.toShort))
        
      
