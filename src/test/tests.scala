/*
    Camouflage, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package camouflage

import probably.*
import gossamer.*
import anticipation.*

object Tests extends Suite(t"Camouflage tests"):
  def run(): Unit =
    test(t"Check first entry is removed"):
      val cache = LruCache[Int, Text](4)
      cache(1)(t"one") // should be evicted
      cache(2)(t"two")
      cache(3)(t"three")
      cache(4)(t"four")
      cache(5)(t"five")
      cache(1)(t"ein")
    .assert(_ == t"ein")
    
    test(t"Check that an access stops an element being evicted"):
      val cache = LruCache[Int, Text](4)
      cache(1)(t"one") 
      cache(2)(t"two")
      cache(3)(t"three")
      cache(4)(t"four")
      cache(1)(t"ein")
      cache(5)(t"five")
      cache(1)(t"un")
    .assert(_ == t"one")
    
    test(t"Check that an series of accesses causes least-recently-used key to be evicted"):
      val cache = LruCache[Int, Text](4)
      cache(1)(t"one") 
      cache(2)(t"two")
      cache(3)(t"three")
      cache(4)(t"four")
      cache(1)(t"ein")
      cache(2)(t"zwei")
      cache(3)(t"drei")
      cache(5)(t"five")
      cache(4)(t"vier")
    .assert(_ == t"vier")