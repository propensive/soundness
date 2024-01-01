/*
    Ulysses, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package ulysses

import probably.*
import gossamer.*
import gastronomy.*, hashFunctions.sha1
import anticipation.*

import language.experimental.genericNumberLiterals

object Tests extends Suite(t"Ulysses tests"):
  def run(): Unit =
    test(t"Check how many bits are required for a bloom filter"):
      val bloom = BloomFilter[Text](100, 0.01)
      bloom.bitSize
    .assert(_ == 663)
    
    test(t"Check that more bits are required to store more elements"):
      val bloom = BloomFilter[Text](1000, 0.01)
      bloom.bitSize
    .assert(_ == 6631)
    
    test(t"Check that more bits are required to store with more certainty"):
      val bloom = BloomFilter[Text](100, 0.001)
      bloom.bitSize
    .assert(_ == 994)

    val bloom = test(t"Add an element to a Bloom filter"):
      BloomFilter[Text](100, 0.001) + t"Hello world"
    .check(_.mayContain(t"Hello world"))

    test(t"Check that Bloom filter does not contain other strings"):
      !bloom.mayContain(t"hello")
    .check(identity(_))

    val bloom2 = test(t"Add multiple elements to a Bloom filter"):
      bloom ++ List(t"hello", t"world")
    .assert { b => b.mayContain(t"hello") && b.mayContain(t"world") }

      
