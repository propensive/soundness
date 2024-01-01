/*
    Serpentine, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package serpentine

import probably.*
import rudiments.*
import digression.*
import gossamer.*

object Benchmarks extends Suite(t"Serpentine Benchmarks"):
  def run(): Unit =
    suite(t"Conjunctions"):
      test(t"Find conjunction of 2-element paths"):
        val p1 = ^ / p"foo" / p"bar"
        val p2 = ^ / p"foo" / p"baz"
        p1.conjunction(p2)
      .benchmark(warmup = 500L, duration = 500L, baseline = Baseline(ratio = Ratio.Time, compare = Compare.Min))
    
      test(t"Find conjunction of 3-element paths"):
        val p1 = ^ / p"foo" / p"bar" / p"quux"
        val p2 = ^ / p"foo" / p"baz" / p"quux"
        p1.conjunction(p2)
      .benchmark(warmup = 500L, duration = 500L)
    
      test(t"Find conjunction of 4-element paths"):
        val p1 = ^ / p"foo" / p"bar" / p"quux" / p"bippy"
        val p2 = ^ / p"foo" / p"baz" / p"quux" / p"bop"
        p1.conjunction(p2)
      .benchmark(warmup = 500L, duration = 500L)
      
      test(t"Find conjunction of 5-element paths"):
        val p1 = ^ / p"foo" / p"bar" / p"quux" / p"bippy" / p"abc"
        val p2 = ^ / p"foo" / p"baz" / p"quux" / p"bop" / p"def"
        p1.conjunction(p2)
      .benchmark(warmup = 500L, duration = 500L)
      
      test(t"Find conjunction of 6-element paths"):
        val p1 = ^ / p"foo" / p"bar" / p"quux" / p"bippy" / p"abc" / p"ghi"
        val p2 = ^ / p"foo" / p"baz" / p"quux" / p"bop" / p"def" / p"jkl"
        p1.conjunction(p2)
      .benchmark(warmup = 500L, duration = 500L)
      
      test(t"Find conjunction of 7-element paths"):
        val p1 = ^ / p"foo" / p"bar" / p"quux" / p"bippy" / p"abc" / p"ghi" / p"mno"
        val p2 = ^ / p"foo" / p"baz" / p"quux" / p"bop" / p"def" / p"jkl" / p"pqr"
        p1.conjunction(p2)
      .benchmark(warmup = 500L, duration = 500L)
