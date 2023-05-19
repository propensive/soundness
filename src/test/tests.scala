/*
    Chiaroscuro, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package chiaroscuro

import probably.*
import gossamer.*
import dissonance.*
import rudiments.*

import Change.*

case class Person(name: Text, age: Int)

import Comparison.*

object Tests extends Suite(t"Chiaroscuro tests"):
  def run(): Unit =
    suite(t"RDiff tests"):
      test(t"Two identical, short Vectors"):
        Vector(1, 2, 3).compareTo(Vector(1, 2, 3))
      .assert(_ == Comparison.Same(t"[1, 2, 3]"))

      test(t"compare two two-parameter case class instances"):
        Person(t"Jack", 12)
      .assert(_ == Person(t"Jill", 12))
