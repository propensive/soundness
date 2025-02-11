/*
    Probably, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package probably

import rudiments.*

object Surface:
  def collapse(todo: List[Juncture], done: List[Surface]): List[Surface] = todo match
    case Nil =>
      done.reverse

    case head :: tail =>
      val todo2 = tail.takeWhile(head.contains(_))
      collapse(tail.drop(todo2.length), Surface(head, collapse(todo2, Nil)) :: done)

case class Surface(juncture: Juncture, children: List[Surface]):
  def covered(hits: Set[Int]): Boolean =
    hits.contains(juncture.id) && children.all(_.covered(hits))

  def uncovered(hits: Set[Int]): Surface =
    Surface(juncture, children.filter(!_.covered(hits)).map(_.uncovered(hits)))
