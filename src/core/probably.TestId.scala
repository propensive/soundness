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

import anticipation.*
import digression.*
import gossamer.*
import hieroglyph.*
import hypotenuse.*
import rudiments.*
import vacuous.*

object TestId:
  given Ordering[TestId] = math.Ordering.Implicits.seqOrdering[List, Text].on(_.ids.reverse)

case class TestId(name: Text, suite: Optional[TestSuite], codepoint: Codepoint):
  val timestamp: Long = System.currentTimeMillis
  import textMetrics.uniform
  lazy val id: Text = (suite.hashCode ^ name.hashCode).hex.pad(6, Rtl, '0').keep(6, Rtl)
  lazy val ids: List[Text] =  id :: suite.let(_.id.ids).or(Nil)

  def apply[ResultType](ctx: TestContext ?=> ResultType): Test[ResultType] =
    Test[ResultType](this, ctx(using _))

  def depth: Int = suite.let(_.id.depth).or(0) + 1
