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
import rudiments.*
import vacuous.*

class TestSuite(val name: Text, val parent: Optional[TestSuite] = Unset)(using codepoint: Codepoint):
  override def equals(that: Any): Boolean = that.matchable(using Unsafe) match
    case that: TestSuite => name == that.name && parent == that.parent
    case _               => false

  override def hashCode: Int = name.hashCode + parent.hashCode

  val id: TestId = TestId(name, parent, codepoint)
