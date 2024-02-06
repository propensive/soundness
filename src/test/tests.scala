/*
    Savagery, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package savagery

import probably.*
import gossamer.*
import spectacular.*

object Tests extends Suite(t"Savagery tests"):
  def run(): Unit =
    test(t"Simple plus sign path"):
      Path().moveTo(0!0).lineUp(2).lineLeft(2).lineUp(1).lineRight(2).lineUp(2).lineRight(1)
          .lineDown(2).lineRight(2).lineDown(1).lineLeft(2).lineDown(2).closed.xml.show
    .assert(_ == t"")