/*
    Galilei, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package galilei

import anticipation.*
import contingency.*
import prepositional.*
import rudiments.*
import turbulence.*

object Handle:
  given readable: Tactic[StreamError] => Handle is Readable by Bytes = _.reader()
  given writable: Tactic[StreamError] => Handle is Writable by Bytes = _.writer(_)

class Handle(val reader: () => Stream[Bytes], val writer: Stream[Bytes] => Unit)
