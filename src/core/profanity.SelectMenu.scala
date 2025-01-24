/*
    Profanity, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package profanity

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.{where as _, *}
import rudiments.*
import vacuous.*

case class SelectMenu[ItemType](options: List[ItemType], current: ItemType)
extends Question[ItemType]:
  import Keypress.*

  def apply(keypress: TerminalEvent): SelectMenu[ItemType] = try keypress match
    case Up   => copy(current = options(0 max options.indexOf(current) - 1))
    case Down => copy(current = options(options.size - 1 min options.indexOf(current) + 1))
    case Home => copy(current = options.head)
    case End  => copy(current = options.last)
    case _    => this

  catch case e: RangeError => this

  def ask
     (using interactivity: Interactivity[TerminalEvent],
            interaction: Interaction[ItemType, SelectMenu[ItemType]])
     [ResultType]
     (lambda: Interactivity[TerminalEvent] ?=> ItemType => ResultType)
  :     ResultType raises DismissError =

    interaction(interactivity.eventStream(), this)(_(_)).lay(abort(DismissError())):
      (result, stream) => lambda(using Interactivity(stream))(result)
