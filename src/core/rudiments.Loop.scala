/*
    Rudiments, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

object Loop:
  enum State:
    case Active, Stopping, Finished

class Loop(iteration: () => Unit):
  private var state: Loop.State = Loop.State.Active

  def stop(): Unit = synchronized:
    if state == Loop.State.Active then state = Loop.State.Stopping

  def run(): Unit =
    while state == Loop.State.Active do iteration()

    synchronized:
      state = Loop.State.Finished
