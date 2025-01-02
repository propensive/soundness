/*
    Feudalism, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package feudalism

import language.experimental.captureChecking

class Semaphore():
  private var count: Int = 0

  def access[ResultType](evaluate: => ResultType): ResultType =
    synchronized:
      while count == -1 do wait()
      count += 1

    try evaluate finally synchronized:
      count -= 1
      notify()

  def isolate[ResultType](evaluate: => ResultType): ResultType =
    synchronized:
      while count != 0 do wait()
      count = -1

    try evaluate finally synchronized:
      count = 0
      notify()
