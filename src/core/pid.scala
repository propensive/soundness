/*
    Rudiments, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import language.experimental.captureChecking

object ExitStatus:
  def apply(value: Int): ExitStatus = if value == 0 then Ok else Fail(value)

enum ExitStatus:
  case Ok
  case Fail(status: Int)

  def apply(): Int = this match
    case Ok           => 0
    case Fail(status) => status

case class Pid(value: Long):
  override def toString(): String = "\u21af"+value
