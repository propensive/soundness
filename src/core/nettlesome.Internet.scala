/*
    Nettlesome, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package nettlesome

import contingency.*
import vacuous.*

import language.experimental.captureChecking

class Internet(val online: Boolean):
  def require[ResultType](block: Online ?=> ResultType)(using Tactic[OfflineError]): ResultType =
    if online then block(using Online) else abort(OfflineError())

  def appropriate[ResultType](block: Online ?=> ResultType): Optional[ResultType] =
    if online then block(using Online) else Unset
