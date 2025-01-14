/*
    Imperial, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package imperial

import ambience.*
import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

object Home extends BaseLayout(Unset)(using BaseLayout.Dir(true, Nil)):
  object Cache extends BaseLayout(t".cache")
  object Config extends BaseLayout(t".config")

  object Local extends BaseLayout(t".local"):
    object Bin extends BaseLayout(t"bin")
    object Lib extends BaseLayout(t"lib")
    object Share extends BaseLayout(t"share")
    object State extends BaseLayout(t"state")
