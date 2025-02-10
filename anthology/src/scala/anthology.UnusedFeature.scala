/*
    Anthology, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package anthology

import anticipation.*
import gossamer.*

enum UnusedFeature[VersionType](val name: Text):
  case Imports(strict: Boolean) extends UnusedFeature[3.3](t"imports")
  case Privates extends UnusedFeature[3.3 | 3.4 | 3.5 | 3.6](t"privates")
  case Locals extends UnusedFeature[3.3 | 3.4 | 3.5 | 3.6](t"locals")
  case Explicits extends UnusedFeature[3.3 | 3.4 | 3.5 | 3.6](t"explicits")
  case Implicits extends UnusedFeature[3.3 | 3.4 | 3.5 | 3.6](t"implicits")
  case Params extends UnusedFeature[3.3 | 3.4 | 3.5 | 3.6](t"params")
  case Linted extends UnusedFeature[3.3 | 3.4 | 3.5 | 3.6](t"linted")
