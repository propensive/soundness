/*
    Ambience, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package ambience

import language.experimental.pureFunctions

import anticipation.*
import vacuous.*

package systemProperties:
  given SystemProperties as empty:
    def apply(name: Text): Unset.type = Unset

  given SystemProperties as virtualMachine:
    def apply(name: Text): Optional[Text] = Optional(System.getProperty(name.s)).let(_.tt)

package environments:
  given Environment as empty:
    def variable(name: Text): Unset.type = Unset

  given Environment as virtualMachine:
    def variable(name: Text): Optional[Text] = Optional(System.getenv(name.s)).let(_.tt)
