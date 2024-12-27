/*
    Vicarious, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package vicarious

import anticipation.*
import prepositional.*
import vacuous.*

import language.dynamics

case class Proxy[KeyType](label: Optional[Text] = Unset) extends Dynamic:
  def selectDynamic(key: String): Proxy[KeyType] =
    Proxy(label.lay(key.tt)(_+".".tt+key.tt))

  def apply[ValueType]()(using catalog: Catalog[KeyType, ValueType]): ValueType =
    catalog.values(label.or("".tt))

  def applyDynamic[ValueType](key: String)()(using Catalog[KeyType, ValueType]): ValueType =
    selectDynamic(key).apply[ValueType]()
