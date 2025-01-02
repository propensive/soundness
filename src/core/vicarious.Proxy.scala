/*
    Vicarious, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import rudiments.*
import vacuous.*

object Proxy:
  transparent inline given derived[KeyType, ValueType]: Proxy[KeyType, ValueType] =
    ${Vicarious.proxy[KeyType, ValueType]}


case class Proxy[KeyType, ValueType](label: Optional[Text] = Unset) extends Selectable:
  def selectDynamic(key: String)(using catalog: Catalog[KeyType, ValueType])
          : ValueType | Proxy[KeyType, ValueType] =
    val label2 = label.lay(key.tt)(_+".".tt+key.tt)
    catalog.values.at(label2).or(Proxy(label2))

object MatchProxy:
  transparent inline given derived[KeyType, ValueType]: MatchProxy[KeyType, ValueType] =
    ${Vicarious.matchProxy[KeyType, ValueType]}

case class MatchProxy[KeyType, ValueType](label: Optional[Text] = Unset) extends Selectable:
  def selectDynamic(key: String): MatchProxy[KeyType, ValueType] =
    MatchProxy(label.lay(key.tt)(_+".".tt+key.tt))

  def unapply(scrutinee: MatchProxy[KeyType, ValueType]): Boolean = scrutinee.label == label
