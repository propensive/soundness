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
import rudiments.*
import vacuous.*

import language.dynamics

case class Catalog[KeyType, ValueType](values: Map[Text, ValueType]):
  def apply(accessor: (`*`: Proxy[KeyType]) ?=> Proxy[KeyType]): ValueType =
    values(accessor(using Proxy[KeyType]()).label.vouch(using Unsafe))

  def map[ValueType2](lambda: ValueType => ValueType2): Catalog[KeyType, ValueType2] =
    Catalog(values.view.mapValues(lambda).to(Map))

  def revise(lambda: (`*`: Proxy[KeyType]) ?=> Proxy[?] ~> ValueType): Catalog[KeyType, ValueType] =
    val partialFunction = lambda(using Proxy())
    Catalog[KeyType, ValueType](values.map: (key, value) =>
      key -> partialFunction.applyOrElse(Proxy(key), _ => value))

  def place[ResultType]
     (lambda: (catalog: Catalog[KeyType, ValueType], `*`: Proxy[KeyType]) ?=> ResultType)
          : ResultType =
    lambda(using this, Proxy())
