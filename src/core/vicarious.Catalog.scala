/*
    Vicarious, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import vacuous.*

import language.dynamics

case class Catalog[KeyType, ValueType](values: IArray[Any]):
  def size: Int = values.length

  inline def apply(accessor: (`*`: Proxy[KeyType, ValueType, 0]) ?=> Proxy[KeyType, ValueType, ?])
          : ValueType =
    values(accessor(using Proxy()).id.vouch(using Unsafe)).asInstanceOf[ValueType]

  def map[ValueType2](lambda: ValueType => ValueType2): Catalog[KeyType, ValueType2] =
    Catalog(values.map[Any] { field => lambda(field.asInstanceOf[ValueType]) })

  def tie[ResultType](using proxy: Proxy[KeyType, ValueType, 0])
     (lambda: (catalog: this.type, `*`: proxy.type) ?=> ResultType)
          : ResultType =
    lambda(using this, proxy)

  def braid[ValueType2](right: Catalog[KeyType, ValueType2])[ResultType]
     (lambda: (ValueType, ValueType2) => ResultType)
          : Catalog[KeyType, ResultType] =
    Catalog(IArray.tabulate[Any](values.length): index =>
      lambda(values(index).asInstanceOf[ValueType], right.values(index).asInstanceOf[ValueType2]))

extension [KeyType, ValueType](catalog: Catalog[KeyType, ValueType])
  def brush(using proxy: Proxy[KeyType, ValueType, Nat])
     (lambda: (`*`: proxy.type) ?=> Proxy[KeyType, ValueType, Nat] ~> ValueType)
          : Catalog[KeyType, ValueType] =

    val partialFunction = lambda(using proxy)

    Catalog(IArray.tabulate(catalog.size): index =>
      partialFunction.applyOrElse(Proxy[KeyType, ValueType, index.type](), _ => catalog.values(index)))
