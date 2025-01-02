/*
    Camouflage, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package camouflage

import anticipation.*
import parasite.*
import rudiments.*
import vacuous.*

object Cache:
  def apply[DurationType: GenericDuration, ValueType](duration: DurationType): Cache[ValueType] =
    new Cache[ValueType](duration.milliseconds)

  def apply[ValueType](): Cache[ValueType] = new Cache(Unset)

class Cache[ValueType](lifetime: Optional[Long]):
  private var expiry: Long = Long.MaxValue
  private var value: Promise[ValueType] = Promise()

  def establish(block: => ValueType): ValueType = value.synchronized:
    if expiry < System.currentTimeMillis then value = Promise()

    if value.ready then value().vouch(using Unsafe) else block.tap: result =>
      value.offer(result)
      expiry = lifetime.lay(Long.MaxValue)(_ + System.currentTimeMillis)
