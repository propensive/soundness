/*
    Feudalism, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package feudalism

import language.experimental.captureChecking

class Ref[ValueType](get: ValueType):
  def apply(): ValueType = get
  def copy(): ValueType^{this} = get

class Mutex[ValueType](initial: ValueType):
  private var count: Int = 0
  private var value: ValueType = initial

  def read
      [ResultType, ImmutableType]
      (using immutable: Immutable[ValueType, ImmutableType])
      (fn: (ref: Ref[ImmutableType]^) => ResultType)
      : ResultType^{fn*} =

    synchronized:
      while count == -1 do wait()
      count += 1
    
    val result = fn(Ref(immutable(value)))
    
    synchronized:
      count -= 1
      notify()

    result
  
  def mutate(fn: ValueType => Unit): Unit =
    synchronized:
      while count != 0 do wait()
      count = -1

    fn(value)
    
    synchronized:
      count = 0
      notify()
  
  def replace(fn: ValueType => ValueType): Unit =
    synchronized:
      while count != 0 do wait()
      count = -1
    
    value = fn(value)
    
    synchronized:
      count = 0
      notify()

trait Immutable[MutableType, ImmutableType]:
  def apply(ref: MutableType): ImmutableType

object Immutable:
  given [ElementType]: Immutable[Array[ElementType], IArray[ElementType]] with
    def apply(ref: Array[ElementType]): IArray[ElementType] = ref.asInstanceOf[IArray[ElementType]]
  
  given Immutable[StringBuilder, String] with
    def apply(ref: StringBuilder): String = ref.toString
  
  given [ValueType](using DummyImplicit): Immutable[ValueType, ValueType] with
    def apply(ref: ValueType): ValueType = ref