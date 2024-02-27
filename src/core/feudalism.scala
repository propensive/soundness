/*
    Feudalism, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import scala.reflect.*

import language.experimental.captureChecking

class MutexRef[ValueType](value: ValueType, makeSnapshot: ValueType => ValueType):
  def apply(): ValueType = value
  def snapshot(): ValueType = makeSnapshot(value)

class Mutex[ValueType](initial: ValueType):
  private var count: Int = 0
  private var value: ValueType = initial

  def read[ResultType, ImmutableType]
      (using immutable: Immutable[ValueType, ImmutableType])
      (lambda: (ref: MutexRef[ImmutableType]) => ResultType)
          : ResultType =

    synchronized:
      while count == -1 do wait()
      count += 1
    
    val result = lambda(MutexRef(immutable.make(value), immutable.snapshot(_)))
    
    synchronized:
      count -= 1
      notify()

    result
  
  def mutate(lambda: ValueType => Unit): Unit =
    synchronized:
      while count != 0 do wait()
      count = -1

    lambda(value)
    
    synchronized:
      count = 0
      notify()
  
  def replace(lambda: ValueType => ValueType): Unit =
    synchronized:
      while count != 0 do wait()
      count = -1
    
    value = lambda(value)
    
    synchronized:
      count = 0
      notify()

trait Immutable[MutableType, ImmutableType]:
  def snapshot(ref: ImmutableType): ImmutableType = ref
  def make(ref: MutableType): ImmutableType

object Immutable:
  given array[ElementType: ClassTag]: Immutable[Array[ElementType], IArray[ElementType]] with
    def make(value: Array[ElementType]): IArray[ElementType] = value.asInstanceOf[IArray[ElementType]]
    
    override def snapshot(value: IArray[ElementType]): IArray[ElementType] =
      val array = new Array[ElementType](value.length)
      System.arraycopy(value, 0, array, 0, value.length)
      array.asInstanceOf[IArray[ElementType]]
  
  given stringBuilder: Immutable[StringBuilder, String] with
    def make(value: StringBuilder): String = value.toString
  
  given any[ValueType](using DummyImplicit): Immutable[ValueType, ValueType] with
    def make(value: ValueType): ValueType = value
