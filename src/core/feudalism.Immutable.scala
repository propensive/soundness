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

trait Immutable[MutableType, ImmutableType]:
  def snapshot(ref: ImmutableType): ImmutableType = ref
  def make(ref: MutableType): ImmutableType

object Immutable:
  given [ElementType: ClassTag] => Immutable[Array[ElementType], IArray[ElementType]] as array:
    def make(value: Array[ElementType]): IArray[ElementType] = value.asInstanceOf[IArray[ElementType]]

    override def snapshot(value: IArray[ElementType]): IArray[ElementType] =
      val array = new Array[ElementType](value.length)
      System.arraycopy(value, 0, array, 0, value.length)
      array.asInstanceOf[IArray[ElementType]]

  given Immutable[StringBuilder, String] as stringBuilder = _.toString
  given [ValueType](using DummyImplicit) => Immutable[ValueType, ValueType] as any = identity(_)
