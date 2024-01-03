/*
    Rudiments, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import vacuous.*

import scala.deriving.*

import java.util.concurrent.atomic as juca

import language.dynamics
import language.experimental.captureChecking

type Nat = Int & Singleton
type Label = String & Singleton

extension [ValueType](value: ValueType)
  def only[ValueType2](fn: PartialFunction[ValueType, ValueType2]): Optional[ValueType2] =
    Some(value).collect(fn).getOrElse(Unset)
  
  def unit: Unit = ()
  def waive: Any => ValueType = _ => value
  def twin: (ValueType, ValueType) = (value, value)
  def triple: (ValueType, ValueType, ValueType) = (value, value, value)
  def puncture(point: ValueType): Optional[ValueType] = if value == point then Unset else point
  inline def is[ValueSubtype <: ValueType]: Boolean = value.isInstanceOf[ValueSubtype]

  transparent inline def matchable(using Unsafe): ValueType & Matchable =
    value.asInstanceOf[ValueType & Matchable]

  def contextually[ResultType](block: ValueType ?=> ResultType): ResultType = block(using value)

extension [ValueType](inline value: => ValueType)
  inline def pipe[ResultType](inline fn: ValueType => ResultType): ResultType = fn(value)

  inline def tap(inline block: ValueType => Unit): ValueType =
    val result: ValueType = value
    block(result)
    result
  
  inline def also(inline block: => Unit): ValueType =
    val result: ValueType = value
    block
    result

case class Counter(first: Int = 0):
  private val atomicInt: juca.AtomicInteger = juca.AtomicInteger(first)
  def apply(): Int = atomicInt.incrementAndGet()

export Rudiments.&

extension [ProductType <: Product](product: ProductType)(using mirror: Mirror.ProductOf[ProductType])
  def tuple: mirror.MirroredElemTypes = Tuple.fromProductTyped(product)

extension [TupleType <: Tuple](tuple: TupleType)
  def to[ProductType](using mirror: Mirror.ProductOf[ProductType]): ProductType = mirror.fromProduct(tuple)
