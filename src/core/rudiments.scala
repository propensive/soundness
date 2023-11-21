/*
    Rudiments, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import anticipation.*

import scala.deriving.*

import java.util.concurrent.atomic as juca

import language.dynamics
import language.experimental.captureChecking

type Nat = Int & Singleton
type Label = String & Singleton

extension [ValueType](value: ValueType)
  def only[ValueType2](fn: PartialFunction[ValueType, ValueType2]): Maybe[ValueType2] =
    Some(value).collect(fn).getOrElse(Unset)
  
  def unit: Unit = ()
  def waive: Any => ValueType = _ => value
  def twin: (ValueType, ValueType) = (value, value)
  def triple: (ValueType, ValueType, ValueType) = (value, value, value)
  def puncture(point: ValueType): Maybe[ValueType] = if value == point then Unset else point
  inline def is[ValueSubtype <: ValueType]: Boolean = value.isInstanceOf[ValueSubtype]

  transparent inline def matchable(using Unsafe): ValueType & Matchable =
    value.asInstanceOf[ValueType & Matchable]

case class Counter(first: Int = 0):
  private val atomicInt: juca.AtomicInteger = juca.AtomicInteger(first)
  def apply(): Int = atomicInt.incrementAndGet()

export Rudiments.&

extension [ProductType <: Product](product: ProductType)(using mirror: Mirror.ProductOf[ProductType])
  def tuple: mirror.MirroredElemTypes = Tuple.fromProductTyped(product)

extension [TupleType <: Tuple](tuple: TupleType)
  def to[ProductType](using mirror: Mirror.ProductOf[ProductType]): ProductType = mirror.fromProduct(tuple)

erased trait Unsafe
erased val Unsafe: Unsafe = ###

object Default:
  given Default[Int](0)
  given [ValueType: ValueOf]: Default[ValueType](valueOf[ValueType])
  given Default[Long](0L)
  given Default[Text]("".tt)
  given Default[String]("")
  given [ElemType]: Default[List[ElemType]](Nil)
  given [ElemType]: Default[Set[ElemType]](Set())
  given [ElemType]: Default[Vector[ElemType]](Vector())

trait Default[+ValueType](default: ValueType):
  def apply(): ValueType = default
