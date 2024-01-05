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

import language.experimental.captureChecking

extension [FunctorType[+_], ValueType](value: FunctorType[ValueType]^)(using functor: Functor[FunctorType])
  def map[ValueType2](fn: ValueType => ValueType2): FunctorType[ValueType2]^{value, fn} =
    functor.map(value, fn)

extension [CofunctorType[-_], ValueType](value: CofunctorType[ValueType]^)
          (using cofunctor: Cofunctor[CofunctorType])
  def contramap[ValueType2](fn: ValueType2 => ValueType): CofunctorType[ValueType2]^{value, fn} =
    cofunctor.contramap(value, fn)

trait Functor[FunctorType[+_]]:
  def map[ValueType, ValueType2](value: FunctorType[ValueType]^, fn: ValueType => ValueType2)
          : FunctorType[ValueType2]^{value, fn}
    
object Functor:
  given list: Functor[List] with
    def map[ElemType, ElemType2](list: List[ElemType]^, fn: ElemType => ElemType2)
            : List[ElemType2]^{list, fn} =
      list.map(fn)
  
  given option: Functor[Option] with
    def map[ValueType, ValueType2](option: Option[ValueType]^, fn: ValueType => ValueType2)
            : Option[ValueType2]^{option, fn} =
      option.map(fn)

trait Cofunctor[CofunctorType[-_]]:
  def contramap[ValueType, ValueType2](value: CofunctorType[ValueType]^, fn: ValueType2 => ValueType)
               : CofunctorType[ValueType2]^{value, fn}
