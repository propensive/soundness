/*
    Mercator, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package mercator

import anticipation.*
import fulminate.*

import scala.collection.BuildFrom

given Realm = realm"mercator"

extension [ValueType, FunctorType[_]](using functor: Functor[FunctorType])
   (value: FunctorType[ValueType])
  def map[ValueType2](lambda: ValueType => ValueType2): FunctorType[ValueType2] =
    functor.map(value)(lambda)

extension [ValueType, MonadType[_]](using monad: Monad[MonadType])(value: MonadType[ValueType])
  def flatMap[ValueType2](lambda: ValueType => MonadType[ValueType2]): MonadType[ValueType2] =
    monad.flatMap(value)(lambda)

extension [MonadType[_], CollectionType[ElemType] <: Iterable[ElemType], ElemType]
   (elems: CollectionType[MonadType[ElemType]])
   (using monad: Monad[MonadType])

  def sequence(using buildFrom: BuildFrom[List[ElemType], ElemType, CollectionType[ElemType]])
  :     MonadType[CollectionType[ElemType]] =

    def recur(todo: Iterable[MonadType[ElemType]], accumulator: MonadType[List[ElemType]])
    :     MonadType[List[ElemType]] =
      if todo.isEmpty then accumulator
      else recur(todo.tail, accumulator.flatMap { xs => todo.head.map(_ :: xs) })

    recur(elems, monad.point(List())).map(_.reverse.to(buildFrom.toFactory(Nil)))

extension [CollectionType[ElemType] <: Iterable[ElemType], ElemType]
   (elems: CollectionType[ElemType])

  def traverse[ElemType2, MonadType[_]](lambda: ElemType => MonadType[ElemType2])
     (using monad:    Monad[MonadType],
            buildFrom: BuildFrom[List[ElemType2], ElemType2, CollectionType[ElemType2]])
  :     MonadType[CollectionType[ElemType2]] =

    def recur(todo: Iterable[ElemType], accumulator: MonadType[List[ElemType2]])
    :     MonadType[List[ElemType2]] =
      if todo.isEmpty then accumulator
      else recur(todo.tail, accumulator.flatMap { xs => lambda(todo.head).map(_ :: xs) })

    recur(elems, monad.point(List())).map(_.reverse.to(buildFrom.toFactory(Nil)))
