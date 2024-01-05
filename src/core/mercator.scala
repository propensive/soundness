/*
    Mercator, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import fulminate.*

import scala.quoted.*
import scala.compiletime.*
import scala.collection.BuildFrom

object Point:
  given Point[[T] =>> Either[?, T]] with
    def point[ValueType](value: ValueType): Either[Nothing, ValueType] = Right(value)

  inline given [PointType[_]]: Point[PointType] =
    ${Mercator.point[PointType]}

trait Point[PointType[_]]:
  def point[ValueType](value: ValueType): PointType[ValueType]

object Functor:
  inline given [FunctorType[_]]: Functor[FunctorType] = ${Mercator.functor[FunctorType]}

trait Functor[FunctorType[_]]:
  def point[ValueType](value: ValueType): FunctorType[ValueType]
  
  def map
      [ValueType, ValueType2]
      (value: FunctorType[ValueType])
      (lambda: ValueType => ValueType2)
      : FunctorType[ValueType2]

object Monad:
  inline given [MonadType[_]]: Monad[MonadType] = ${Mercator.monad[MonadType]}

trait Monad[MonadType[_]] extends Functor[MonadType]:
  def flatMap
      [ValueType, ValueType2]
      (value: MonadType[ValueType])
      (lambda: ValueType => MonadType[ValueType2])
      : MonadType[ValueType2]

object Mercator:
  def point[TypeConstructorType[_]: Type](using Quotes): Expr[Point[TypeConstructorType]] =
    import quotes.reflect.*
    val pointType = TypeRepr.of[TypeConstructorType].typeSymbol
    val companion = Ref(pointType.companionModule)
    
    val applyMethods = companion.symbol.typeRef.typeSymbol.methodMembers.filter: method =>
      method.tree match
        case DefDef("apply", List(TypeParamClause(List(tpe)), terms), _, _) =>
          terms match
            case TermParamClause(List(ValDef(_, tRef, _))) => tRef.tpe.asMatchable match
              case AppliedType(ap, List(tRef)) =>
                ap.typeSymbol == defn.RepeatedParamClass && tRef.typeSymbol == tpe.symbol
              
              case _ =>
                tRef.tpe.typeSymbol == tpe.symbol
            
            case _ => false
        case _ => false
      
    if applyMethods.length == 1
    then '{
      new Point[TypeConstructorType]:
        def point[ValueType](value: ValueType): TypeConstructorType[ValueType] =
          ${
            companion
                .select(applyMethods(0))
                .appliedToType(TypeRepr.of[ValueType])
                .appliedTo('value.asTerm)
                .asExprOf[TypeConstructorType[ValueType]]
          }
    }
    else if applyMethods.length == 0
    then fail(msg"the companion object ${pointType.name} has no candidate apply methods")
    else fail(msg"the companion object ${pointType.name} has more than one candidate apply method")

  def functor[FunctorType[_]](using Type[FunctorType], Quotes): Expr[Functor[FunctorType]] =
    import quotes.reflect.*
    val functorType = TypeRepr.of[FunctorType].typeSymbol
    
    val mapMethods = functorType.methodMembers.filter: method =>
      method.tree match
        case DefDef("map", _, _, _) => true
        case _                      => false
    
    val pointExpr: Expr[Point[FunctorType]] = Expr.summon[Point[FunctorType]].getOrElse:
      fail(msg"could not find Point value for ${functorType.name}")

    lazy val makeFunctor = '{
      new Functor[FunctorType]:
        def point[ValueType](value: ValueType): FunctorType[ValueType] = ${pointExpr}.point(value)

        def map
            [ValueType, ValueType2]
            (value: FunctorType[ValueType])(lambda: ValueType => ValueType2): FunctorType[ValueType2] =
          ${'value.asTerm.select(mapMethods(0)).appliedToType(TypeRepr.of[ValueType2])
              .appliedTo('lambda.asTerm).asExprOf[FunctorType[ValueType2]]}
    }

    if mapMethods.length == 1 then makeFunctor else if mapMethods.length == 0 then
      fail(msg"the type ${functorType.name} has no map methods")
    else fail(msg"the type ${functorType.name} has more than one possible map method")
    
  def monad[MonadType[_]](using Type[MonadType], Quotes): Expr[Monad[MonadType]] =
    import quotes.reflect.*
    val monadType = TypeRepr.of[MonadType].typeSymbol
    
    val flatMapMethods = monadType.methodMembers.filter: method =>
      method.tree match
        case DefDef("flatMap", _, _, _) => true
        case _                      => false
    
    val functorExpr: Expr[Functor[MonadType]] = Expr.summon[Functor[MonadType]].getOrElse:
      fail(msg"could not find Functor value for ${monadType.name}")

    lazy val makeMonad = '{
      new Monad[MonadType]:
        def point[ValueType](value: ValueType): MonadType[ValueType] = ${functorExpr}.point(value)

        def map
            [ValueType, ValueType2]
            (value: MonadType[ValueType])(lambda: ValueType => ValueType2): MonadType[ValueType2] =
          ${functorExpr}.map(value)(lambda)
        
        def flatMap
            [ValueType, ValueType2]
            (value: MonadType[ValueType])(lambda: ValueType => MonadType[ValueType2])
            : MonadType[ValueType2] =
          ${'value.asTerm
              .select(flatMapMethods(0))
              .appliedToType(TypeRepr.of[ValueType2])
              .appliedTo('lambda.asTerm)
              .asExprOf[MonadType[ValueType2]]}
    }

    if flatMapMethods.length == 1 then makeMonad
    else if flatMapMethods.length == 0
    then fail(msg"the type ${monadType.name} has no flatMap methods")
    else fail(msg"the type ${monadType.name} has more than one possible flatMap method")

extension [ValueType, FunctorType[_]]
    (using functor: Functor[FunctorType])(value: FunctorType[ValueType])
  def map[ValueType2](lambda: ValueType => ValueType2): FunctorType[ValueType2] = functor.map(value)(lambda)

extension [ValueType, MonadType[_]]
    (using monad: Monad[MonadType])(value: MonadType[ValueType])
  def flatMap[ValueType2](lambda: ValueType => MonadType[ValueType2]): MonadType[ValueType2] =
    monad.flatMap(value)(lambda)

extension [MonadType[_], CollectionType[ElemType] <: Iterable[ElemType], ElemType]
    (elems: CollectionType[MonadType[ElemType]])
    (using monad: Monad[MonadType])

  def sequence
      (using buildFrom: BuildFrom[List[ElemType], ElemType, CollectionType[ElemType]])
      : MonadType[CollectionType[ElemType]] =
    
    def recur
        (todo: Iterable[MonadType[ElemType]], acc: MonadType[List[ElemType]])
        : MonadType[List[ElemType]] =
      if todo.isEmpty then acc else recur(todo.tail, acc.flatMap { xs => todo.head.map(_ :: xs) })
        
    recur(elems, monad.point(List())).map(_.reverse.to(buildFrom.toFactory(Nil)))
    
extension [CollectionType[ElemType] <: Iterable[ElemType], ElemType]
    (elems: CollectionType[ElemType])

  def traverse[ElemType2, MonadType[_]](lambda: ElemType => MonadType[ElemType2])
      (using monad: Monad[MonadType],
          buildFrom: BuildFrom[List[ElemType2], ElemType2, CollectionType[ElemType2]])
      : MonadType[CollectionType[ElemType2]] =
    
    def recur
        (todo: Iterable[ElemType], acc: MonadType[List[ElemType2]])
        : MonadType[List[ElemType2]] =
      if todo.isEmpty then acc
      else recur(todo.tail, acc.flatMap { xs => lambda(todo.head).map(_ :: xs) })
        
    recur(elems, monad.point(List())).map(_.reverse.to(buildFrom.toFactory(Nil)))
    
