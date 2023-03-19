package mercator

import language.dynamics
import scala.quoted.*

object Functor:
  inline given [FunctorType[_]]: Functor[FunctorType] = ${MercatorMacros.functor[FunctorType]}

object Point:
  given Point[[T] =>> Either[?, T]] with
    def point[ValueType](value: ValueType): Either[Nothing, ValueType] = Right(value)

  inline given [TypeConstructorType[_]]: Point[TypeConstructorType] =
    ${MercatorMacros.point[TypeConstructorType]}

trait Point[TypeConstructorType[_]]:
  def point[ValueType](value: ValueType): TypeConstructorType[ValueType]

trait Functor[FunctorType[_]]:
  def point[ValueType](value: ValueType): FunctorType[ValueType]
  
  def map
      [ValueType, ValueType2]
      (value: FunctorType[ValueType])
      (fn: ValueType => ValueType2)
      : FunctorType[ValueType2]

trait Monad[MonadType[_]] extends Functor[MonadType]:
  def flatMap
      [ValueType, ValueType2]
      (value: MonadType[ValueType])
      (fn: ValueType => MonadType[ValueType2])
      : MonadType[ValueType2]

object MercatorMacros:

  def point[TypeConstructorType[_]: Type](using Quotes): Expr[Point[TypeConstructorType]] =
    import quotes.reflect.*
    val companion = TypeRepr.of[TypeConstructorType].typeSymbol.companionModule
    
    val applyMethods = Ref(companion).symbol.typeRef.typeSymbol.memberMethods.filter: method =>
      method.tree match
        case DefDef("apply", List(TypeParamClause(List(tpe)), terms), _, _) =>
          terms match
            case TermParamClause(List(ValDef(_, tRef, _))) if tRef.tpe.typeSymbol == tpe.symbol =>
              true
            case TermParamClause(List(ValDef(_, tRef, _))) => tRef.tpe match
              case AppliedType(ap, List(tRef)) if ap.typeSymbol == defn.RepeatedParamClass &&
                  tRef.typeSymbol == tpe.symbol =>
                true
              case _ => false
            case _                                         => false
        case _                                                        => false
      
    if applyMethods.length == 1
    then
    
    '{
      new Point[TypeConstructorType]:
        def point[ValueType](value: ValueType): TypeConstructorType[ValueType] =
          ${
            Ref(companion)
                .select(applyMethods(0))
                .appliedToType(TypeRepr.of[ValueType])
                .appliedTo('value.asTerm)
                .asExprOf[TypeConstructorType[ValueType]]
          }
    }
    else if applyMethods.length == 0
    then report.errorAndAbort(s"mercator: the companion object ${companion.name} has no candidate apply methods")
    else report.errorAndAbort(s"mercator: the companion object ${companion.name} has more than one candidate apply method")

  def functor[FunctorType[_]](using Type[FunctorType], Quotes): Expr[Functor[FunctorType]] =
    import quotes.reflect.*
    
    '{
      new Functor[FunctorType]:
        def point[ValueType](value: ValueType): FunctorType[ValueType] = ${
          ???
        }

    }
