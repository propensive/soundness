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

object Monad:
  inline given [MonadType[_]]: Monad[MonadType] = ${MercatorMacros.monad[MonadType]}

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

  def functor[TypeConstructorType[_]](using Type[TypeConstructorType], Quotes): Expr[Functor[TypeConstructorType]] =
    import quotes.reflect.*
    val functorType = TypeRepr.of[TypeConstructorType].typeSymbol
    
    val mapMethods = functorType.memberMethods.filter: method =>
      method.tree match
        case DefDef("map", _, _, _) => true
        case _                      => false
    
    val pointExpr: Expr[Point[TypeConstructorType]] = Expr.summon[Point[TypeConstructorType]].getOrElse:
      report.errorAndAbort(s"mercator: could not find Point value for ${functorType.name}")

    if mapMethods.length == 1
    then
      '{
        new Functor[TypeConstructorType]:
          def point[ValueType](value: ValueType): TypeConstructorType[ValueType] =
            ${pointExpr}.point(value)
          
          def map
              [ValueType, ValueType2]
              (value: TypeConstructorType[ValueType])
              (fn: ValueType => ValueType2)
              : TypeConstructorType[ValueType2] =
            ${
              'value.asTerm
                  .select(mapMethods(0))
                  .appliedToType(TypeRepr.of[ValueType2])
                  .appliedTo('fn.asTerm)
                  .asExprOf[TypeConstructorType[ValueType2]]
            }
      }
    else report.errorAndAbort(s"mercator: the type ${functorType.name} has no candidate map methods")

    def monad[TypeConstructorType[_]](using Type[TypeConstructorType], Quotes): Expr[Monad[TypeConstructorType]] =
      import quotes.reflect.*
      val monadType = TypeRepr.of[TypeConstructorType].typeSymbol

      val flatMapMethods = monadType.memberMethods.filter: method =>
        method.tree match
          case DefDef("flatMap", _, _, _) => true
          case _                          => false

          if flatMapMethods == 1
          then
            '{
              new Monad[TypeConstructorType]:
                def point[ValueType](value: ValueType): TypeConstructorType[ValueType] = $functor.point(value)
                def map[ValueType, ValueType2](value: TypeConstructorType[ValueType])(fn: ValueType => ValueType2): TypeConstructorType[ValueType2] = $functor.map(value)(fn)
                def flatMap[ValueType, ValueType2](value: TypeConstructorType[ValueType])(fn: ValueType => TypeConstructorType[ValueType2]): TypeConstructorType[ValueType2] =
                  ${
                    'value.asTerm
                        .select(flatMapMethods(0))
                        .appliedToType(TypeRepr.of[ValueType2])
                        .appliedTo('fn.asTerm)
                        .asExprOf[TypeConstructorType[ValueType2]]
                  }
            }
          else report.errorAndAbort(s"mercator: the type ${monadType.name} has no candidate flatMap methods")
