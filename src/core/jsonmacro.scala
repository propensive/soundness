package euphemism

import rudiments.*
import gossamer.*

import org.typelevel.jawn.*, ast.*

import scala.quoted.*
import scala.collection.mutable as scm

import unsafeExceptions.canThrowAny


object JsonMacro:
  def deriveReader[T: Type](using Quotes): Expr[Json.Reader[T]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val fields = tpe.typeSymbol.caseFields.collect { case f: Symbol if f.isValDef => f }
    val companion = tpe.typeSymbol.companionModule
    val constructor = tpe.typeSymbol.primaryConstructor.tree

    def union(fields: List[Symbol]): TypeRepr =
      fields match
        case Nil =>
          OrType(TypeRepr.of[JsonTypeError], TypeRepr.of[JsonAccessError])
        case head :: tail =>
          val typeRepr = head.tree match
            case v: ValDef => v.tpt.tpe
            case d: DefDef => d.returnTpt.tpe
            case _         => throw Impossible("case field is not of the expected AST type")
  
          typeRepr.asType match
            case '[t] =>
              Expr.summon[Json.Reader[t]].getOrElse {
                report.errorAndAbort(s"cannot find Reader for case field of type ${Type.of[t]}")
              } match
                case '{ $r: Json.Reader[`t`] { type E = e } } =>
                  if TypeRepr.of[e] == TypeRepr.of[Nothing] then union(tail) else OrType(TypeRepr.of[e], union(tail))
    
    union(fields).asType match
      case '[errorUnion] =>
        '{
          new Json.MapReader[T]({ (vs: scm.Map[String, JValue]) =>
              ${
                def recur(fields: List[Symbol]): List[Expr[Any]] =
                  fields match
                    case Nil =>
                      Nil
                    case head :: tail =>
                      val typeRepr = head.tree match
                        case valDef: ValDef => valDef.tpt.tpe
                        case defDef: DefDef => defDef.returnTpt.tpe
                        case _         => throw Impossible("case field is not of the expected AST type")
              
                      typeRepr.asType match
                        case '[paramType] =>
                          Expr.summon[Json.Reader[paramType]].getOrElse {
                            report.errorAndAbort(s"euphemism: cannot find Reader for case field of type ${Type.of[paramType]}")
                          } match
                            case '{ $reader: Json.Reader[`paramType`] { type E = e & Exception } } =>
                              val label = Expr(head.name)
                              val expr: Expr[`paramType`] =
                                '{
                                  $reader.read(vs.get($label).getOrElse(throw JsonAccessError(Txt($label))))
                                }
                              expr :: recur(tail)

                val ap = companion.declaredMethod("apply").head

                Ref(companion).select(ap).appliedToArgs(recur(fields).map(_.asTerm)).asExprOf[T]
              }
          }):
            type E = `errorUnion` & Exception
            
        }