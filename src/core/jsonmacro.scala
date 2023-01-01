/*
    Euphemism, version 0.4.0. Copyright 2019-23 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package euphemism

import rudiments.*
import gossamer.*

import scala.quoted.*
import scala.collection.mutable as scm

import unsafeExceptions.canThrowAny

/*
object EuphemismMacro:
  def deriveReader[T: Type](using Quotes): Expr[Json.Reader[T]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val fields = tpe.typeSymbol.caseFields.collect { case f: Symbol if f.isValDef => f }
    val companion = tpe.typeSymbol.companionModule
    val constructor = tpe.typeSymbol.primaryConstructor.tree

    def union(fields: List[Symbol]): TypeRepr =
      fields match
        case Nil =>
          TypeRepr.of[Nothing]
        
        case head :: tail =>
          val typeRepr = head.tree match
            case v: ValDef => v.tpt.tpe
            case d: DefDef => d.returnTpt.tpe
            case _         => throw Mistake("case field is not of the expected AST type")
  
          typeRepr.asType match
            case '[t] =>
              Expr.summon[Json.Reader[t]].getOrElse:
                report.errorAndAbort(
                    s"cannot find Reader for case field of type ${TypeRepr.of[t].show}")
              match
                case '{ $r: Json.Reader[`t`] { type E = e } } =>
                  if TypeRepr.of[e] == TypeRepr.of[Nothing] then union(tail)
                  else OrType(TypeRepr.of[e], union(tail))
                
                case '{ $r: Json.Reader[`t`] } =>
                  union(tail)
                
                case other =>
                  report.errorAndAbort(s"Unexpectedly found "+other.show)
            
            case _ =>
              throw Mistake("The case '[t] should be irrefutable")
    
    union(fields).asType match
      case '[errorUnion] => '{
        new Json.MapReader[T]({ (vs: scm.Map[String, JValue]) => ${
          def recur(fields: List[Symbol]): List[Expr[Any]] = fields match
            case Nil =>
              Nil
            
            case head :: tail =>
              val typeRepr = head.tree match
                case valDef: ValDef => valDef.tpt.tpe
                case defDef: DefDef => defDef.returnTpt.tpe
                case _              => throw Mistake(
                                            "case field is not of the expected AST type")
      
              typeRepr.asType match
                case '[paramType] =>
                  Expr.summon[Json.Reader[paramType]].getOrElse:
                    val typeName = TypeRepr.of[paramType].show
                    report.errorAndAbort(
                        s"euphemism: cannot find Reader for case field of type $typeName")
                  match
                    case '{ $reader: Json.Reader[`paramType`] } =>
                      val label = Expr(head.name)
                      val expr: Expr[`paramType`] =
                        '{
                          $reader.read(vs.get($label).getOrElse(
                              throw JsonAccessError(JsonAccessError.Reason.Label(Text($label)))))
                        }
                      expr :: recur(tail)
                    
                    case _ =>
                      throw Mistake("Expr.summon should never retrieve a value which doesn't "+
                                            "match the first case")
                
                case _ =>
                  throw Mistake("the pattern '[paramType] should be irrefutable")
            
          val ap = companion.declaredMethod("apply").head
            
          Ref(companion).select(ap).appliedToArgs(recur(fields).map(_.asTerm)).asExprOf[T]
        } }):
          type E = errorUnion & Exception
      }

      case _ => throw Mistake("the case '[errorUnion] should be irrefutable")
*/