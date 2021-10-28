/*
    Euphemism, version 0.13.0. Copyright 2019-21 Jon Pretty, Propensive OÜ.

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
                                  $reader.read(vs.get($label).getOrElse(throw JsonAccessError(Text($label))))
                                }
                              expr :: recur(tail)

                val ap = companion.declaredMethod("apply").head

                Ref(companion).select(ap).appliedToArgs(recur(fields).map(_.asTerm)).asExprOf[T]
              }
          }):
            type E = `errorUnion` & Exception
            
        }