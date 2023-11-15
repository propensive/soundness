/*
    Eucalyptus, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package eucalyptus

import rudiments.*
import fulminate.*
import anticipation.*
import parasite.*

import scala.quoted.*

import language.experimental.captureChecking

object Eucalyptus:
  def record
      [MessageType: Type]
      (level: Expr[Level], message: Expr[MessageType], log: Expr[Log],
          communicable: Expr[Communicable[MessageType]], realm: Expr[Realm])
      (using Quotes)
      : Expr[Unit] =
  '{
    val time = System.currentTimeMillis
    
    try $log.record(Entry($realm, $level, $communicable.message($message), time, $log.envelopes))
    catch case e: Exception => ()
  }

  def realm(context: Expr[StringContext])(using Quotes): Expr[Realm] =
    import quotes.reflect.*
    val name: String = context.valueOrAbort.parts.head
    if !name.matches("[a-z]+") then fail(msg"the realm name should comprise only of lowercase letters")
    else '{Realm.make(${Expr(name)}.tt)(using Unsafe)}

  def route(routes: Expr[PartialFunction[Entry, Any]], monitor: Expr[Monitor])(using Quotes): Expr[Log] =
    import quotes.reflect.*

    def invalidRoutes(): Nothing = fail(msg"the routes must be specified as one or more case clauses")
    
    val count: Int = routes.asTerm match
      case Inlined(_, _, Block(List(DefDef(_, _, _, Some(Match(_, caseDefs)))), _)) => caseDefs.length
      case _                                                                        => invalidRoutes()
    
    '{
      val loggers: Array[Logger | Null] = new Array(${Expr(count)})

      new Log():
        def record(entry: Entry): Unit = ${
          def partialFunction(index: Int) = routes.asTerm match
            case Inlined(_, _, Block(List(defDef), term)) => defDef match
              case DefDef(ident, scrutineeType, returnType, Some(Match(matchId, caseDefs))) =>
                val caseDef = caseDefs(index) match
                  case CaseDef(pattern, guard, target) => (target.asExpr: @unchecked) match
                    case '{$target: targetType} =>
                      def typeName = TypeRepr.of[targetType].show
                      
                      val logWriter: Expr[LogWriter[targetType]] = Expr.summon[LogWriter[targetType]].getOrElse:
                        fail(msg"could not get a logger")
                      
                      val action = '{
                        loggers(${Expr(index)}) match
                          case null => loggers(${Expr(index)}) = $logWriter.logger($target)
                          case _    => ()
                        
                        loggers(${Expr(index)}).nn.put(entry)
                      }
                      
                      CaseDef(pattern, guard, action.asTerm)
              
                val definition = DefDef.copy(defDef)(ident, scrutineeType, returnType, Some(Match(matchId,
                    List(caseDef))))
                
                Block(List(definition), term).asExprOf[PartialFunction[Entry, Any]]
               
              case _ =>
                invalidRoutes()

            case _ =>
              invalidRoutes()

          def recur(index: Int, expr: Expr[Unit]): Expr[Unit] = if index >= count then expr else '{
            $expr
            ${partialFunction(index)}.lift(entry)
          }

          recur(0, '{()})
        }
    }
