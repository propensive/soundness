/*
    Eucalyptus, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

// import anticipation.*
// import fulminate.*
// import gossamer.*
// import parasite.*
// import rudiments.*

// import scala.quoted.*

// import language.experimental.captureChecking

// object Eucalyptus:
//   given Realm = realm"eucalyptus"

//   def record[MessageType: Type, TextType: Type]
//       (level:          Expr[Level],
//        message:        Expr[MessageType],
//        log:            Expr[Log[TextType]],
//        realm:          Expr[Realm],
//        presentational: Expr[TextType is Presentational],
//        show:           Expr[Any])
//       (using Quotes)
//           : Expr[Unit] =

//     '{  val time = System.currentTimeMillis
//         val presentationalValue = $presentational

//         try
//           val castShow = $show.asInstanceOf[presentationalValue.Show[MessageType]]
//           $log.record(Entry($realm, $level, presentationalValue.show($message)(using castShow), time, $log.envelopes))
//         catch case e: Exception => ()  }

//   def route[TextType: Type]
//       (routes:         Expr[PartialFunction[Entry[TextType], Any]],
//        monitor:        Expr[Monitor],
//        presentational: Expr[TextType is Presentational])
//       (using Quotes)
//           : Expr[Log[TextType]] =

//     import quotes.reflect.*

//     def invalidRoutes(): Nothing =
//       halt(m"the routes must be specified as one or more case clauses")

//     val count: Int = routes.asTerm match
//       case Inlined(_, _, Block(List(DefDef(_, _, _, Some(Match(_, caseDefs)))), _)) =>
//         caseDefs.length

//       case _ =>
//         invalidRoutes()

//     '{
//       val loggers: Array[Logger[TextType] | Null] = new Array(${Expr(count)})

//       new TextLog[TextType]($presentational(_)):
//         def record(entry: Entry[TextType]): Unit =
//           ${
//             def partialFunction(index: Int) = routes.asTerm match
//               case Inlined(_, _, Block(List(defDef), term)) => defDef match
//                 case DefDef(ident, scrutineeType, returnType, Some(Match(matchId, caseDefs))) =>
//                   val caseDef = caseDefs(index) match
//                     case CaseDef(pattern, guard, target) => target.asExpr.absolve match
//                       case '{$target: targetType} =>
//                         def typeName = TypeRepr.of[targetType].show

//                         val logWriter: Expr[LogWriter[targetType, TextType]] =
//                           Expr.summon[LogWriter[targetType, TextType]].getOrElse:
//                             val writerName = TypeRepr.of[LogWriter[targetType, TextType]].show.tt
//                             halt(m"could not get an instance of $writerName")

//                         val action =
//                          '{
//                              loggers(${Expr(index)}) match
//                                case null => loggers(${Expr(index)}) = $logWriter.logger($target)
//                                case _    => ()

//                              loggers(${Expr(index)}).nn.put(entry)  }

//                         CaseDef(pattern, guard, action.asTerm)

//                   val matchCase = Some(Match(matchId, List(caseDef)))
//                   val definition = DefDef.copy(defDef)(ident, scrutineeType, returnType, matchCase)

//                   Block(List(definition), term).asExprOf[PartialFunction[Entry[TextType], Any]]

//                 case _ =>
//                   invalidRoutes()

//               case _ =>
//                 invalidRoutes()

//             def recur(index: Int, expr: Expr[Unit]): Expr[Unit] = if index >= count then expr else
//              '{  $expr
//                  ${partialFunction(index)}.lift(entry)  }

//             recur(0, '{()})
//           }
//       }
