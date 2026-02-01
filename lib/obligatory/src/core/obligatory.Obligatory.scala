                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.53.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package obligatory

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import hyperbole.*
import inimitable.*
import jacinta.*
import parasite.*
import prepositional.*
import proscenium.*
import revolution.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

import scala.annotation.*
import scala.quoted.*

import errorDiagnostics.stackTraces
import stdioSources.virtualMachine.ansi

object Obligatory:
  given Realm = realm"obligatory"

  def dispatcher[interface: Type](target: Expr[interface]): Macro[Json => Optional[Json]] =
    import quotes.reflect.*

    val rpcType = TypeRepr.of[rpc].typeSymbol
    val interface = TypeRepr.of[interface]

    val rpcMethods = interface.typeSymbol.declaredMethods.filter: method =>
      method.annotations.exists(_.tpe.typeSymbol == rpcType)

    ' {
        json =>
          import dynamicJsonAccess.enabled
          given Tactic[JsonError] = strategies.throwUnsafely
          val request = json.as[JsonRpc.Request]
          val method = request.method

          $ {
              val cases = rpcMethods.map: method =>
                val params = method.paramSymss.head.map: param =>
                  param.info.asType.absolve match
                    case '[param] => Expr.summon[param is Decodable in Json] match
                      case Some(decoder) =>
                        '{$decoder.decoded(request.params(${Expr(param.name)}))}
                        . asTerm
                      case None =>
                        halt(m"could not find a JSON decoder for parameter ${param.name} of method ${method.name}")

                val application = Apply(Select(target.asTerm, method), params)

                val rtnType: TypeRepr = method.info.absolve match
                  case MethodType(_, _, rtn) => rtn

                val rhs = rtnType.asType match
                  case '[Unit] => '{${application.asExpr} yet Unset}
                  case '[rtn] => Expr.summon[rtn is Encodable in Json] match
                    case Some(encoder) =>
                      '{JsonRpc.Response("2.0", $encoder.encode(${application.asExprOf[rtn]}), request.id).json}
                    case None =>
                      halt(m"could not find a JSON encoder for return type of method ${method.name}")

                CaseDef(Literal(StringConstant(method.name)), None, rhs.asTerm)

              Match('method.asTerm, cases).asExprOf[Optional[Json]]
            }
      }



  def remote[interface: Type](url: Expr[HttpUrl]): Macro[interface] =
    import quotes.reflect.*
    val rpcType = TypeRepr.of[rpc].typeSymbol
    val interface = TypeRepr.of[interface]

    val rpcMethods = interface.typeSymbol.declaredMethods.filter: method =>
      method.annotations.exists(_.tpe.typeSymbol == rpcType)

    def decls(classSymbol: Symbol) = rpcMethods.map: method =>
      Symbol.newMethod(classSymbol, method.name, method.info, Flags.EmptyFlags, Symbol.noSymbol)

    val parents  = List(TypeTree.of[Object], TypeTree.of[interface])

    val module = Symbol.newModule
     (owner    = Symbol.spliceOwner,
      name     = Symbol.freshName(interface.typeSymbol.name),
      modFlags = Flags.EmptyFlags,
      clsFlags = Flags.EmptyFlags,
      parents  = _ => parents.map(_.tpe),
      decls    = decls,
      privateWithin = Symbol.noSymbol)

    val classSymbol = module.moduleClass

    val defDefs = rpcMethods.map: method =>
      val paramSymbols = method.paramSymss.head
      val runSym = classSymbol.declaredMethod(method.name).head

      DefDef(runSym, {
        case List(params) =>
          given Quotes = runSym.asQuotes
          val entries = params.zip(paramSymbols).map: (ident, param) =>
            param.info.asType match
              case '[param] =>
                Expr.summon[param is Encodable in Json] match
                  case Some('{$encoder: (`param` `is` Encodable `in` Json) }) =>
                    val name = Expr(param.name)
                    '{ $name -> ${encoder}.encoded(${ident.asExprOf[param]}) }
                  case _ =>
                    halt(m"no encoder found for parameter ${param.name} of method ${method.name}")
              case _ =>
                halt(m"all remote methods in ${TypeRepr.of[interface].show} must have a single parameter list")

          val rtnType: TypeRepr = runSym.info.absolve match
            case MethodType(_, _, rtn) => rtn

          val notification = rtnType.typeSymbol == TypeRepr.of[Unit].typeSymbol
          val id = if notification then '{Unset} else Expr(Uuid().show)
          val methodName = Expr(method.name.tt)

          Expr.summon[Monitor] match
            case Some(monitor) => Expr.summon[Codicil] match
              case Some(codicil) => Expr.summon[Online] match
                case Some(online) =>
                  if notification then Some:
                    ' {
                        val json = Map(${Varargs(entries)}*).json
                        unsafely:
                          JsonRpc.request($url, $methodName, json)
                           (using $monitor, $codicil, $online)
                          . await()
                      }
                    . asTerm
                  else rtnType.asType.absolve match
                    case '[rtn] => Expr.summon[rtn is Decodable in Json] match
                      case Some(decoder) =>
                        Some:
                          ' {
                              val json = Map(${Varargs(entries)}*).json
                              unsafely:
                                JsonRpc.request($url, $methodName, json)
                                 (using $monitor, $codicil, $online)
                                . await()
                                . decode[rtn](using $decoder)
                            }
                          . asTerm

                      case _ => halt(m"a contextual ${TypeRepr.of[rtn is Decodable in Json].show} was not found")
                case _ => halt(m"a contextual Online instance is required")
              case _ => halt(m"a contextual Codicil instance is required")
            case _ => halt(m"a contextual Monitor instance is required")
        case _ => halt(m"the method ${method.name} must have exactly one parameter list")
      })

    val modDef = ClassDef.module(module, parents, body = defDefs)


    Block(modDef.toList, Ref(module)).asExprOf[interface]
