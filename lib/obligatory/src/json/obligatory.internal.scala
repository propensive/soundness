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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import scala.annotation.*
import scala.quoted.*

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gigantism.*
import inimitable.*
import jacinta.*
import parasite.*
import prepositional.*
import rudiments.*
import spectacular.*
import urticose.*
import vacuous.*


object internal:
  def methodNames[interface: Type]: Macro[Set[Text]] =
    import quotes.reflect.*

    val rpcType = TypeRepr.of[rpc].typeSymbol

    val names = TypeRepr.of[interface].typeSymbol.declaredMethods.filter: method =>
      val annotations = method.annotations ++ method.allOverriddenSymbols.flatMap(_.annotations)
      annotations.exists(_.tpe.typeSymbol == rpcType)

    . map: method =>
        Expr(method.name.tt)

    '{Set(${Varargs(names)}*)}

  def dispatcher[interface: Type](target: Expr[interface]): Macro[Json => Optional[Json]] =
    import quotes.reflect.*

    val rpcType = TypeRepr.of[rpc].typeSymbol
    val bareType = TypeRepr.of[bare].typeSymbol
    val interface = TypeRepr.of[interface]

    val rpcMethods = interface.typeSymbol.declaredMethods.filter: method =>
      val annotations = method.annotations ++ method.allOverriddenSymbols.flatMap(_.annotations)
      annotations.exists(_.tpe.typeSymbol == rpcType)

    ' {
        json =>
          import dynamicJsonAccess.enabled
          given Tactic[JsonError] = strategies.throwUnsafely

          (safely(json.method.as[Text]): @scala.unchecked) match
            case Unset =>
              val response = json.as[JsonRpc.Response]

              response.id.let: id =>
                JsonRpc.receive(id.as[Text], response.result)

              Unset

            case method: Text =>
              val request = json.as[JsonRpc.Request]

              $ {
                  val cases = rpcMethods.map: method =>
                    val params = method.paramSymss.head.map: param =>
                      // A `@bare` parameter is decoded from the whole `params` value; every other
                      // parameter is decoded from the like-named field of a `params` object.
                      val bare = param.annotations.exists(_.tpe.typeSymbol == bareType)

                      param.info.asType.absolve match
                        // Summon the schema-carrying `Json.Decodable` rather than a
                        // plain `Decodable in Json`: for a `Json`-typed parameter the
                        // latter is ambiguous between jacinta's carrier and
                        // distillate's universal `generic` identity decoder.
                        case '[param] => Expr.summon[param is Json.Decodable] match
                          case Some(decoder) =>
                            val source =
                              if bare then '{request.params}
                              else '{request.params(${Expr(param.name)})}

                            '{$decoder.decoded($source)}.asTerm

                          case None =>
                            halt:
                              m"""
                                could not find a contextual
                                ${TypeRepr.of[param is Decodable in Json].show} instance for the
                                parameter ${param.name} of ${method.name}
                              """

                    val application = Apply(Select(target.asTerm, method), params)

                    val result: TypeRepr = method.info.absolve match
                      case MethodType(_, _, result) => result

                      case _ =>
                        halt:
                          m"""
                            the type of method ${method.name} has the unexpected type,
                            ${method.info.show}
                          """

                    val rhs = result.asType.absolve match
                      case '[Unit]   => '{${application.asExpr} yet Unset}

                      case '[result] => Expr.summon[result is Encodable in Json] match
                        case Some(encoder) =>
                          ' {
                              JsonRpc.Response
                                ( "2.0",
                                  $encoder.encode(${application.asExprOf[result]}),
                                  request.id )

                              . json
                            }

                        case None =>
                          halt:
                            m"""
                              could not find a contextual
                              ${TypeRepr.of[result is Encodable in Json].show} instance for the
                              return type of ${method.name}
                            """

                    CaseDef(Literal(StringConstant(method.name)), None, rhs.asTerm)

                  val wildcard = Expr.summon[Tactic[JsonRpcError]] match
                    case Some(tactic) =>
                      val rhs =
                        '{abort(JsonRpcError(JsonRpcError.Reason.UnknownMethod))(using $tactic)}

                      CaseDef(Wildcard(), None, rhs.asTerm)

                    case None =>
                      halt(728, m"could not find a contextual `Tactic[JsonRpcError]` instance")

                  Match('method.asTerm, cases :+ wildcard).asExprOf[Optional[Json]]
                }
      }

  def remote[interface: Type](url: Expr[HttpUrl]): Macro[interface] =
    import quotes.reflect.*

    val rpcType = TypeRepr.of[rpc].typeSymbol
    val interface = TypeRepr.of[interface]

    val rpcMethods = interface.typeSymbol.declaredMethods.filter: method =>
      val allAnnotations = method.annotations ++ method.allOverriddenSymbols.flatMap(_.annotations)
      allAnnotations.exists(_.tpe.typeSymbol == rpcType)

    def decls(classSymbol: Symbol) = rpcMethods.map: method =>
      Symbol.newMethod(classSymbol, method.name, method.info, Flags.EmptyFlags, Symbol.noSymbol)

    val parents  = List(TypeTree.of[Object], TypeTree.of[interface])

    val module =
      Symbol.newModule
        ( owner    = Symbol.spliceOwner,
          name     = Symbol.freshName(interface.typeSymbol.name),
          modFlags = Flags.EmptyFlags,
          clsFlags = Flags.EmptyFlags,
          parents  = _ => parents.map(_.tpe),
          decls    = decls,
          privateWithin = Symbol.noSymbol )

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
                  case Some('{$encoder: (`param` `is` Encodable `in` Json)}) =>
                    val name = Expr(param.name)
                    '{$name -> ${encoder}.encoded(${ident.asExprOf[param]})}

                  case _ =>
                    halt:
                      m"""
                        could not find a contextual ${TypeRepr.of[param is Encodable in Json].show}
                        instance for parameter parameter ${param.name} of ${method.name}
                      """

              case _ =>
                halt:
                  m"""
                    all remote methods in ${TypeRepr.of[interface].show} must have a single
                    parameter list
                  """

          val result: TypeRepr = runSym.info.absolve match
            case MethodType(_, _, result) => result

            case _ =>
              halt:
                m"the type of method ${method.name} has the unexpected type, ${method.info.show}"

          val notification = result.typeSymbol == TypeRepr.of[Unit].typeSymbol
          val id = if notification then '{Unset} else Expr(Uuid().show)
          val methodName = Expr(method.name.tt)

          Expr.summon[Monitor] match
            case Some(monitor) => Expr.summon[Probate] match
              case Some(probate) => Expr.summon[Online] match
                case Some(online) =>
                  if notification then Some:
                    ' {
                        val json = Map(${Varargs(entries)}*).json

                        safely[AsyncError]:
                          JsonRpc.notification($url, $methodName, json)
                            ( using $monitor, $probate, $online )

                          . await()

                        . yet (())
                      }

                    . asTerm
                  else result.asType.absolve match
                    case '[result] => Expr.summon[result is Json.Decodable] match
                      case Some(decoder) =>
                        Some:
                          ' {
                              val json = Map(${Varargs(entries)}*).json

                              unsafely:
                                JsonRpc.request($url, $methodName, json)
                                  ( using $monitor, $probate, $online )

                                . await()
                                . decode[result](using $decoder)
                            }

                          . asTerm

                      case _ =>
                        halt:
                          m"""
                            a contextual ${TypeRepr.of[result is Decodable in Json].show} was not
                            found
                          """

                case _ => halt(593, m"a contextual `Online` instance is required")

              case _ => halt(169, m"a contextual `Probate` instance is required")

            case _ => halt(323, m"a contextual `Monitor` instance is required")

        case _ =>
          halt(69, m"the method ${method.name} must have exactly one parameter list")
      })

    val modDef = ClassDef.module(module, parents, body = defDefs)


    Block(modDef.toList, Ref(module)).asExprOf[interface]

  def client[interface: Type](rpc: Expr[JsonRpc]): Macro[interface] =
    import quotes.reflect.*

    val rpcType = TypeRepr.of[rpc].typeSymbol
    val interface = TypeRepr.of[interface]

    val rpcMethods = interface.typeSymbol.declaredMethods.filter: method =>
      val allAnnotations = method.annotations ++ method.allOverriddenSymbols.flatMap(_.annotations)
      allAnnotations.exists(_.tpe.typeSymbol == rpcType)

    def decls(classSymbol: Symbol) = rpcMethods.map: method =>
      Symbol.newMethod(classSymbol, method.name, method.info, Flags.EmptyFlags, Symbol.noSymbol)

    val parents  = List(TypeTree.of[Object], TypeTree.of[interface])

    val module =
      Symbol.newModule
        ( owner    = Symbol.spliceOwner,
          name     = Symbol.freshName(interface.typeSymbol.name),
          modFlags = Flags.EmptyFlags,
          clsFlags = Flags.EmptyFlags,
          parents  = _ => parents.map(_.tpe),
          decls    = decls,
          privateWithin = Symbol.noSymbol )

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
                  case Some('{$encoder: (`param` `is` Encodable `in` Json)}) =>
                    val name = Expr(param.name)
                    '{$name -> ${encoder}.encoded(${ident.asExprOf[param]})}

                  case _ =>
                    halt:
                      m"""
                        could not find a contextual ${TypeRepr.of[param is Encodable in Json].show}
                        instance for parameter ${param.name} of ${method.name}
                      """

              case _ =>
                halt:
                  m"""
                    all remote methods in ${TypeRepr.of[interface].show} must have a single
                    parameter list
                  """

          val result: TypeRepr = runSym.info.absolve match
            case MethodType(_, _, result) => result

          val notification = result.typeSymbol == TypeRepr.of[Unit].typeSymbol
          val id = if notification then '{Unset} else Expr(Uuid().show)
          val methodName = Expr(method.name.tt)

          if notification then Some:
            ' {
                val json = Map(${Varargs(entries)}*).json
                JsonRpc.notification($rpc, $methodName, json)
              }

            . asTerm
          else result.asType.absolve match
            case '[result] => Expr.summon[result is Json.Decodable] match
              case Some(decoder) =>
                Some:
                  ' {
                      val json = Map(${Varargs(entries)}*).json

                      unsafely:
                        JsonRpc.request($rpc, $methodName, json)
                        . await()
                        . decode[result](using $decoder)
                    }

                  . asTerm

              case _ =>
                halt:
                  m"""
                    could not find a contextual ${TypeRepr.of[result is Decodable in Json].show}
                    instance for the return type of ${method.name}
                  """

        case _ =>
          halt(69, m"the method ${method.name} must have exactly one parameter list")
      })

    val modDef = ClassDef.module(module, parents, body = defDefs)


    Block(modDef.toList, Ref(module)).asExprOf[interface]
