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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.quoted.*

import anticipation.*
import contingency.*
import cordillera.*
import distillate.*
import fulminate.*
import gigantism.*
import locomotion.*
import parasite.*
import prepositional.*
import proscenium.*

object grpcInternal:
  // Derive a client stub for a service interface of `@rpc`-annotated methods. Each
  // method takes a single request message and returns either a response message
  // (unary) or a `Progression` of them (server-streaming); the generated body delegates to
  // `GrpcChannel`, with the `:path` built from `service` and the method's own name.
  // The protobuf codecs and the call's error capabilities are summoned at the call
  // site, so `remote` must be invoked where they are in scope.
  def remote[interface: Type](channel: Expr[GrpcChannel], service: Expr[Text])
  :   Macro[interface] =

    import quotes.reflect.*

    val rpcType = TypeRepr.of[rpc].typeSymbol
    val interface = TypeRepr.of[interface]

    val rpcMethods = interface.typeSymbol.declaredMethods.filter: method =>
      val annotations = method.annotations ++ method.allOverriddenSymbols.flatMap(_.annotations)
      annotations.exists(_.tpe.typeSymbol == rpcType)

    def decls(classSymbol: Symbol) = rpcMethods.map: method =>
      Symbol.newMethod(classSymbol, method.name, method.info, Flags.EmptyFlags, Symbol.noSymbol)

    val parents = scala.collection.immutable.List(TypeTree.of[Object], TypeTree.of[interface])

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
      val runSym = classSymbol.declaredMethod(method.name).head

      DefDef(runSym, {
        case scala.collection.immutable.List(params) =>
          given Quotes = runSym.asQuotes
          val argument = params.head
          val name = Expr(method.name.tt)

          // The call's error capabilities are captured from the `remote` call site, so
          // the derived methods can have the interface's plain (raises-free) signatures.
          def tactic[error <: Hazard: Type]: Expr[Tactic[error]] =
            Expr.summon[Tactic[error]].getOrElse:
              halt(m"a contextual ${TypeRepr.of[Tactic[error]].show} instance is required")

          // Summoned at the expansion site, outside the quote: a `given Monitor` declared inside
          // the quote would resolve to itself and recurse.
          def monitorExpr: Expr[Monitor] = Expr.summon[Monitor] match
            case Some(monitor) => monitor
            case _             => halt(m"a contextual `Monitor` instance is required")

          def encoder[request: Type]: Expr[request is Encodable in Protobuf] =
            Expr.summon[request is Encodable in Protobuf].getOrElse:
              halt(m"no ${TypeRepr.of[request is Encodable in Protobuf].show} instance was found")

          def decoder[response: Type]: Expr[response is Decodable in Protobuf] =
            Expr.summon[response is Decodable in Protobuf].getOrElse:
              halt(m"no ${TypeRepr.of[response is Decodable in Protobuf].show} instance was found")

          runSym.info.absolve match
            case MethodType(_, scala.collection.immutable.List(parameter), result) => parameter.asType.absolve match
              case '[request] =>
                val enc = encoder[request]
                val req = argument.asExprOf[request]

                result.asType.absolve match
                  case '[Progression[response]] =>
                    val dec = decoder[response]

                    Some:
                      ' {
                          given Monitor = ${monitorExpr}
                          given Tactic[GrpcError] = ${tactic[GrpcError]}
                          given Tactic[Http2Error] = ${tactic[Http2Error]}
                          given Tactic[AsyncError] = ${tactic[AsyncError]}
                          given Tactic[ProtobufError] = ${tactic[ProtobufError]}

                          $channel.serverStreaming[request, response]
                            (Grpc.Method($service, $name), $req)(using $enc, $dec)
                        }

                      . asTerm

                  case '[response] =>
                    val dec = decoder[response]

                    Some:
                      ' {
                          given Monitor = ${monitorExpr}
                          given Tactic[GrpcError] = ${tactic[GrpcError]}
                          given Tactic[Http2Error] = ${tactic[Http2Error]}
                          given Tactic[AsyncError] = ${tactic[AsyncError]}
                          given Tactic[ProtobufError] = ${tactic[ProtobufError]}

                          $channel.unary[request, response]
                            (Grpc.Method($service, $name), $req)(using $enc, $dec)
                        }

                      . asTerm

            case _ =>
              halt:
                m"""
                  every `@rpc` method in ${TypeRepr.of[interface].show} must take exactly one
                  request parameter
                """

        case _ =>
          halt:
            m"""
              every `@rpc` method in ${TypeRepr.of[interface].show} must take exactly one parameter
              list
            """
      })

    val moduleDef = ClassDef.module(module, parents, body = defDefs)

    Block(moduleDef.toList, Ref(module)).asExprOf[interface]
