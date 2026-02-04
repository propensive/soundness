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
package synesthesia

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

object Synesthesia:
  given Realm = realm"synesthesia"

  def spec[interface: Type]: Macro[interface is McpSpecification] =
    import quotes.reflect.*
    val toolType = TypeRepr.of[tool].typeSymbol
    val interface = TypeRepr.of[interface]

    val toolMethods = interface.typeSymbol.declaredMethods.filter: method =>
      val allAnnotations = method.annotations ++ method.allOverriddenSymbols.flatMap(_.annotations)
      allAnnotations.exists(_.tpe.typeSymbol == toolType)

    // This has been written as a partial function because the more natural way of writing it,
    // by including `target` as a lambda variable, causes the compiler to emit bad bytecode.
    val invocation: Expr[interface ~> ((Text, Json) => Json)] =
      ' {
          {
            case target: `interface` =>
              (method: Text, input: Json) =>
                import dynamicJsonAccess.enabled
                given Tactic[JsonError] = strategies.throwUnsafely

                val request = input.as[Map[Text, Json]]

                $ {
                    val cases = toolMethods.map: method =>
                      val params = method.paramSymss.head.map: param =>
                        param.info.asType.absolve match
                          case '[param] => Expr.summon[param is Decodable in Json] match
                            case Some(decodable) =>
                              ' {
                                  given param is Decodable in Json = $decodable
                                  println(s"received ${request(${Expr(param.name)})}")
                                  request(${Expr(param.name)}).as[param]
                                }
                              . asTerm
                            case None =>
                              halt(m"""could not find a contextual
                                      `${TypeRepr.of[param].show} is Decodable in Json` instance for
                                      the parameter ${param.name} of ${method.name}""")

                      val application = Apply(Select('target.asTerm, method), params)

                      val result: TypeRepr = method.info.absolve match
                        case MethodType(_, _, result) => result

                      val rhs = result.asType.absolve match
                        case '[result] => Expr.summon[result is Encodable in Json] match
                          case Some(encoder) =>
                            ' {
                                import jsonPrinters.indented
                                val output = Map("result".tt -> $encoder.encode(${application.asExprOf[result]}))
                                println(s"returning ${output.json.show}")
                                output.json
                              }

                          case None =>
                            halt(m"""could not find a contextual
                                    `${TypeRepr.of[result].show} is Encodable in Json` instance for
                                    the return type of ${method.name}""")

                      CaseDef(Literal(StringConstant(method.name)), None, rhs.asTerm)

                    Match('method.asTerm, cases).asExprOf[Json]
                  }
            }
          }

    val entries = toolMethods.map: method =>
      val paramNames = method.paramSymss.head.map: param =>
        Expr(param.name.tt)

      val params = method.paramSymss.head.map: param =>
        param.info.asType.absolve match
          case '[param] => Expr.summon[param is Schematic in JsonSchema] match
            case Some(schematic) =>
              ' {(${Expr(param.name)}.tt, $schematic.schema())}

            case None =>
              halt(m"There was no JSON schema for ${param.name}")

      val properties = '{${Expr.ofList(params)}.toMap}

      val result: TypeRepr = method.info.absolve match
        case MethodType(_, _, result) => result

      result.asType match
        case '[result] => Expr.summon[result is Schematic in JsonSchema] match
          case Some(schematic) =>
            ' {
                val name = ${Expr(method.name)}
                val inputSchema =
                  JsonSchema.Object
                    ( properties = $properties, required = ${Expr.ofList(paramNames)} )

                val outputSchema =
                  JsonSchema.Object
                    ( properties = Map(t"result" -> $schematic.schema()),
                      required   = List(t"result") )

                Mcp.Tool(name, inputSchema = inputSchema, outputSchema = outputSchema)
              }

    val toolsList = Expr.ofList(entries)

    ' {
        new McpSpecification:
          type Self = interface
          def tools(): List[Mcp.Tool] = $toolsList

          def invoke(server: interface, method: Text, input: Json): Json =
            $invocation(server)(method, input)
      }
