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
package synesthesia

import scala.compiletime.*

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import hyperbole.*
import inimitable.*
import jacinta.*
import monotonous.*
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

  def prompt(context: Expr[StringContext], args: Expr[Seq[Any]], human: Boolean): Macro[Discourse] =
    import quotes.reflect.*
    val parts: List[String] = context.valueOrAbort.parts.to(List)
    val Varargs(arguments) = args

    val insertions = arguments.map:
      case '{$argument: argument} =>
        Expr.summon[argument is Showable] match
          case Some(showable) =>
            '{$showable.text($argument)}

          case None =>
            halt(m"""could not find a contextual `${TypeRepr.of[argument].show} is Showable`
                    instance""")

    val result = insertions.zip(parts.tail.map(Expr(_))).foldLeft(Expr(parts.head)):
      case (result, (insertion, part)) =>
        '{$result+$insertion+$part}

    val types = parts.map(StringConstant(_)).map(ConstantType(_).asType).reverse

    if human then '{Human($result.tt)} else '{Agent($result.tt)}


  def spec[interface: Type]: Macro[interface is McpSpecification] =
    import quotes.reflect.*
    val toolType = TypeRepr.of[tool].typeSymbol
    val promptType = TypeRepr.of[prompt].typeSymbol
    val aboutType = TypeRepr.of[about].typeSymbol
    val titleType = TypeRepr.of[title].typeSymbol
    val uiType = TypeRepr.of[ui].typeSymbol
    val resourceType = TypeRepr.of[resource].typeSymbol
    val interface = TypeRepr.of[interface]

    val toolMethods = interface.typeSymbol.declaredMethods.filter: method =>
      val allAnnotations = method.annotations ++ method.allOverriddenSymbols.flatMap(_.annotations)
      allAnnotations.exists(_.tpe.typeSymbol == toolType)

    val promptMethods = interface.typeSymbol.declaredMethods.filter: method =>
      val allAnnotations = method.annotations ++ method.allOverriddenSymbols.flatMap(_.annotations)
      allAnnotations.exists(_.tpe.typeSymbol == promptType)

    val resourceMethods = interface.typeSymbol.declaredMethods.filter: method =>
      val allAnnotations = method.annotations ++ method.allOverriddenSymbols.flatMap(_.annotations)
      allAnnotations.exists(_.tpe.typeSymbol == resourceType)

    val jsonErrors = Expr.summon[Tactic[JsonError]].getOrElse:
      halt(m"""could not find a contextual `Tactic[McpError]` instance""")

    // This has been written as a partial function because the more natural way of writing it,
    // by including `target` as a lambda variable, causes the compiler to emit bad bytecode.
    val toolInvocation: Expr[interface ~> ((Text, Json, McpClient) => Json)] =
      ' {
          {
            case target: `interface` =>
              (method: Text, input: Json, client: McpClient) =>
                import dynamicJsonAccess.enabled
                given Tactic[JsonError] = $jsonErrors

                val request = input.as[Map[Text, Json]]

                $ {
                    val cases = toolMethods.map: method =>
                      val result: TypeRepr = method.info.absolve match
                        case MethodType(_, _, MethodType(_, _, result)) => result
                        case MethodType(_, _, result) => result

                      val params = method.paramSymss.head.map: param =>
                        param.info.asType.absolve match
                          case '[param] => Expr.summon[param is Decodable in Json] match
                            case Some(decodable) =>
                              ' {
                                  given param is Decodable in Json = $decodable
                                  request(${Expr(param.name)}).as[param]
                                }
                              . asTerm
                            case None =>
                              halt:
                                m"""
                                  could not find a contextual `${TypeRepr.of[param].show} is
                                  Decodable in Json` instance for the parameter ${param.name} of
                                  ${method.name}
                                """


                      val application = method.paramSymss.length match
                        case 1 => Apply(Select('target.asTerm, method), params)

                        case 2 =>
                          Apply(Apply(Select('target.asTerm, method), params), List('client.asTerm))

                        case _ =>
                          halt:
                            m"""
                              MCP tool definitions should have exactly one explicit parameter block
                              and optionally one contextual parameter block
                            """

                      val rhs = result.asType.absolve match
                        case '[result] => Expr.summon[result is Encodable in Json] match
                          case Some(encoder) =>
                            ' {
                                import jsonPrinters.indented
                                val output =
                                  Map
                                    ( "result".tt
                                      -> $encoder.encode(${application.asExprOf[result]}) )
                                output.json
                              }

                          case None =>
                            halt:
                              m"""
                                could not find a contextual `${TypeRepr.of[result].show} is
                                Encodable in Json` instance for the return type of ${method.name}
                              """

                      CaseDef(Literal(StringConstant(method.name)), None, rhs.asTerm)

                    val wildcard = Expr.summon[Tactic[McpError]] match
                      case Some(tactic) =>
                        CaseDef(Wildcard(), None, '{abort(McpError())(using $tactic)}.asTerm)

                      case None =>
                        halt(m"""could not find a contextual `Tactic[McpError]` instance""")

                    Match('method.asTerm, cases :+ wildcard).asExprOf[Json]
                  }
            }
          }

    // This has been written as a partial function because the more natural way of writing it,
    // by including `target` as a lambda variable, causes the compiler to emit bad bytecode.
    val promptInvocation
    : Expr[interface ~> ((Text, Map[Text, Text], McpClient) => List[Discourse])] =
        ' {
            {
              case target: `interface` =>
                (method: Text, input: Map[Text, Text], client: McpClient) =>
                  import dynamicJsonAccess.enabled
                  given Tactic[JsonError] = $jsonErrors

                  $ {
                      val cases = promptMethods.map: method =>
                        val result: TypeRepr = method.info.absolve match
                          case MethodType(_, _, MethodType(_, _, result)) => result
                          case MethodType(_, _, result)                   => result
                          case result                                     => result

                        val params = method.paramSymss.headOption.map: paramList =>
                          paramList.map: param =>
                            param.info.asType.absolve match
                              case '[param] => Expr.summon[param is Decodable in Text] match
                                case Some(decodable) =>
                                  ' {
                                      given param is Decodable in Text = $decodable
                                      val key = ${Expr(param.name)}.tt
                                      if input.has(key) then input(key).decode[param]
                                      else provide[Tactic[McpError]](abort(McpError()))
                                    }
                                  . asTerm

                                case None => halt:
                                  m"""
                                    could not find a contextual `${TypeRepr.of[param].show} is
                                    Decodable in Text` instance for the parameter ${param.name} of
                                    ${method.name}
                                  """

                        val application = method.paramSymss.length match
                          case 0 => Select('target.asTerm, method)
                          case 1 => Apply(Select('target.asTerm, method), params.get)

                          case 2 =>
                            Apply
                              ( Apply(Select('target.asTerm, method), params.get),
                                List('client.asTerm) )

                          case _ => halt:
                            m"""
                              MCP prompt definitions should have exactly one explicit parameter
                              block
                            """

                        result.asType match
                          case '[List[Discourse]] =>
                          case '[result] => halt:
                            m"""
                              the MCP prompt method returns ${TypeRepr.of[result].show}, but it must
                              return `List[Discourse]`
                            """

                        CaseDef(Literal(StringConstant(method.name)), None, application)

                      val wildcard =
                        val rhs = '{provide[Tactic[McpError]](abort(McpError()))}
                        CaseDef(Wildcard(), None, rhs.asTerm)

                      Match('method.asTerm, cases :+ wildcard).asExprOf[List[Discourse]]
                    }
              }
            }

    val resourceInvocation: Expr[interface ~> (Text => Mcp.Contents)] =
      ' {
          {
            case target: `interface` =>
              (uri: Text) =>
                $ {
                    val cases = resourceMethods.map: method =>
                      val allAnnotations =
                        method.annotations ++ method.allOverriddenSymbols.flatMap(_.annotations)

                      method.info.widen.asType.absolve match
                        case '[result] =>
                          val result: TypeRepr = method.info.widen
                          val value = Select('target.asTerm, method).asExprOf[result]

                          val uri: Expr[Text] =
                            ' {
                                $ {
                                    allAnnotations.find(_.tpe.typeSymbol == resourceType).get
                                    . asExprOf[resource]
                                  }
                                . uri
                              }

                          val rhs = Expr.summon[result is Streamable by Text] match
                            case Some(streamable) =>
                              ' {
                                  given result is Streamable by Text = $streamable

                                  Mcp.Contents:
                                    Mcp.TextResourceContents
                                      ( $uri,
                                        mimeType = t"text/html;profile=mcp-app", // FIXME
                                        text = $value.read[Text])
                                }
                            case None => Expr.summon[result is Streamable by Data] match
                              case Some(streamable) =>
                                ' {
                                    import alphabets.base64.standard
                                    given result is Streamable by Data = $streamable
                                    Mcp.Contents:
                                      Mcp.BlobResourceContents
                                        ( $uri, Unset, blob = $value.read[Data].serialize[Base64] )
                                  }
                              case None => halt:
                                m"""
                                 there was no contextual `${TypeRepr.of[result].show} is Streamable`
                                 instance for the return type of ${method.name}
                                 """

                          val application = method.paramSymss.length match
                            case 0 => Select('target.asTerm, method)

                            case _ => halt:
                              m"""
                               MCP resource definitions should have exactly one explicit parameter
                               block and optionally one contextual parameter block
                               """

                          (uri, rhs)

                    cases.foldLeft('{provide[Tactic[McpError]](abort(McpError()))}):
                      case (acc, (pattern, rhs)) => '{if uri == $pattern then $rhs else $acc}
                  }
            }
          }

    val toolEntries = toolMethods.map: method =>
      val allAnnotations = method.annotations ++ method.allOverriddenSymbols.flatMap(_.annotations)

      val about: Expr[Optional[Text]] =
        allAnnotations.find(_.tpe.typeSymbol == aboutType).map: annotation =>
          '{${annotation.asExprOf[about]}.text}
        . getOrElse('{Unset})

      val title: Expr[Optional[Text]] =
        allAnnotations.find(_.tpe.typeSymbol == titleType).map: annotation =>
          '{${annotation.asExprOf[title]}.text}
        . getOrElse('{Unset})

      val ui: Expr[Optional[Text]] =
        allAnnotations.find(_.tpe.typeSymbol == uiType).map: annotation =>
          '{${annotation.asExprOf[ui]}.uri}
        . getOrElse('{Unset})

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
        case MethodType(_, _, MethodType(_, _, result)) => result
        case MethodType(_, _, result) => result

      result.asType.absolve match
        case '[result] => Expr.summon[result is Schematic in JsonSchema] match
          case Some(schematic) =>
            ' {
                val inputSchema =
                  JsonSchema.Object
                    ( properties = $properties, required = ${Expr.ofList(paramNames)} )

                val outputSchema =
                  JsonSchema.Object
                    ( properties = Map(t"result" -> $schematic.schema()),
                      required   = List(t"result") )

                val uiJson: Optional[Json] = $ui.let: resource =>
                  val ui =
                    Map
                      ( t"visibility"  -> List(t"model", t"app").json,
                        t"resourceUri" -> resource.json )

                  Map(t"ui" -> ui.json).json

                Mcp.Tool
                  ( name         = ${Expr(method.name)},
                    title        = $title,
                    description  = $about,
                    inputSchema  = inputSchema,
                    outputSchema = outputSchema,
                    _meta        = uiJson )
              }
          case None => halt:
            m"""
             there was no contextual `${TypeRepr.of[result].show} is Schematic in JsonSchema`
             instance for the return type of ${method.name}
             """

    val promptEntries = promptMethods.map: method =>
      val allAnnotations = method.annotations ++ method.allOverriddenSymbols.flatMap(_.annotations)

      val about: Expr[Optional[Text]] =
        allAnnotations.find(_.tpe.typeSymbol == aboutType).map: annotation =>
          '{${annotation.asExprOf[about]}.text}
        . getOrElse('{Unset})

      val title: Expr[Optional[Text]] =
        allAnnotations.find(_.tpe.typeSymbol == titleType).map: annotation =>
          '{${annotation.asExprOf[title]}.text}
        . getOrElse('{Unset})

      val params =
        method.paramSymss.headOption.map: paramList =>
          paramList.map: param =>
            val annotations = param.annotations

            val title: Expr[Optional[Text]] =
              annotations.find(_.tpe.typeSymbol == titleType).map: annotation =>
                '{${annotation.asExprOf[title]}.text}
              . getOrElse('{Unset})

            val about: Expr[Optional[Text]] =
              allAnnotations.find(_.tpe.typeSymbol == aboutType).map: annotation =>
                '{${annotation.asExprOf[about]}.text}
              . getOrElse('{Unset})

            '{Mcp.PromptArgument(${Expr(param.name.tt)}, $title, $about)}
        . getOrElse(Nil)

      ' {
          Mcp.Prompt
            ( name         = ${Expr(method.name.tt)},
              title        = $title,
              description  = $about,
              arguments    = ${if params.isEmpty then 'Unset else Expr.ofList(params)})
        }

    val resourceEntries = resourceMethods.map: method =>
      val allAnnotations = method.annotations ++ method.allOverriddenSymbols.flatMap(_.annotations)
      allAnnotations.exists(_.tpe.typeSymbol == resourceType)

      val uri: Expr[Text] =
        '{${allAnnotations.find(_.tpe.typeSymbol == resourceType).get.asExprOf[resource]}.uri}

      val about: Expr[Optional[Text]] =
        allAnnotations.find(_.tpe.typeSymbol == aboutType).map: annotation =>
          '{${annotation.asExprOf[about]}.text}
        . getOrElse('{Unset})

      val title: Expr[Optional[Text]] =
        allAnnotations.find(_.tpe.typeSymbol == titleType).map: annotation =>
          '{${annotation.asExprOf[title]}.text}
        . getOrElse('{Unset})

      if method.paramSymss.length > 0 then halt(m"MCP resource methods cannot have any parameters")

      val result: TypeRepr = method.info.widen

      result.asType.absolve match
        case '[result] =>
          Expr.summon[result is Streamable by Text] match
            case Some(streamable) =>
              ' {
                  Mcp.Resource
                    ( name        = ${Expr(method.name)},
                      uri         = $uri,
                      title       = $title,
                      description = $about )
                }

            case None => Expr.summon[result is Streamable by Data] match
              case Some(streamable) =>
                ' {
                    Mcp.Resource
                      ( name        = ${Expr(method.name)},
                        uri         = $uri,
                        title       = $title,
                        description = $about )
                  }

              case None => halt:
                m"""
                 there was no contextual `${TypeRepr.of[result].show} is Streamable` instance for
                 the return type of ${method.name}
                 """

    ' {
        new McpSpecification:
          type Self = interface
          def tools(): List[Mcp.Tool] = ${Expr.ofList(toolEntries)}
          def resources(): List[Mcp.Resource] = ${Expr.ofList(resourceEntries)}
          def prompts(): List[Mcp.Prompt] = ${Expr.ofList(promptEntries)}

          def invokeTool(server: interface, client: McpClient, method: Text, input: Json): Json =
            $toolInvocation(server)(method, input, client)

          def invokePrompt
            ( server: interface, client: McpClient, method: Text, input: Map[Text, Text] )
          : List[Discourse] =

              $promptInvocation(server)(method, input, client)

          def invokeResource(server: interface, method: Text): Mcp.Contents =
            $resourceInvocation(server)(method)
      }
