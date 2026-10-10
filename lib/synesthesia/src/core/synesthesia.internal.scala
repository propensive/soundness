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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import scala.annotation
import scala.annotation.*
import scala.collection.immutable.Seq
import scala.quoted.*

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gigantism.*
import gossamer.*
import jacinta.*
import monotonous.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import zephyrine.*

object internal:
  def prompt(context: Expr[StringContext], arguments0: Expr[Seq[Any]], human: Boolean)
  :   Macro[Discourse] =

    import quotes.reflect.*

    val parts: List[String] = context.valueOrAbort.parts.to(List)
    val arguments = arguments0.absolve match case Lifts.Varargs(arguments) => arguments

    // Hoisted from the `map` below: a quote (with its implicit summons) inside a combinator
    // lambda in a macro risks the `wildApprox` crash.
    def encode(value: Expr[Any]): Expr[Text] =
      value.absolve match
        case '{$argument: argument} =>
          Expr.summon[argument is Showable] match
            case Some(showable) =>
              '{$showable.text($argument)}

            case None =>
              halt:
                m"could not find a contextual `${TypeRepr.of[argument].show} is Showable` instance"

    val insertions = arguments.map(encode)

    def concatenate(insertions: List[Expr[Text]], parts: List[String], done: Expr[String])
    :   Expr[String] =

      insertions match
        case Nil => done

        case insertion :: insertions2 => parts.absolve match
          case part :: parts2 =>
            concatenate(insertions2, parts2, '{$done+$insertion+${Expr(part)}})

    val result = parts.absolve match
      case head :: tail => concatenate(insertions, tail, Expr(head))

    val types = parts.map(StringConstant(_)).map(ConstantType(_).asType).reverse

    if human then '{Human($result.tt)} else '{Agent($result.tt)}


  def spec[interface: Type]: Macro[interface is Mcp.Specification] =
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

    val jsonErrors = Expr.summon[Tactic[Json.Error]].getOrElse:
      halt(m"could not find a contextual `Tactic[Json.Error]` instance")

    val mcpErrors = Expr.summon[Tactic[Mcp.Error]].getOrElse:
      halt(m"could not find a contextual `Tactic[Mcp.Error]` instance")

    // A parameter is required in the input schema unless its type admits `Unset` or it has a
    // default argument; a missing one is then `Unset` or the default, rather than an error.
    def admitsUnset(param: Symbol): Boolean = TypeRepr.of[Unset.type] <:< param.info

    def defaultGetter(method: Symbol, index: Int): Option[Symbol] =
      if !method.paramSymss.head(index).flags.is(Flags.HasDefault) then None
      else interface.typeSymbol.methodMember(method.name+"$default$"+(index + 1)).headOption

    // The URIs that `@ui` annotations on tools name: a resource at one of them is the app's
    // user interface, and is served with the MCP-app profile unless its MIME type is given.
    val uiUris: scala.collection.immutable.List[Expr[Text]] = toolMethods.flatMap: method =>
      val allAnnotations = method.annotations ++ method.allOverriddenSymbols.flatMap(_.annotations)

      allAnnotations.filter(_.tpe.typeSymbol == uiType).map: annotation =>
        '{${annotation.asExprOf[ui]}.uri}

    // This has been written as a partial function because the more natural way of writing it,
    // by including `target` as a lambda variable, causes the compiler to emit bad bytecode.
    val toolInvocation: Expr[interface ~> ((Text, Json, Mcp.Client) => Json)] =
      ' {
          {
            case target: `interface` =>
              (method: Text, input: Json, client: Mcp.Client) =>
                given Tactic[Json.Error] = $jsonErrors

                val request = input.as[Map[Text, Json]]

                $ {
                    val cases = toolMethods.map: method =>
                      val result: TypeRepr = method.info.absolve match
                        case MethodType(_, _, MethodType(_, _, result)) => result
                        case MethodType(_, _, result)                   => result

                      val params = method.paramSymss.head.zipWithIndex.map: (param, index) =>
                        param.info.asType.absolve match
                          case '[param] => Expr.summon[param is Json.Decodable] match
                            case Some(decodable) =>
                              val fallback: Expr[param] = defaultGetter(method, index) match
                                case Some(getter) => Select('target.asTerm, getter).asExprOf[param]

                                case None =>
                                  if admitsUnset(param) then '{Unset}.asExprOf[param] else
                                    ' {
                                        abort(Mcp.Error(Mcp.Error.Reason.MissingParameter))
                                          ( using $mcpErrors )
                                      }

                              ' {
                                  request.at(${Expr(param.name)}.tt) match
                                    case Unset      => $fallback
                                    case json: Json => $decodable.decoded(json)
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
                          Apply
                            ( Apply(Select('target.asTerm, method), params),
                              scala.collection.immutable.List('client.asTerm) )

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
                                val output =
                                  Map
                                    ( t"result" ->
                                      $encoder.encode(${application.asExprOf[result]}) )

                                output.in[Json]
                              }

                          case None =>
                            halt:
                              m"""
                                could not find a contextual `${TypeRepr.of[result].show} is
                                Encodable in Json` instance for the return type of ${method.name}
                              """

                      CaseDef(Literal(StringConstant(method.name)), None, rhs.asTerm)

                    val wildcard =
                      val rhs =
                        '{abort(Mcp.Error(Mcp.Error.Reason.UnknownMethod))(using $mcpErrors)}

                      CaseDef(Wildcard(), None, rhs.asTerm)

                    Match('method.asTerm, cases :+ wildcard).asExprOf[Json]
                  }
            }
        }

    // This has been written as a partial function because the more natural way of writing it,
    // by including `target` as a lambda variable, causes the compiler to emit bad bytecode.
    val promptInvocation
    :   Expr[interface ~> ((Text, Map[Text, Text], Mcp.Client) => List[Discourse])] =

      ' {
          {
            case target: `interface` =>
              (method: Text, input: Map[Text, Text], client: Mcp.Client) =>
                given Tactic[Json.Error] = $jsonErrors

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
                                    input.at(${Expr(param.name)}.tt) match
                                      case text: Text => $decodable.decoded(text)

                                      case Unset =>
                                        abort(Mcp.Error(Mcp.Error.Reason.MissingParameter))
                                          ( using $mcpErrors )
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
                              scala.collection.immutable.List('client.asTerm) )

                        case _ => halt:
                          m"MCP prompt definitions should have exactly one explicit parameter block"

                      result.asType.absolve match
                        case '[List[Discourse]] =>

                        case '[result] => halt:
                          m"""
                            the MCP prompt method returns ${TypeRepr.of[result].show}, but it must
                            return `List[Discourse]`
                          """

                      CaseDef(Literal(StringConstant(method.name)), None, application)

                    val wildcard =
                      val rhs =
                        '{abort(Mcp.Error(Mcp.Error.Reason.UnknownMethod))(using $mcpErrors)}

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

                          val annotation =
                            allAnnotations.find(_.tpe.typeSymbol == resourceType).get
                            . asExprOf[resource]

                          val uri: Expr[Text] = '{$annotation.uri}

                          // The resource's own MIME type, or the MCP-app profile for a tool's
                          // user interface, or the plainest type for its form.
                          def mimeType(plain: Expr[Text]): Expr[Text] =
                            ' {
                                $annotation.mimeType match
                                  case mimeType: Text => mimeType

                                  case Unset =>
                                    if List(${Varargs(uiUris)}*).exists(_ == $uri)
                                    then t"text/html;profile=mcp-app"
                                    else $plain
                              }

                          // The aggregation is spelled out rather than bound as a given for
                          // `read`: the streamable captures the expansion site's tactic, which a
                          // pure given binding would reject under capture checking.
                          val rhs = Expr.summon[result is Streamable by Text over Credit] match
                            case Some(streamable) =>
                              val aggregable = Expr.summon[Text is Aggregable by Text].getOrElse:
                                halt(m"could not find a contextual `Text is Aggregable by Text`")

                              ' {
                                  Mcp.Contents:
                                    Mcp.TextResourceContents
                                      ( $uri,
                                        mimeType = ${mimeType('{t"text/plain"})},
                                        text     = $aggregable.accept($streamable.stream($value)) )
                                }

                            case None => Expr.summon[result is Streamable by Data over Credit] match
                              case Some(streamable) =>
                                val aggregable = Expr.summon[Data is Aggregable by Data].getOrElse:
                                  halt(m"could not find a contextual `Data is Aggregable by Data`")

                                ' {
                                    import alphabets.base64Standard

                                    Mcp.Contents:
                                      Mcp.BlobResourceContents
                                        ( $uri,
                                          mimeType = ${mimeType('{t"application/octet-stream"})},
                                          blob     =
                                            $aggregable.accept($streamable.stream($value))
                                            . serialize[Base64] )
                                  }

                              case None => halt:
                                m"""
                                  there was no contextual ${TypeRepr.of[result is Streamable].show}
                                  instance for the return type of ${method.name}
                                """

                          if method.paramSymss.nonEmpty
                          then halt(m"MCP resource methods cannot have any parameters")

                          (uri, rhs)

                    val initial =
                      '{provide[Tactic[Mcp.Error]](abort(Mcp.Error(Mcp.Error.Reason.UnknownResource)))}

                    cases.foldLeft(initial):
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

      // The tool's `_meta`, naming the resource that is its user interface, if it has one. The
      // list literal is ascribed: its own type is populated, which no JSON encoder covers.
      val uiJson: Expr[Optional[Json]] =
        allAnnotations.find(_.tpe.typeSymbol == uiType).map: annotation =>
          ' {
              val ui =
                Map
                  ( t"visibility"  -> (List(t"model", t"app"): List[Text]).in[Json],
                    t"resourceUri" -> ${annotation.asExprOf[ui]}.uri.in[Json] )

              Map(t"ui" -> ui.in[Json]).in[Json]
            }

        . getOrElse('{Unset})

      val required = method.paramSymss.head.zipWithIndex.collect:
        case (param, index) if !admitsUnset(param) && defaultGetter(method, index).isEmpty =>
          '{${Expr(param.name)}.tt}

      val params = method.paramSymss.head.map: param =>
        param.info.asType.absolve match
          case '[param] => Expr.summon[param is Schematic over JsonSchema] match
            case Some(schematic) =>
              val schema: Expr[JsonSchema] =
                param.annotations.find(_.tpe.typeSymbol == aboutType) match
                  case Some(annotation) =>
                    '{$schematic.schema().`description_=`(${annotation.asExprOf[about]}.text)}

                  case None =>
                    '{$schematic.schema()}

              '{(${Expr(param.name)}.tt, $schema)}

            case None =>
              halt(m"There was no JSON schema for ${param.name}")

      val properties = '{Map.from(${Expr.ofList(params)})}

      val result: TypeRepr = method.info.absolve match
        case MethodType(_, _, MethodType(_, _, result)) => result
        case MethodType(_, _, result)                   => result

      result.asType.absolve match
        case '[result] => Expr.summon[result is Schematic over JsonSchema] match
          case Some(schematic) =>
            ' {
                val inputSchema =
                  JsonSchema.Object
                    ( properties = $properties, required = List(${Varargs(required)}*) )

                val outputSchema =
                  JsonSchema.Object
                    ( properties = Map(t"result" -> $schematic.schema()),
                      required   = List(t"result") )

                Mcp.Tool
                  ( name         = ${Expr(method.name)},
                    title        = $title,
                    description  = $about,
                    inputSchema  = inputSchema,
                    outputSchema = outputSchema,
                    _meta        = $uiJson )
              }

          case None => halt:
            m"""
              there was no contextual ${TypeRepr.of[result is Schematic over JsonSchema].show}
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
              annotations.find(_.tpe.typeSymbol == aboutType).map: annotation =>
                '{${annotation.asExprOf[about]}.text}

              . getOrElse('{Unset})

            '{Mcp.PromptArgument(${Expr(param.name)}.tt, $title, $about)}

        . getOrElse(scala.collection.immutable.Nil)

      ' {
          Mcp.Prompt
            ( name         = ${Expr(method.name)}.tt,
              title        = $title,
              description  = $about,
              arguments    = ${if params.isEmpty then 'Unset else '{List(${Varargs(params)}*)}} )
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

      val annotation = allAnnotations.find(_.tpe.typeSymbol == resourceType).get.asExprOf[resource]

      def entry(plain: Expr[Text]): Expr[Mcp.Resource] =
        ' {
            Mcp.Resource
              ( name        = ${Expr(method.name)},
                uri         = $uri,
                title       = $title,
                description = $about,
                mimeType    =
                  $annotation.mimeType match
                    case mimeType: Text => mimeType

                    case Unset =>
                      if List(${Varargs(uiUris)}*).exists(_ == $uri)
                      then t"text/html;profile=mcp-app"
                      else $plain )
          }

      val result: TypeRepr = method.info.widen

      result.asType.absolve match
        case '[result] =>
          Expr.summon[result is Streamable by Text] match
            case Some(streamable) => entry('{t"text/plain"})

            case None => Expr.summon[result is Streamable by Data] match
              case Some(streamable) => entry('{t"application/octet-stream"})

              case None => halt:
                m"""
                  there was no contextual ${TypeRepr.of[result is Streamable].show} instance for the
                  return type of ${method.name}
                """

    ' {
        new Mcp.Specification:
          type Self = interface
          def tools(): List[Mcp.Tool] = List(${Varargs(toolEntries)}*)
          def resources(): List[Mcp.Resource] = List(${Varargs(resourceEntries)}*)
          def prompts(): List[Mcp.Prompt] = List(${Varargs(promptEntries)}*)

          def invokeTool(server: interface, client: Mcp.Client, method: Text, input: Json): Json =
            $toolInvocation(server)(method, input, client)

          def invokePrompt
            ( server: interface, client: Mcp.Client, method: Text, input: Map[Text, Text] )
          :   List[Discourse] =

            $promptInvocation(server)(method, input, client)

          def invokeResource(server: interface, method: Text): Mcp.Contents =
            $resourceInvocation(server)(method)
      }
