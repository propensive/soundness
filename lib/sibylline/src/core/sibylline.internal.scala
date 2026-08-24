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
package sibylline

import scala.quoted.*

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import jacinta.*
import prepositional.*
import rudiments.*
import vacuous.*

object internal:
  def toolkit[tools: Type](target: Expr[tools])(using Quotes): Expr[Toolkit] =
    import quotes.reflect.*

    val toolType = TypeRepr.of[ability].typeSymbol
    val aboutType = TypeRepr.of[synesthesia.about].typeSymbol

    val methods = TypeRepr.of[tools].typeSymbol.declaredMethods.filter: method =>
      val all = method.annotations ++ method.allOverriddenSymbols.flatMap(_.annotations)
      all.exists(_.tpe.typeSymbol == toolType)

    if methods.isEmpty then halt(m"no @ability-annotated methods were found on the target")

    // The wire specifications: one `Llm.Tool` per method, its parameters' schemas summoned as
    // `Schematic over JsonSchema` instances.
    val entries = methods.map: method =>
      val all = method.annotations ++ method.allOverriddenSymbols.flatMap(_.annotations)

      val description: Expr[Text] =
        all.find(_.tpe.typeSymbol == aboutType).map: annotation =>
          '{${annotation.asExprOf[synesthesia.about]}.text}

        . getOrElse('{t""})

      val explicit = method.paramSymss.headOption.getOrElse(scala.Nil)
      val paramNames = explicit.map: param => '{${Expr(param.name)}.tt}

      val params = explicit.map: param =>
        param.info.asType.absolve match
          case '[param] => Expr.summon[param is Schematic over JsonSchema] match
            case Some(schematic) => '{(${Expr(param.name)}.tt, $schematic.schema())}

            case None => halt:
              m"there was no JSON schema for the parameter ${param.name} of ${method.name}"

      ' {
          val required: List[Text] = List.from(${Expr.ofList(paramNames)})

          Llm.Tool
            ( ${Expr(method.name)}.tt,
              $description,
              JsonSchema.Object
                ( properties = Map.from(${Expr.ofList(params)}),
                  required   = required ) )
        }

    // The dispatcher: a match on the tool's name, decoding each argument from the model's JSON
    // (a missing or malformed argument raises `Invalid`, which the tool loop reports back to
    // the model as an `is_error` result), applying the method, and encoding its return value.
    // A second, contextual parameter block is summoned here, at the expansion site.
    def dispatch
      ( name:    Expr[Text],
        request: Expr[Map[Text, Json]],
        tactic:  Expr[Tactic[Llm.Error]] )
      ( using Quotes )
    :   Expr[Json] =

      val cases = methods.map: method =>
        val explicit = method.paramSymss.headOption.getOrElse(scala.Nil)

        val params = explicit.map: param =>
          param.info.asType.absolve match
            case '[param] => Expr.summon[param is Json.Decodable] match
              case Some(decodable) =>
                ' {
                    given paramDecodable: (param is Json.Decodable) = $decodable

                    val value: param =
                      $request.at(${Expr(param.name)}.tt)
                      . let: json => safely(json.as[param])
                      . or:
                          abort
                            ( Llm.Error
                                ( Llm.Error.Reason.Invalid,
                                  t"the argument ${${Expr(param.name)}.tt} was malformed" ) )
                            ( using $tactic )

                    value
                  }

                . asTerm

              case None => halt:
                m"""
                  could not find a contextual `${TypeRepr.of[param].show} is Decodable in Json`
                  instance for the parameter ${param.name} of ${method.name}
                """

        val contextual = method.paramSymss.lift(1).map: paramList =>
          paramList.map: param =>
            param.info.asType.absolve match
              case '[param] => Expr.summon[param] match
                case Some(value) => value.asTerm

                case None => halt:
                  m"""
                    could not summon the contextual parameter ${param.name} of ${method.name}
                    at the Toolkit construction site
                  """

        val application = method.paramSymss.length match
          case 0 => Select(target.asTerm, method)
          case 1 => Apply(Select(target.asTerm, method), params)
          case 2 => Apply(Apply(Select(target.asTerm, method), params), contextual.get)

          case _ => halt:
            m"""
              tool definitions should have exactly one explicit parameter block and optionally
              one contextual parameter block
            """

        val result: TypeRepr = method.info.absolve match
          case MethodType(_, _, MethodType(_, _, result)) => result
          case MethodType(_, _, result)                   => result
          case result                                     => result

        val rhs = result.asType.absolve match
          case '[result] => Expr.summon[result is Encodable in Json] match
            case Some(encoder) => '{$encoder.encode(${application.asExprOf[result]})}

            case None => halt:
              m"""
                could not find a contextual `${TypeRepr.of[result].show} is Encodable in Json`
                instance for the return type of ${method.name}
              """

        CaseDef(Literal(StringConstant(method.name)), None, rhs.asTerm)

      val wildcard =
        val rhs =
          ' {
              abort(Llm.Error(Llm.Error.Reason.Invalid, t"the tool ${$name} is not defined"))
                ( using $tactic )
            }

        CaseDef(Wildcard(), None, rhs.asTerm)

      Match(name.asTerm, cases :+ wildcard).asExprOf[Json]

    ' {
        new Toolkit:
          def specs: List[Llm.Tool] = List.from(${Expr.ofList(entries)})

          def invoke(name: Text, arguments: Json): Json raises Llm.Error =
            val tactic: Tactic[Llm.Error] = summon[Tactic[Llm.Error]]
            val request: Map[Text, Json] = safely(arguments.as[Map[Text, Json]]).or(Map())
            ${dispatch('name, 'request, 'tactic)}
      }
