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
package telekinesis

import scala.collection.immutable.Seq
import scala.collection.`+:`

import scala.caps

import scala.quoted.*

import anticipation.*
import fulminate.*
import gigantism.*
import gossamer.*
import prepositional.*
import spectacular.*
import urticose.*
import vacuous.*

object internal:
  def expand
    ( todo:   Seq[Expr[Any]],
      method: Optional[Expr[Http.Method]] = Unset,
      status: Optional[Expr[Http.Status]] = Unset,
      done:   List[Expr[Http.Header]]     = Nil )
    ( using Quotes )
  :   (Optional[Expr[Http.Method]], Optional[Expr[Http.Status]], Expr[Seq[Http.Header]]) =

    import quotes.reflect.*

    def unnamed[value: Type](value: Expr[value], tail: Seq[Expr[Any]]) =
      // A plain match rather than `getOrElse`: under capture checking the `B >: A` bound of
      // `getOrElse` is inferred against the capture-decorated summon result and fails.
      val summoned = Expr.summon[Directive of ? >: value] match
        case Some(directive) => directive
        case None =>
          val typeName = Type.of[value].show
          halt(m"the type $typeName does not uniquely identify a particular HTTP header")

      summoned
      . absolve
      . match
        case '{type keyType <: Label; $directive: (Directive { type Self = keyType })} =>
          TypeRepr.of[keyType].absolve match
            case ConstantType(StringConstant(key)) =>
              val header =
                '{Http.Header(${Expr(key)}.tt.uncamel.kebab, $directive.encode($value))}

              expand(tail, method, status, header :: done)

    todo.absolve match
      case '{$method2: Http.Method} +: tail =>
        if method.present then halt(m"the request method can only be specified once")
        expand(tail, method2, status, done)

      case '{("", $method2: Http.Method)} +: tail =>
        if method.present then halt(m"the request method can only be specified once")
        expand(tail, method2, status, done)

      case '{$status2: Http.Status} +: tail =>
        if status.present then halt(m"the HTTP status can only be specified once")
        expand(tail, method, status2, done)

      case '{("", $status2: Http.Status)} +: tail =>
        if status.present then halt(m"the HTTP status can only be specified once")
        expand(tail, method, status2, done)

      case '{("", $value: valueType)} +: tail =>
        unnamed[valueType](value, tail)

      case '{type keyType <: Label; ($key: keyType, $value: valueType)} +: tail =>
        val name: Text = List.of(key.value.get.tt.uncamel.stdlib.map(_.capitalize)).kebab

        val Directive = Expr.summon[keyType is Directive of valueType].getOrElse:
          val typeName = Type.of[valueType].show
          halt(m"the header $name cannot take a value of type $typeName")

        val header = '{Http.Header($key.tt.uncamel.kebab, $Directive.encode($value))}
        expand(tail, method, status, header :: done)

      case '{$value: valueType} +: tail =>
        unnamed[valueType](value, tail)

      case Seq() =>
        (method, status, Expr.ofList(done.stdlib.reverse))


  def submit[target: Type, payload: Type]
    ( submit:   Expr[Http.Submit[target]],
      headers:  Expr[Seq[(Label, Any)] | Seq[Any]],
      online:   Expr[Online],
      loggable: Expr[HttpEvent is Loggable],
      payload:  Expr[payload],
      postable: Expr[(payload is Postable)^],
      client:   Expr[HttpClient onto target] )
  :   Macro[Http.Response] =

    headers.absolve match
      case Varargs(exprs) =>
        val (method0, _, headers) = expand(exprs)

        val method = (method0: @unchecked) match
          case Unset                     => '{Http.Post}
          case method: Expr[Http.Method] => method

        ' {
            // No `Online` binding in the staged code: the permission gates summonability of
            // the enclosing extension method, and (with `Online` a capability) a local
            // capability-typed given inside the quote would trip the quote wall and, under
            // separation checking, hide the outer parameter from later statements.

            // Staging-boundary seal: quoted types must stay pure, so the (honestly
            // tracked) evidence is sealed on entry to the generated code. The
            // capability is fully applied within this request expression.
            given postable0: (payload is Postable) =
              caps.unsafe.unsafeAssumePure($postable)

            // Staging-boundary seal, like `postable0` below: quoted types must stay pure.
            given loggable0: HttpEvent is Loggable =
              caps.unsafe.unsafeAssumePure($loggable)
            val host: Host = $submit.host
            val path = $submit.originForm
            val contentType = Http.Header("content-type".tt, postable0.mediaType($payload).show)

            val request =
              Http.Request
                ( $method,
                  1.1,
                  host,
                  path,
                  contentType :: $headers.to(List),
                  () => postable0.stream($payload) )

            $client.request(request, $submit.target)
          }


  def fetch[target: Type]
    ( fetch:    Expr[Http.Fetch[target]],
      headers:  Expr[Seq[Any]],
      online:   Expr[Online],
      loggable: Expr[HttpEvent is Loggable],
      client:   Expr[HttpClient onto target] )
  :   Macro[Http.Response] =

    headers.absolve match
      case Varargs(exprs) =>
        val (method0, _, headers) = expand(exprs)

        val method = (method0: @unchecked) match
          case Unset                     => '{Http.Get}
          case method: Expr[Http.Method] => method

        ' {
            // No `Online` binding, as in `submit` above.
            // Staging-boundary seal, like `postable0` below: quoted types must stay pure.
            given loggable0: HttpEvent is Loggable =
              caps.unsafe.unsafeAssumePure($loggable)

            val path = $fetch.originForm

            val request =
              Http.Request
                ( $method, 1.1, $fetch.host, path, $headers.to(List), () => Http.emptyBody() )

            $client.request(request, $fetch.target)
          }


  def response(headers: Expr[Seq[Any]]): Macro[Http.Response.Protoresponse | Http.Response] =
    headers.absolve.match
      case Varargs(exprs) => List.of(exprs.toList).only:
        case '{$value: valueType} :: Nil =>
          Expr.summon[(? >: valueType) is Servable].map: servable => '{$servable.serve($value)}
          . optional

    . or:
        headers.absolve match
          case Varargs(exprs) =>
            val (_, status, headers2) = expand(exprs)

            val status2: Expr[Optional[Http.Status]] = (status: @unchecked) match
              case Unset                   => '{Unset}
              case expr: Expr[Http.Status] => expr

            '{Http.Response.Protoresponse($status2, List.of($headers2.toList))}
