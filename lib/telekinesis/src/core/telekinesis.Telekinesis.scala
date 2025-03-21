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
┃    Soundness, version 0.27.0.                                                                    ┃
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

import scala.quoted.*

import anticipation.*
import fulminate.*
import gossamer.*
import nettlesome.*
import prepositional.*
import proscenium.*
import spectacular.*
import vacuous.*

object Telekinesis:
  def expand
     (todo:    Seq[Expr[Any]],
      method:  Optional[Expr[Http.Method]]  = Unset,
      status:  Optional[Expr[Http.Status]]  = Unset,
      done:    List[Expr[Http.Header]]      = Nil)
     (using Quotes)
  :     (Optional[Expr[Http.Method]], Optional[Expr[Http.Status]], Expr[Seq[Http.Header]]) =
    import quotes.reflect.*

    def unnamed[ValueType: Type](value: Expr[ValueType], tail: Seq[Expr[Any]]) =
      Expr.summon[Prefixable of ? >: ValueType].getOrElse:
        val typeName = TypeRepr.of[ValueType].show
        halt(m"the type $typeName does not uniquely identify a particular HTTP header")

      . absolve
      . match
          case '{ type keyType <: Label; $prefixable: (Prefixable { type Self = keyType }) } =>
            TypeRepr.of[keyType].absolve match
              case ConstantType(StringConstant(key)) =>
                val header =
                  '{Http.Header(${Expr(key)}.tt.uncamel.kebab, $prefixable.encode($value))}

                expand(tail, method, status, header :: done)

    todo.absolve match
      case '{ $method2: Http.Method } +: tail =>
        if method.present then halt(m"the request method can only be specified once")
        expand(tail, method2, status, done)

      case '{ ("", $method2: Http.Method) } +: tail =>
        if method.present then halt(m"the request method can only be specified once")
        expand(tail, method2, status, done)

      case '{ $status2: Http.Status } +: tail =>
        if status.present then halt(m"the HTTP status can only be specified once")
        expand(tail, method, status2, done)

      case '{ ("", $status2: Http.Status) } +: tail =>
        if status.present then halt(m"the HTTP status can only be specified once")
        expand(tail, method, status2, done)

      case '{ ("", $value: valueType) } +: tail =>
        unnamed[valueType](value, tail)

      case '{ type keyType <: Label; ($key: keyType, $value: valueType) } +: tail =>
        val name: Text = key.value.get.tt.uncamel.map(_.capitalize).kebab

        val Prefixable = Expr.summon[keyType is Prefixable of valueType].getOrElse:
          val typeName = TypeRepr.of[valueType].show
          halt(m"the header $name cannot take a value of type $typeName")

        val header = '{Http.Header($key.tt.uncamel.kebab, $Prefixable.encode($value))}
        expand(tail, method, status, header :: done)

      case '{ $value: valueType } +: tail =>
        unnamed[valueType](value, tail)

      case Seq() =>
        (method, status, Expr.ofList(done.reverse))

  def submit[TargetType: Type, PayloadType: Type]
     (submit:   Expr[Http.Submit[TargetType]],
      headers:  Expr[Seq[(Label, Any)] | Seq[Any]],
      online:   Expr[Online],
      loggable: Expr[HttpEvent is Loggable],
      payload:  Expr[PayloadType],
      postable: Expr[PayloadType is Postable],
      client:   Expr[HttpClient onto TargetType])
     (using Quotes)
  :     Expr[Http.Response] =

    headers.absolve match
      case Varargs(exprs) =>
        val (method0, _, headers) = expand(exprs)

        val method = method0 match
          case Unset                    => '{Http.Post}
          case method: Expr[Http.Method] => method

        '{  given Online = $online
            given PayloadType is Postable = $postable
            given HttpEvent is Loggable = $loggable
            val host: Hostname = $submit.host
            val body = $postable.stream($payload)
            val path = $submit.originForm
            val contentType = Http.Header("content-type".tt, $postable.medium($payload).show)

            val request =
              Http.Request($method, 1.1, host, path, contentType :: $headers.to(List), body)

            $client.request(request, $submit.target)  }

  def fetch[TargetType: Type]
     (fetch:    Expr[Http.Fetch[TargetType]],
      headers:  Expr[Seq[Any]],
      online:   Expr[Online],
      loggable: Expr[HttpEvent is Loggable],
      client:   Expr[HttpClient onto TargetType])
     (using Quotes)
  :     Expr[Http.Response] =

    headers.absolve match
      case Varargs(exprs) =>
        val (method0, _, headers) = expand(exprs)

        val method = method0 match
          case Unset                    => '{Http.Get}
          case method: Expr[Http.Method] => method

        '{  given Online = $online
            given HttpEvent is Loggable = $loggable

            val path = $fetch.originForm
            val request = Http.Request($method, 1.1, $fetch.host, path, $headers.to(List), Stream())

            $client.request(request, $fetch.target)  }

  def response(headers: Expr[Seq[Any]])(using Quotes)
  :     Expr[Http.Response.Prototype | Http.Response] =

    headers.absolve.match
      case Varargs(exprs) => exprs.to(List).only:
        case '{ $value: valueType } :: Nil =>
          Expr.summon[(? >: valueType) is Servable].map { servable => '{$servable.serve($value)} }
          . optional

    . or:
        headers.absolve match
          case Varargs(exprs) =>
            val (_, status, headers2) = expand(exprs.to(List))

            val status2: Expr[Optional[Http.Status]] = status match
              case Unset                   => '{Unset}
              case expr: Expr[Http.Status] => expr

            '{Http.Response.Prototype($status2, $headers2)}

  def query(values: Expr[Seq[(Label, Any)]])(using Quotes): Expr[Query] =

    def recur(exprs: List[Expr[(Label, Any)]], done: List[Expr[List[(Text, Text)]]] = Nil)
    :     Expr[Query] =

      exprs match
        case '{ type keyType <: Label
                ($key: keyType, $value: valueType) } :: tail =>

          Expr.summon[keyType is Parametric into (? >: valueType)].getOrElse:
            Expr.summon[keyType is Parametric].absolve match
              case Some('{ $parametric: (Parametric { type Result = resultType }) }) =>
                halt(m"""the parameter ${key.valueOrAbort} takes values of ${Type.of[resultType]}
                         but the provided value had type ${Type.of[valueType]}""")

              case None =>
                halt(m"could not find a contextual Parametric value for ${key.valueOrAbort}")


          val encodable = Expr.summon[valueType is Encodable in Query].getOrElse:
            halt(m"""there is no contextual ${Type.of[Encodable in Query]} instance for values
                     of ${Type.of[valueType]}""")

          val parameters = '{  given valueType is Encodable in Query = $encodable
                               $value.encode.prefix($key.tt).values  }

          recur(tail, parameters :: done)

        case _ =>
          '{Query.of(${Expr.ofList(done.reverse)}.flatten)}

    values.absolve match
      case Varargs(exprs) => recur(exprs.to(List))
