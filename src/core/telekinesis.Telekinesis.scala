/*
    Telekinesis, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package telekinesis

import scala.quoted.*

import anticipation.*
import fulminate.*
import gossamer.*
import nettlesome.*
import prepositional.*
import proscenium.*
import vacuous.*

object Telekinesis:
  def expand
     (todo:   Seq[Expr[Any]],
      method: Optional[Expr[Http.Method]] = Unset,
      done:   List[Expr[HttpHeader]]     = Nil)
     (using Quotes)
  :     (Optional[Expr[Http.Method]], Expr[Seq[HttpHeader]]) =
    import quotes.reflect.*

    def unnamed[ValueType: Type](value: Expr[ValueType], tail: Seq[Expr[Any]]) =
      Expr.summon[Capitate of ValueType].getOrElse:
        val typeName = TypeRepr.of[ValueType].show
        halt(m"the type $typeName does not uniquely identify a particular HTTP header")

      . absolve
      . match
        case '{ type keyType <: Label; $capitate: (Capitate { type Self = keyType }) } =>
          TypeRepr.of[keyType].absolve match
            case ConstantType(StringConstant(key)) =>
              val header =
                '{HttpHeader(${Expr(key)}.tt.uncamel.snake, $capitate.encode($value))}

              expand(tail, method, header :: done)

    todo.absolve match
      case '{ $method2: Http.Method } +: tail =>
        if method.present then halt(m"the request method can only be specified once")
        expand(tail, method2, done)

      case '{ ("", $method2: Http.Method) } +: tail =>
        if method.present then halt(m"the request method can only be specified once")
        expand(tail, method2, done)

      case '{ ("", $value: valueType) } +: tail =>
        unnamed[valueType](value, tail)

      case '{ type keyType <: Label; ($key: keyType, $value: valueType) } +: tail =>
        val name: Text = key.value.get.tt.uncamel.map(_.capitalize).kebab

        val capitate = Expr.summon[keyType is Capitate of valueType].getOrElse:
          val typeName = TypeRepr.of[valueType].show
          halt(m"the header $name cannot take a value of type $typeName")

        val header = '{HttpHeader($key.tt.uncamel.snake, $capitate.encode($value))}
        expand(tail, method, header :: done)

      case '{ $value: valueType } +: tail =>
        unnamed[valueType](value, tail)

      case Seq() =>
        (method, Expr.ofList(done.reverse))

  def submit[PayloadType: Type]
     (submit:   Expr[Submit],
      headers:  Expr[Seq[(Label, Any)] | Seq[Any]],
      online:   Expr[Online],
      loggable: Expr[HttpEvent is Loggable],
      payload:  Expr[PayloadType],
      postable: Expr[Postable[PayloadType]])
     (using Quotes)
  :     Expr[HttpResponse] =

    headers.absolve match
      case Varargs(exprs) =>
        val (method0, headers) = expand(exprs)

        val method = method0 match
          case Unset                    => '{Http.Post}
          case method: Expr[Http.Method] => method

        '{  given Online = $online
            given Postable[PayloadType] = $postable
            given HttpEvent is Loggable = $loggable
            Http.request($submit.url, $payload, $method, $headers)  }

  def fetch
     (fetch:    Expr[Fetch],
      headers:  Expr[Seq[(Label, Any)] | Seq[Any]],
      online:   Expr[Online],
      loggable: Expr[HttpEvent is Loggable])
     (using Quotes)
  :     Expr[HttpResponse] =

    headers.absolve match
      case Varargs(exprs) =>
        val (method0, headers) = expand(exprs)

        val method = method0 match
          case Unset                    => '{Http.Get}
          case method: Expr[Http.Method] => method

        '{  given Online = $online
            given HttpEvent is Loggable = $loggable
            Http.request($fetch.url, (), $method, $headers)  }
