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
     (todo:   Seq[Expr[(Label, Any)]],
      method: Optional[Expr[HttpMethod]]   = Unset,
      done:   List[Expr[HttpRequestHeader]] = Nil)
     (using Quotes)
  :     (Optional[Expr[HttpMethod]], Expr[Seq[HttpRequestHeader]]) =
    import quotes.reflect.*

    todo match
      case '{ ("", $method: HttpMethod) } +: tail =>
        expand(tail, method, done)

      case '{ type keyType <: Label; ($key: keyType, $value: valueType) } +: tail =>
        val name: Text = key.value.get.tt.uncamel.map(_.capitalize).kebab

        val capitate = Expr.summon[keyType is Capitate of valueType].getOrElse:
          val typeName = TypeRepr.of[valueType].show
          halt(m"the header $name cannot take a value of type $typeName")

        val header = '{HttpRequestHeader($key.tt.uncamel.snake, $capitate.encode($value))}
        expand(tail, method, header :: done)

      case _ =>
        (method, Expr.ofList(done.reverse))

  def submit[PayloadType: Type]
     (submit:   Expr[Submit],
      headers:  Expr[Seq[(Label, Any)]],
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
          case Unset                    => '{Post}
          case method: Expr[HttpMethod] => method

        '{
            Http.request($submit.url, $payload, $method, $headers)
             (using $postable, $online)
             (using $loggable)  }

  def fetch
     (fetch:    Expr[Fetch],
      headers:  Expr[Seq[(Label, Any)]],
      online:   Expr[Online],
      loggable: Expr[HttpEvent is Loggable])
     (using Quotes)
  :     Expr[HttpResponse] =

    headers.absolve match
      case Varargs(exprs) =>
        val (method0, headers) = expand(exprs)

        val method = method0 match
          case Unset                    => '{Get}
          case method: Expr[HttpMethod] => method

        '{  given Online = $online
            given HttpEvent is Loggable = $loggable
            Http.request($fetch.url, (), $method, $headers)  }
