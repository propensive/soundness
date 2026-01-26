package jacinta

import scala.collection.mutable as scm
import scala.annotation.*

import anticipation.*
import contingency.*
import telekinesis.*
import urticose.*
import vacuous.*
import wisteria.*
import zephyrine.*

package jsonPointerRegistries:
  given standalone: JsonPointer.Registry:
    protected def lookup(url: HttpUrl): Optional[Json] = Unset

  given fetching: (Online, HttpEvent is Loggable, HttpClient) => JsonPointer.Registry:
    protected def lookup(url: HttpUrl): Optional[Json] =
      recover:
        case VariantError(_, _, _) => Unset
        case ConnectError(_)       => Unset
        case HttpError(_, _)       => Unset
        case ParseError(_, _, _)   => Unset
        case JsonError(_)          => Unset
      . within(url.fetch().receive[Json])
