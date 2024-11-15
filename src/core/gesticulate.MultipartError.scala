package gesticulate

import anticipation.*
import fulminate.*

import scala.reflect.*

object MultipartError:
  enum Reason:
    case Expected(char: Char)
    case StreamContinues, BadBoundaryEnding, MediaType, BadDisposition

  given Reason is Communicable =
    case Reason.Expected(char)    => m"the character '$char' was expected"
    case Reason.StreamContinues   => m"the stream continues beyond the last part"
    case Reason.BadBoundaryEnding => m"unexpected content followed the boundary"
    case Reason.MediaType         => m"the media type is invalid"
    case Reason.BadDisposition    => m"the `Content-Disposition` header has the wrong format"

import MultipartError.Reason

case class MultipartError(reason: MultipartError.Reason)(using Diagnostics)
extends Error(m"The multipart data could not be read because $reason")
