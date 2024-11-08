package telekinesis

import spectacular.*
import anticipation.*
import gossamer.*

object HttpVersion:
  given HttpVersion is Showable as showable =
    case 0.9 => t"HTTP/0.9"
    case 1.0 => t"HTTP/1.0"
    case 1.1 => t"HTTP/1.1"
    case 2.0 => t"HTTP/2"
    case 3.0 => t"HTTP/3"

  def parse(text: Text): HttpVersion = text match
    case t"HTTP/0.9"             => 0.9
    case t"HTTP/1.1"             => 1.1
    case t"HTTP/2" | t"HTTP/2.0" => 2.0
    case t"HTTP/3" | t"HTTP/3.0" => 3.0
    case _                       => 1.0

type HttpVersion = 0.9 | 1.0 | 1.1 | 2.0 | 3.0
