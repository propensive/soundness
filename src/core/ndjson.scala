package jacinta

import turbulence.*
import perforate.*
import merino.*
import anticipation.*
import rudiments.*

object Ndjson:
  def parse
      [SourceType]
      (value: SourceType)
      (using readable: Readable[SourceType, Line], jsonParse: Raises[JsonParseError], textReadable: Readable[Text, Bytes])
      : Ndjson =
    Ndjson(value.stream[Line].map { line => Json.parse(line.content) })
  
case class Ndjson(stream: LazyList[Json])
