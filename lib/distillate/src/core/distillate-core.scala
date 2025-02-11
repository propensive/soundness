package distillate

import prepositional.*

extension [FormatType](value: FormatType)
  def decode[ResultType](using decodable: ResultType is Decodable in FormatType): ResultType =
    decodable.decoded(value)
