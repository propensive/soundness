package jacinta

import anticipation.*
import aviation.*
import contingency.*
import distillate.*
import prepositional.*

package jsonEncodables:
  given encodeInstantsAsUnixEpochMilliseconds: Instant is Encodable in Json =
    instant => Json(instant.long)

  given encodeDurationsAsMilliseconds: Duration is Encodable in Json =
    duration => Json(duration.value.toLong)

package jsonDecodables:
  given decodeInstantsAsUnixEpochMilliseconds: Tactic[JsonError] => Instant is Decodable in Json =
    json => Instant.of(json.as[Long])

  given decodeDurationsAsMilliseconds: Tactic[JsonError] => Duration is Decodable in Json =
    json => Duration.of(json.as[Long])
