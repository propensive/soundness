package jacinta

import anticipation.*
import aviation.*
import contingency.*
import distillate.*
import prepositional.*

package jsonEncodables:
  given encodeInstantsAsUnixEpochMilliseconds: Instant is Encodable in Json =
    instant => Json(instant.long)

package jsonDecodables:
  given decodeInstantsAsUnixEpochMilliseconds: Tactic[JsonError] => Instant is Decodable in Json =
    json => Instant.of(json.as[Long])
