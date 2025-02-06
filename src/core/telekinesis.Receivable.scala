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

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import turbulence.*
import vacuous.*

object Receivable:
  given text: Text is Receivable = _.body.read[Bytes].utf8
  given bytes: Bytes is Receivable = _.body.read[Bytes]
  given byteStream: Stream[Bytes] is Receivable = _.body

  given readable: [StreamType: Aggregable by Bytes] => Tactic[HttpError]
  =>    StreamType is Receivable =
    response =>
      response.successBody.let(StreamType.aggregate(_)).lest:
        HttpError(response.status, response.headers)

  given instantiable: [ContentType: Instantiable across HttpRequests from Text] => Tactic[HttpError]
  =>    ContentType is Receivable = response =>
    response.successBody.let(_.read[Bytes].utf8).let(ContentType(_)).lest:
      HttpError(response.status, response.headers)

  given httpStatus: HttpStatus is Receivable = _.status

trait Receivable:
  type Self
  def read(response: HttpResponse): Self
