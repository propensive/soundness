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
import gossamer.*
import hieroglyph.*, charEncoders.utf8
import monotonous.*
import spectacular.*

object Auth:
  import alphabets.base64.standard

  given Auth is Showable =
    case Basic(username, password) => t"Basic ${t"$username:$password".bytes.serialize[Base64]}"
    case Bearer(token)             => t"Bearer $token"
    case Digest(digest)            => t"Digest $digest"
    case Hoba(text)                => t"HOBA $text"
    case Mutual(text)              => t"Mutual $text"
    case Negotiate(text)           => t"Negotiate $text"
    case OAuth(text)               => t"OAuth $text"
    case ScramSha1(text)           => t"SCRAM-SHA-1 $text"
    case ScramSha256(text)         => t"SCRAM-SHA-256 $text"
    case Vapid(text)               => t"vapid $text"

  given ("authorization" is GenericHttpRequestParam[Auth]) = _.show

enum Auth:
  case Basic(username: Text, password: Text)
  case Bearer(token: Text)
  case Digest(digest: Text)
  case Hoba(text: Text)
  case Mutual(text: Text)
  case Negotiate(text: Text)
  case OAuth(text: Text)
  case ScramSha1(text: Text)
  case ScramSha256(text: Text)
  case Vapid(text: Text)
