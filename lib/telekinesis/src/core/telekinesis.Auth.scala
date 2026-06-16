                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package telekinesis

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8Encoder
import kaleidoscope.*
import monotonous.*
import prepositional.*
import spectacular.*

object Auth:
  import alphabets.base64Standard

  given showable: Auth is Showable =
    case Basic(username, password) => t"Basic ${t"$username:$password".data.serialize[Base64]}"
    case Bearer(token)             => t"Bearer $token"
    case Digest(digest)            => t"Digest $digest"
    case Hoba(text)                => t"HOBA $text"
    case Mutual(text)              => t"Mutual $text"
    case Negotiate(text)           => t"Negotiate $text"
    case OAuth(text)               => t"OAuth $text"
    case ScramSha1(text)           => t"SCRAM-SHA-1 $text"
    case ScramSha256(text)         => t"SCRAM-SHA-256 $text"
    case Vapid(text)               => t"vapid $text"

  given decodable: Tactic[AuthError] => Auth is Decodable in Text = value => value match
    case r"Bearer $token(.*)"        => Bearer(token)
    case r"Digest $digest(.*)"       => Digest(digest)
    case r"HOBA $value(.*)"          => Hoba(value)
    case r"Mutual $value(.*)"        => Mutual(value)
    case r"Negotiate $value(.*)"     => Negotiate(value)
    case r"OAuth $value(.*)"         => OAuth(value)
    case r"SCRAM-SHA-1 $value(.*)"   => ScramSha1(value)
    case r"SCRAM-SHA-256 $value(.*)" => ScramSha256(value)
    case r"vapid $value(.*)"         => Vapid(value)

    case r"Basic $username(.*):$password(.*)" =>
      safely(Basic(username.deserialize[Base64].utf8, password.deserialize[Base64].utf8)).lest:
        AuthError(value)

    case value =>
      abort(AuthError(value))

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
