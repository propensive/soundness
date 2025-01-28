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
import hieroglyph.*
import gesticulate.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*

object Capitate:
  given contentEncoding: [EncodingType <: Encoding]
  =>    ("contentEncoding" is Capitate of EncodingType) = _.name

  given accept: ("accept" is Capitate of MediaType) = _.basic

  given authorization: ("authorization" is Capitate of Auth) = _.show
  given cacheControl: ("cacheControl" is Capitate of Text) = identity(_)
  given connection: ("connection" is Capitate of Text) = identity(_)
  given contentMd5: ("contentMd5" is Capitate of Text) = identity(_)
  given contentType: ("contentType" is Capitate of MediaType) = _.basic
  given contentLength: ("contentLength" is Capitate of Memory) = _.long.toString.tt
  given date: ("date" is Capitate of Text) = identity(_)
  given expect: ("expect" is Capitate of Text) = identity(_)
  given forwarded: ("forwarded" is Capitate of Text) = identity(_)
  given from: ("from" is Capitate of Text) = identity(_)
  given host: ("host" is Capitate of Text) = identity(_)
  given http2Settings: ("http2Settings" is Capitate of Text) = identity(_)
  given ifMatch: ("ifMatch" is Capitate of Text) = identity(_)
  given ifModifiedSince: ("ifModifiedSince" is Capitate of Text) = identity(_)
  given ifNoneMatch: ("ifNonMatch" is Capitate of Text) = identity(_)
  given ifRange: ("ifRange" is Capitate of Text) = identity(_)
  given ifUnmodifiedSince: ("ifUnmodifiedSince" is Capitate of Text) = identity(_)
  given maxForwards: ("maxForwards" is Capitate of Int) = _.toString.tt
  given origin: ("origin" is Capitate of Text) = identity(_)
  given pragma: ("pragma" is Capitate of Text) = identity(_)
  given prefer: ("prefer" is Capitate of Text) = identity(_)
  given proxyAuthorization: ("proxyAuthorization" is Capitate of Text) = identity(_)
  given range: ("range" is Capitate of Text) = identity(_)
  given referer: ("referer" is Capitate of Text) = identity(_)
  given te: ("te" is Capitate of Text) = identity(_)
  given trailer: ("trailer" is Capitate of Text) = identity(_)
  given transferEncoding: ("transferEncoding" is Capitate of Text) = identity(_)
  given userAgent: ("userAgent" is Capitate of Text) = identity(_)
  given upgrade: ("upgrade" is Capitate of Text) = identity(_)
  given via: ("via" is Capitate of Text) = identity(_)
  given warning: ("warning" is Capitate of Text) = identity(_)

trait Capitate:
  type Self <: Label
  type Subject
  def encode(value: Subject): Text
  inline def key: Text = valueOf[Self]
