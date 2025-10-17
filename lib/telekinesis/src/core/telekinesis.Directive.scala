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
┃    Soundness, version 0.44.0.                                                                    ┃
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
import denominative.*
import hieroglyph.*
import gesticulate.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import urticose.*

object Directive:
  // Request headers
  given contentEncoding: [encoding <: Encoding]
        => ("contentEncoding" is Directive of encoding) = _.name

  given accept: ("accept" is Directive of MediaType) = _.show
  given accept2: ("accept" is Directive of List[MediaType]) = _.map(_.show).join(t",")

  given authorization: ("authorization" is Directive of Auth) = _.show
  given cacheControl: ("cacheControl" is Directive of Text) = identity(_)
  given connection: ("connection" is Directive of Text) = identity(_)
  given contentMd5: ("contentMd5" is Directive of Text) = identity(_)
  given contentType: ("contentType" is Directive of MediaType) = _.basic
  given contentLength: ("contentLength" is Directive of Memory) = _.long.toString.tt
  given cookie: ("cookie" is Directive of List[Cookie.Value]) = _.map(_.show).join(t"; ")
  given date: ("date" is Directive of Text) = identity(_)
  given expect: ("expect" is Directive of Text) = identity(_)
  given forwarded: ("forwarded" is Directive of Text) = identity(_)
  given from: ("from" is Directive of Text) = identity(_)
  given host: ("host" is Directive of Hostname) = _.show
  given http2Settings: ("http2Settings" is Directive of Text) = identity(_)
  given ifMatch: ("ifMatch" is Directive of Text) = identity(_)
  given ifModifiedSince: ("ifModifiedSince" is Directive of Text) = identity(_)
  given ifNoneMatch: ("ifNoneMatch" is Directive of Text) = identity(_)
  given ifRange: ("ifRange" is Directive of Text) = identity(_)
  given ifUnmodifiedSince: ("ifUnmodifiedSince" is Directive of Text) = identity(_)
  given maxForwards: ("maxForwards" is Directive of Int) = _.toString.tt

  given origin: [url: Abstractable across Urls to Text]
        => ("origin" is Directive of url) = _.generic

  given pragma: ("pragma" is Directive of Text) = identity(_)
  given prefer: ("prefer" is Directive of Text) = identity(_)
  given proxyAuthorization: ("proxyAuthorization" is Directive of Text) = identity(_)

  given range: ("range" is Directive of Interval) =
    interval => t"bytes=${interval.start.n0}-${interval.end.n0}"

  given referer: ("referer" is Directive of Text) = identity(_)
  given te: ("te" is Directive of Text) = identity(_)
  given trailer: ("trailer" is Directive of Text) = identity(_)
  given transferEncoding: ("transferEncoding" is Directive of TransferEncoding) = _.encode
  given userAgent: ("userAgent" is Directive of Text) = identity(_)
  given upgrade: ("upgrade" is Directive of Text) = identity(_)
  given via: ("via" is Directive of Text) = identity(_)
  given warning: ("warning" is Directive of Text) = identity(_)

  // Response headers
  given acceptCharset: ("acceptCharset" is Directive of Text) = identity(_)
  given accessControlAllowOrigin: ("accessControlAllowOrigin" is Directive of Text) = identity(_)

  given accessControlAllowCredentials: ("accessControlAllowCredentials" is Directive of Text) =
    identity(_)

  given accessControlExposeHeaders: ("accessControlExposeHeaders" is Directive of Text) =
    identity(_)

  given accessControlMaxAge: ("accessControlMaxAge" is Directive of Text) = identity(_)
  given accessControlAllowMethods: ("accessControlAllowMethods" is Directive of Text) = identity(_)
  given accessControlAllowHeaders: ("accessControlAllowHeaders" is Directive of Text) = identity(_)
  given acceptPatch: ("acceptPatch" is Directive of Text) = identity(_)
  given acceptRanges: ("acceptRanges" is Directive of Text) = identity(_)
  given age: ("age" is Directive of Text) = identity(_)
  given allow: ("allow" is Directive of Text) = identity(_)
  given altSvc: ("altSvc" is Directive of Text) = identity(_)
  given contentDisposition: ("contentDisposition" is Directive of Text) = identity(_)
  given contentLanguage: ("contentLanguage" is Directive of Text) = identity(_)
  given contentLocation: ("contentLocation" is Directive of Text) = identity(_)
  given contentRange: ("contentRange" is Directive of Text) = identity(_)
  given deltaBase: ("deltaBase" is Directive of Text) = identity(_)
  given eTag: ("eTag" is Directive of Text) = identity(_)
  given expires: ("expires" is Directive of Text) = identity(_)
  given im: ("im" is Directive of Text) = identity(_)
  given lastModified: ("lastModified" is Directive of Text) = identity(_)
  given link: ("link" is Directive of Text) = identity(_)
  given location: ("location" is Directive of Text) = identity(_)
  given p3p: ("p3p" is Directive of Text) = identity(_)
  given preferenceApplied: ("preferenceApplied" is Directive of Text) = identity(_)
  given proxyAuthenticate: ("proxyAuthenticate" is Directive of Text) = identity(_)
  given publicKeyPins: ("publicKeyPins" is Directive of Text) = identity(_)
  given retryAfter: ("retryAfter" is Directive of Text) = identity(_)
  given server: ("server" is Directive of Text) = identity(_)
  given setCookie: ("setCookie" is Directive of Text) = identity(_)
  given strictTransportSecurity: ("strictTransportSecurity" is Directive of Text) = identity(_)
  given tk: ("tk" is Directive of Text) = identity(_)
  given vary: ("vary" is Directive of Text) = identity(_)
  given wwwAuthenticate: ("wwwAuthenticate" is Directive of Text) = identity(_)
  given xFrameOptions: ("xFrameOptions" is Directive of Text) = identity(_)

trait Directive extends Topical:
  type Self <: Label
  def encode(value: Topic): Text
  inline def key: Text = valueOf[Self]
