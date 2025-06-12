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
┃    Soundness, version 0.34.0.                                                                    ┃
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
import nettlesome.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*

object Prefixable:
  // Request headers
  given contentEncoding: [encoding <: Encoding]
        => ("contentEncoding" is Prefixable of encoding) = _.name

  given accept: ("accept" is Prefixable of MediaType) = _.show
  given accept2: ("accept" is Prefixable of List[MediaType]) = _.map(_.show).join(t",")

  given authorization: ("authorization" is Prefixable of Auth) = _.show
  given cacheControl: ("cacheControl" is Prefixable of Text) = identity(_)
  given connection: ("connection" is Prefixable of Text) = identity(_)
  given contentMd5: ("contentMd5" is Prefixable of Text) = identity(_)
  given contentType: ("contentType" is Prefixable of MediaType) = _.basic
  given contentLength: ("contentLength" is Prefixable of Memory) = _.long.toString.tt
  given cookie: ("cookie" is Prefixable of List[Cookie.Value]) = _.map(_.show).join(t"; ")
  given date: ("date" is Prefixable of Text) = identity(_)
  given expect: ("expect" is Prefixable of Text) = identity(_)
  given forwarded: ("forwarded" is Prefixable of Text) = identity(_)
  given from: ("from" is Prefixable of Text) = identity(_)
  given host: ("host" is Prefixable of Hostname) = _.show
  given http2Settings: ("http2Settings" is Prefixable of Text) = identity(_)
  given ifMatch: ("ifMatch" is Prefixable of Text) = identity(_)
  given ifModifiedSince: ("ifModifiedSince" is Prefixable of Text) = identity(_)
  given ifNoneMatch: ("ifNoneMatch" is Prefixable of Text) = identity(_)
  given ifRange: ("ifRange" is Prefixable of Text) = identity(_)
  given ifUnmodifiedSince: ("ifUnmodifiedSince" is Prefixable of Text) = identity(_)
  given maxForwards: ("maxForwards" is Prefixable of Int) = _.toString.tt

  given origin: [url: Abstractable across Urls into Text]
        => ("origin" is Prefixable of url) = _.generic

  given pragma: ("pragma" is Prefixable of Text) = identity(_)
  given prefer: ("prefer" is Prefixable of Text) = identity(_)
  given proxyAuthorization: ("proxyAuthorization" is Prefixable of Text) = identity(_)

  given range: ("range" is Prefixable of Interval) =
    interval => t"bytes=${interval.start.n0}-${interval.end.n0}"

  given referer: ("referer" is Prefixable of Text) = identity(_)
  given te: ("te" is Prefixable of Text) = identity(_)
  given trailer: ("trailer" is Prefixable of Text) = identity(_)
  given transferEncoding: ("transferEncoding" is Prefixable of TransferEncoding) = _.encode
  given userAgent: ("userAgent" is Prefixable of Text) = identity(_)
  given upgrade: ("upgrade" is Prefixable of Text) = identity(_)
  given via: ("via" is Prefixable of Text) = identity(_)
  given warning: ("warning" is Prefixable of Text) = identity(_)

  // Response headers
  given acceptCharset: ("acceptCharset" is Prefixable of Text) = identity(_)
  given accessControlAllowOrigin: ("accessControlAllowOrigin" is Prefixable of Text) = identity(_)

  given accessControlAllowCredentials: ("accessControlAllowCredentials" is Prefixable of Text) =
    identity(_)

  given accessControlExposeHeaders: ("accessControlExposeHeaders" is Prefixable of Text) =
    identity(_)

  given accessControlMaxAge: ("accessControlMaxAge" is Prefixable of Text) = identity(_)
  given accessControlAllowMethods: ("accessControlAllowMethods" is Prefixable of Text) = identity(_)
  given accessControlAllowHeaders: ("accessControlAllowHeaders" is Prefixable of Text) = identity(_)
  given acceptPatch: ("acceptPatch" is Prefixable of Text) = identity(_)
  given acceptRanges: ("acceptRanges" is Prefixable of Text) = identity(_)
  given age: ("age" is Prefixable of Text) = identity(_)
  given allow: ("allow" is Prefixable of Text) = identity(_)
  given altSvc: ("altSvc" is Prefixable of Text) = identity(_)
  given contentDisposition: ("contentDisposition" is Prefixable of Text) = identity(_)
  given contentLanguage: ("contentLanguage" is Prefixable of Text) = identity(_)
  given contentLocation: ("contentLocation" is Prefixable of Text) = identity(_)
  given contentRange: ("contentRange" is Prefixable of Text) = identity(_)
  given deltaBase: ("deltaBase" is Prefixable of Text) = identity(_)
  given eTag: ("eTag" is Prefixable of Text) = identity(_)
  given expires: ("expires" is Prefixable of Text) = identity(_)
  given im: ("im" is Prefixable of Text) = identity(_)
  given lastModified: ("lastModified" is Prefixable of Text) = identity(_)
  given link: ("link" is Prefixable of Text) = identity(_)
  given location: ("location" is Prefixable of Text) = identity(_)
  given p3p: ("p3p" is Prefixable of Text) = identity(_)
  given preferenceApplied: ("preferenceApplied" is Prefixable of Text) = identity(_)
  given proxyAuthenticate: ("proxyAuthenticate" is Prefixable of Text) = identity(_)
  given publicKeyPins: ("publicKeyPins" is Prefixable of Text) = identity(_)
  given retryAfter: ("retryAfter" is Prefixable of Text) = identity(_)
  given server: ("server" is Prefixable of Text) = identity(_)
  given setCookie: ("setCookie" is Prefixable of Text) = identity(_)
  given strictTransportSecurity: ("strictTransportSecurity" is Prefixable of Text) = identity(_)
  given tk: ("tk" is Prefixable of Text) = identity(_)
  given vary: ("vary" is Prefixable of Text) = identity(_)
  given wwwAuthenticate: ("wwwAuthenticate" is Prefixable of Text) = identity(_)
  given xFrameOptions: ("xFrameOptions" is Prefixable of Text) = identity(_)

trait Prefixable:
  type Self <: Label
  type Subject
  def encode(value: Subject): Text
  inline def key: Text = valueOf[Self]
