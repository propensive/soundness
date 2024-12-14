/*
    Telekinesis, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import rudiments.*

object ResponseHeader:
  lazy val standard: Map[Text, ResponseHeader[?]] = List(AcceptCharset, AccessControlAllowOrigin,
      AccessControlAllowCredentials, AccessControlExposeHeaders, AccessControlMaxAge,
      AccessControlAllowMethods, AccessControlAllowHeaders, AcceptPatch, AcceptRanges, Age, Allow,
      AltSvc, CacheControl, Connection, ContentDisposition, ContentEncoding, ContentLanguage,
      ContentLength, ContentLocation, ContentMd5, ContentRange, ContentType, Date, DeltaBase, ETag,
      Expires, Im, LastModified, Link, Location, P3p, Pragma, PreferenceApplied, ProxyAuthenticate,
      PublicKeyPins, RetryAfter, Server, SetCookie, StrictTransportSecurity, Trailer,
      TransferEncoding, Tk, Upgrade, Vary, Via, Warning, WwwAuthenticate, XFrameOptions)

    . bi.map(_.header -> _).to(Map)

  def unapply(str: Text): Some[ResponseHeader[?]] =
    Some(standard.get(str.lower).getOrElse(Custom(str)))

  case class Value(header: ResponseHeader[?], value: Text)

enum ResponseHeader[ValueType](val header: Text):
  case AcceptCharset extends ResponseHeader[Text](t"accept-charset")
  case AccessControlAllowOrigin extends ResponseHeader[Text](t"access-control-allow-origin")

  case AccessControlAllowCredentials
  extends ResponseHeader[Text](t"access-control-allow-credentials")

  case AccessControlExposeHeaders extends ResponseHeader[Text](t"access-control-expose-headers")
  case AccessControlMaxAge extends ResponseHeader[Text](t"access-control-max-age")
  case AccessControlAllowMethods extends ResponseHeader[Text](t"access-control-allow-methods")
  case AccessControlAllowHeaders extends ResponseHeader[Text](t"access-control-allow-headers")
  case AcceptPatch extends ResponseHeader[Text](t"accept-patch")
  case AcceptRanges extends ResponseHeader[Text](t"accept-ranges")
  case Age extends ResponseHeader[Text](t"age")
  case Allow extends ResponseHeader[Text](t"allow")
  case AltSvc extends ResponseHeader[Text](t"alt-svc")
  case CacheControl extends ResponseHeader[Text](t"cache-control")
  case Connection extends ResponseHeader[Text](t"connection")
  case ContentDisposition extends ResponseHeader[Text](t"content-disposition")
  case ContentEncoding extends ResponseHeader[Text](t"content-encoding")
  case ContentLanguage extends ResponseHeader[Text](t"content-language")
  case ContentLength extends ResponseHeader[Memory](t"content-length")
  case ContentLocation extends ResponseHeader[Text](t"content-location")
  case ContentMd5 extends ResponseHeader[Text](t"content-md5")
  case ContentRange extends ResponseHeader[Text](t"content-range")
  case ContentType extends ResponseHeader[Text](t"content-type")
  case Date extends ResponseHeader[Text](t"date")
  case DeltaBase extends ResponseHeader[Text](t"delta-base")
  case ETag extends ResponseHeader[Text](t"etag")
  case Expires extends ResponseHeader[Text](t"expires")
  case Im extends ResponseHeader[Text](t"im")
  case LastModified extends ResponseHeader[Text](t"last-modified")
  case Link extends ResponseHeader[Text](t"link")
  case Location extends ResponseHeader[Text](t"Location")
  case P3p extends ResponseHeader[Text](t"p3p")
  case Pragma extends ResponseHeader[Text](t"pragma")
  case PreferenceApplied extends ResponseHeader[Text](t"preference-applied")
  case ProxyAuthenticate extends ResponseHeader[Text](t"proxy-authenticate")
  case PublicKeyPins extends ResponseHeader[Text](t"public-key-pins")
  case RetryAfter extends ResponseHeader[Text](t"retry-after")
  case Server extends ResponseHeader[Text](t"server")
  case SetCookie extends ResponseHeader[Text](t"set-cookie")
  case StrictTransportSecurity extends ResponseHeader[Text](t"strict-transport-security")
  case Trailer extends ResponseHeader[Text](t"trailer")
  case TransferEncoding extends ResponseHeader[Text](t"transfer-encoding")
  case Tk extends ResponseHeader[Text](t"tk")
  case Upgrade extends ResponseHeader[Text](t"upgrade")
  case Vary extends ResponseHeader[Text](t"vary")
  case Via extends ResponseHeader[Text](t"via")
  case Warning extends ResponseHeader[Text](t"warning")
  case WwwAuthenticate extends ResponseHeader[Text](t"www-authenticate")
  case XFrameOptions extends ResponseHeader[Text](t"x-frame-options")
  case Custom(name: Text) extends ResponseHeader[Text](name.lower)

  def apply(value: Text): ResponseHeader.Value = ResponseHeader.Value(this, value)
