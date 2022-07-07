/*
    Telekinesis, version 0.4.0. Copyright 2022-22 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import gossamer.*
import gastronomy.*

object RequestHeader:
  lazy val standard: Map[Text, RequestHeader] = Set(AIm, Accept, AcceptCharset, AcceptDatetime,
      AcceptEncoding, AcceptLanguage, AccessControlRequestMethod, AccessControlRequestHeaders,
      Authorization, CacheControl, Connection, ContentEncoding, ContentLength, ContentMd5,
      ContentType, Cookie, Date, Expect, Forwarded, From, Host, Http2Settings, IfMatch,
      IfModifiedSince, IfNoneMatch, IfRange, IfUnmodifiedSince, MaxForwards, Origin, Pragma, Prefer,
      ProxyAuthorization, Range, Referer, Te, Trailer, TransferEncoding, UserAgent, Upgrade, Via,
      Warning).mtwin.map(_.header -> _).to(Map)

  def unapply(str: Text): Some[RequestHeader] = Some(standard.get(str.lower).getOrElse(Custom(str)))
  
  object Value:
    given Show[Value] = value => t"${value.header}: ${value.value}"

  case class Value(header: RequestHeader, value: Text)

  object nonStandard:
    val UpgradeInsecureRequests = RequestHeader.Custom(t"upgrade-insecure-requests")
    val XRequestedWith = RequestHeader.Custom(t"x-requested-with")
    val Dnt = RequestHeader.Custom(t"dnt")
    val XForwardedFor = RequestHeader.Custom(t"x-forwarded-for")
    val XForwardedHost = RequestHeader.Custom(t"x-forwarded-host")
    val XForwardedProto = RequestHeader.Custom(t"x-forwarded-proto")
    val FrontEndHttps = RequestHeader.Custom(t"front-end-https")
    val XHttpMethodOverride = RequestHeader.Custom(t"x-http-method-override")
    val XattDeviceId = RequestHeader.Custom(t"x-att-deviceid")
    val XWapProfile = RequestHeader.Custom(t"x-wap-profile")
    val ProxyConnection = RequestHeader.Custom(t"proxy-connection")
    val Xuidh = RequestHeader.Custom(t"x-uidh")
    val XCsrfToken = RequestHeader.Custom(t"x-csrf-token")
    val XRequestId = RequestHeader.Custom(t"x-request-id")
    val XCorrelationId = RequestHeader.Custom(t"x-correlation-id")
    val SaveData = RequestHeader.Custom(t"save-data")

  given Show[RequestHeader] = _.header

enum RequestHeader(val header: Text):
  case AIm extends RequestHeader(t"a-im")
  case Accept extends RequestHeader(t"accept")
  case AcceptCharset extends RequestHeader(t"accept-charset")
  case AcceptDatetime extends RequestHeader(t"accept-datetime")
  case AcceptEncoding extends RequestHeader(t"accept-encoding")
  case AcceptLanguage extends RequestHeader(t"accept-language")
  case AccessControlRequestMethod extends RequestHeader(t"access-control-request-method")
  case AccessControlRequestHeaders extends RequestHeader(t"access-control-request-headers")
  case Authorization extends RequestHeader(t"authorization")
  case CacheControl extends RequestHeader(t"cache-control")
  case Connection extends RequestHeader(t"connection")
  case ContentEncoding extends RequestHeader(t"content-encoding")
  case ContentLength extends RequestHeader(t"content-length")
  case ContentMd5 extends RequestHeader(t"content-md5")
  case ContentType extends RequestHeader(t"content-type")
  case Cookie extends RequestHeader(t"cookie")
  case Date extends RequestHeader(t"date")
  case Expect extends RequestHeader(t"expect")
  case Forwarded extends RequestHeader(t"forwarded")
  case From extends RequestHeader(t"from")
  case Host extends RequestHeader(t"host")
  case Http2Settings extends RequestHeader(t"http2-settings")
  case IfMatch extends RequestHeader(t"if-match")
  case IfModifiedSince extends RequestHeader(t"if-modified-since")
  case IfNoneMatch extends RequestHeader(t"if-none-match")
  case IfRange extends RequestHeader(t"if-range")
  case IfUnmodifiedSince extends RequestHeader(t"if-unmodified-since")
  case MaxForwards extends RequestHeader(t"max-forwards")
  case Origin extends RequestHeader(t"origin")
  case Pragma extends RequestHeader(t"pragma")
  case Prefer extends RequestHeader(t"prefer")
  case ProxyAuthorization extends RequestHeader(t"proxy-authorization")
  case Range extends RequestHeader(t"range")
  case Referer extends RequestHeader(t"referer")
  case Te extends RequestHeader(t"te")
  case Trailer extends RequestHeader(t"trailer")
  case TransferEncoding extends RequestHeader(t"transfer-encoding")
  case UserAgent extends RequestHeader(t"user-agent")
  case Upgrade extends RequestHeader(t"upgrade")
  case Via extends RequestHeader(t"via")
  case Warning extends RequestHeader(t"warning")
  case Custom(name: Text) extends RequestHeader(name.lower)
  
  def apply(content: Text): RequestHeader.Value = RequestHeader.Value(this, content)

object ResponseHeader:
  lazy val standard: Map[Text, ResponseHeader] = List(AcceptCh, AccessControlAllowOrigin,
      AccessControlAllowCredentials, AccessControlExposeHeaders, AccessControlMaxAge,
      AccessControlAllowMethods, AccessControlAllowHeaders, AcceptPatch, AcceptRanges, Age, Allow,
      AltSvc, CacheControl, Connection, ContentDisposition, ContentEncoding, ContentLanguage,
      ContentLength, ContentLocation, ContentMd5, ContentRange, ContentType, Date, DeltaBase, ETag,
      Expires, Im, LastModified, Link, Location, P3p, Pragma, PreferenceApplied, ProxyAuthenticate,
      PublicKeyPins, RetryAfter, Server, SetCookie, StrictTransportSecurity, Trailer,
      TransferEncoding, Tk, Upgrade, Vary, Via, Warning, WwwAuthenticate, XFrameOptions)
    .mtwin.map(_.header -> _).to(Map)
  
  def unapply(str: Text): Some[ResponseHeader] =
    Some(standard.get(str.lower).getOrElse(Custom(str)))

  given Show[ResponseHeader] = _.header

enum ResponseHeader(val header: Text):
  case AcceptCh extends ResponseHeader(t"accept-ch")
  case AccessControlAllowOrigin extends ResponseHeader(t"access-control-allow-origin")
  case AccessControlAllowCredentials extends ResponseHeader(t"access-control-allow-credentials")
  case AccessControlExposeHeaders extends ResponseHeader(t"access-control-expose-headers")
  case AccessControlMaxAge extends ResponseHeader(t"access-control-max-age")
  case AccessControlAllowMethods extends ResponseHeader(t"access-control-allow-methods")
  case AccessControlAllowHeaders extends ResponseHeader(t"access-control-allow-headers")
  case AcceptPatch extends ResponseHeader(t"accept-patch")
  case AcceptRanges extends ResponseHeader(t"accept-ranges")
  case Age extends ResponseHeader(t"age")
  case Allow extends ResponseHeader(t"allow")
  case AltSvc extends ResponseHeader(t"alt-svc")
  case CacheControl extends ResponseHeader(t"cache-control")
  case Connection extends ResponseHeader(t"connection")
  case ContentDisposition extends ResponseHeader(t"content-disposition")
  case ContentEncoding extends ResponseHeader(t"content-encoding")
  case ContentLanguage extends ResponseHeader(t"content-language")
  case ContentLength extends ResponseHeader(t"content-length")
  case ContentLocation extends ResponseHeader(t"content-location")
  case ContentMd5 extends ResponseHeader(t"content-md5")
  case ContentRange extends ResponseHeader(t"content-range")
  case ContentType extends ResponseHeader(t"content-type")
  case Date extends ResponseHeader(t"date")
  case DeltaBase extends ResponseHeader(t"delta-base")
  case ETag extends ResponseHeader(t"etag")
  case Expires extends ResponseHeader(t"expires")
  case Im extends ResponseHeader(t"im")
  case LastModified extends ResponseHeader(t"last-modified")
  case Link extends ResponseHeader(t"link")
  case Location extends ResponseHeader(t"Location")
  case P3p extends ResponseHeader(t"p3p")
  case Pragma extends ResponseHeader(t"pragma")
  case PreferenceApplied extends ResponseHeader(t"preference-applied")
  case ProxyAuthenticate extends ResponseHeader(t"proxy-authenticate")
  case PublicKeyPins extends ResponseHeader(t"public-key-pins")
  case RetryAfter extends ResponseHeader(t"retry-after")
  case Server extends ResponseHeader(t"server")
  case SetCookie extends ResponseHeader(t"set-cookie")
  case StrictTransportSecurity extends ResponseHeader(t"strict-transport-security")
  case Trailer extends ResponseHeader(t"trailer")
  case TransferEncoding extends ResponseHeader(t"transfer-encoding")
  case Tk extends ResponseHeader(t"tk")
  case Upgrade extends ResponseHeader(t"upgrade")
  case Vary extends ResponseHeader(t"vary")
  case Via extends ResponseHeader(t"via")
  case Warning extends ResponseHeader(t"warning")
  case WwwAuthenticate extends ResponseHeader(t"www-authenticate")
  case XFrameOptions extends ResponseHeader(t"x-frame-options")
  case Custom(name: Text) extends ResponseHeader(name.lower)

object Auth:
  given Show[Auth] =
    case Basic(username, password) => t"Basic ${t"$username:$password".bytes.encode[Base64]}"
    case Bearer(token)             => t"Bearer $token"
    case Digest(digest)            => t"Digest $digest"
    case Hoba(text)                => t"HOBA $text"
    case Mutual(text)              => t"Mutual $text"
    case Negotiate(text)           => t"Negotiate $text"
    case OAuth(text)               => t"OAuth $text"
    case ScramSha1(text)           => t"SCRAM-SHA-1 $text"
    case ScramSha256(text)         => t"SCRAM-SHA-256 $text"
    case Vapid(text)               => t"vapid $text"

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

  def apply(): RequestHeader.Value = RequestHeader.Authorization(this.show)
