/*

    Scintillate, version 0.2.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package scintillate

import rudiments.*

object RequestHeader:
  val standard: Map[String, RequestHeader] = Set(AIm, Accept, AcceptCharset, AcceptDatetime, AcceptEncoding,
      AcceptLanguage, AccessControlRequestMethod, AccessControlRequestHeaders, Authorization, CacheControl,
      Connection, ContentEncoding, ContentLength, ContentMd5, ContentType, Cookie, Date, Expect, Forwarded,
      From, Host, Http2Settings, IfMatch, IfModifiedSince, IfNoneMatch, IfRange, IfUnmodifiedSince, MaxForwards,
      Origin, Pragma, Prefer, ProxyAuthorization, Range, Referer, Te, Trailer, TransferEncoding, UserAgent,
      Upgrade, Via, Warning).map(_.twin).map(_.header -> _).to(Map)

  def unapply(str: String): Some[RequestHeader] =
    Some(standard.get(str.toLowerCase).getOrElse(NonStandard(str)))

  object nonStandard:
    val UpgradeInsecureRequests = RequestHeader.NonStandard("upgrade-insecure-requests")
    val XRequestedWith = RequestHeader.NonStandard("x-requested-with")
    val Dnt = RequestHeader.NonStandard("dnt")
    val XForwardedFor = RequestHeader.NonStandard("x-forwarded-for")
    val XForwardedHost = RequestHeader.NonStandard("x-forwarded-host")
    val XForwardedProto = RequestHeader.NonStandard("x-forwarded-proto")
    val FrontEndHttps = RequestHeader.NonStandard("front-end-https")
    val XHttpMethodOverride = RequestHeader.NonStandard("x-http-method-override")
    val XattDeviceId = RequestHeader.NonStandard("x-att-deviceid")
    val XWapProfile = RequestHeader.NonStandard("x-wap-profile")
    val ProxyConnection = RequestHeader.NonStandard("proxy-connection")
    val Xuidh = RequestHeader.NonStandard("x-uidh")
    val XCsrfToken = RequestHeader.NonStandard("x-csrf-token")
    val XRequestId = RequestHeader.NonStandard("x-request-id")
    val XCorrelationId = RequestHeader.NonStandard("x-correlation-id")
    val SaveData = RequestHeader.NonStandard("save-data")

enum RequestHeader(val header: String):
  case AIm extends RequestHeader("a-im")
  case Accept extends RequestHeader("accept")
  case AcceptCharset extends RequestHeader("accept-charset")
  case AcceptDatetime extends RequestHeader("accept-datetime")
  case AcceptEncoding extends RequestHeader("accept-encoding")
  case AcceptLanguage extends RequestHeader("accept-language")
  case AccessControlRequestMethod extends RequestHeader("access-control-request-method")
  case AccessControlRequestHeaders extends RequestHeader("access-control-request-headers")
  case Authorization extends RequestHeader("authorization")
  case CacheControl extends RequestHeader("cache-control")
  case Connection extends RequestHeader("connection")
  case ContentEncoding extends RequestHeader("content-encoding")
  case ContentLength extends RequestHeader("content-length")
  case ContentMd5 extends RequestHeader("content-md5")
  case ContentType extends RequestHeader("content-Type")
  case Cookie extends RequestHeader("cookie")
  case Date extends RequestHeader("date")
  case Expect extends RequestHeader("expect")
  case Forwarded extends RequestHeader("forwarded")
  case From extends RequestHeader("from")
  case Host extends RequestHeader("host")
  case Http2Settings extends RequestHeader("http2-settings")
  case IfMatch extends RequestHeader("if-match")
  case IfModifiedSince extends RequestHeader("if-modified-since")
  case IfNoneMatch extends RequestHeader("if-none-match")
  case IfRange extends RequestHeader("if-range")
  case IfUnmodifiedSince extends RequestHeader("if-unmodified-since")
  case MaxForwards extends RequestHeader("max-forwards")
  case Origin extends RequestHeader("origin")
  case Pragma extends RequestHeader("pragma")
  case Prefer extends RequestHeader("prefer")
  case ProxyAuthorization extends RequestHeader("proxy-authorization")
  case Range extends RequestHeader("range")
  case Referer extends RequestHeader("referer")
  case Te extends RequestHeader("te")
  case Trailer extends RequestHeader("trailer")
  case TransferEncoding extends RequestHeader("transfer-encoding")
  case UserAgent extends RequestHeader("user-agent")
  case Upgrade extends RequestHeader("upgrade")
  case Via extends RequestHeader("via")
  case Warning extends RequestHeader("warning")
  case NonStandard(name: String) extends RequestHeader(name.toLowerCase)

enum ResponseHeader(val header: String):
  case AcceptCh extends ResponseHeader("accept-ch")
  case AccessControlAllowOrigin extends ResponseHeader("access-control-allow-origin")
  case AccessControlAllowCredentials extends ResponseHeader("access-control-allow-credentials")
  case AccessControlExposeHeaders extends ResponseHeader("access-control-expose-headers")
  case AccessControlMaxAge extends ResponseHeader("access-control-max-age")
  case AccessControlAllowMethods extends ResponseHeader("access-control-allow-methods")
  case AccessControlAllowHeaders extends ResponseHeader("access-control-allow-headers")
  case AcceptPatch extends ResponseHeader("accept-patch")
  case AcceptRanges extends ResponseHeader("accept-ranges")
  case Age extends ResponseHeader("age")
  case Allow extends ResponseHeader("allow")
  case AltSvc extends ResponseHeader("alt-svc")
  case CacheControl extends ResponseHeader("cache-control")
  case Connection extends ResponseHeader("connection")
  case ContentDisposition extends ResponseHeader("content-disposition")
  case ContentEncoding extends ResponseHeader("content-encoding")
  case ContentLanguage extends ResponseHeader("content-language")
  case ContentLength extends ResponseHeader("content-length")
  case ContentLocation extends ResponseHeader("content-location")
  case ContentMd5 extends ResponseHeader("content-md5")
  case ContentRange extends ResponseHeader("content-range")
  case ContentType extends ResponseHeader("content-type")
  case Date extends ResponseHeader("date")
  case DeltaBase extends ResponseHeader("delta-base")
  case ETag extends ResponseHeader("etag")
  case Expires extends ResponseHeader("expires")
  case Im extends ResponseHeader("im")
  case LastModified extends ResponseHeader("last-modified")
  case Link extends ResponseHeader("link")
  case Location extends ResponseHeader("Location")
  case P3p extends ResponseHeader("p3p")
  case Pragma extends ResponseHeader("pragma")
  case PreferenceApplied extends ResponseHeader("preference-applied")
  case ProxyAuthenticate extends ResponseHeader("proxy-authenticate")
  case PublicKeyPins extends ResponseHeader("public-key-pins")
  case RetryAfter extends ResponseHeader("retry-after")
  case Server extends ResponseHeader("server")
  case SetCookie extends ResponseHeader("set-cookie")
  case StrictTransportSecurity extends ResponseHeader("strict-transport-security")
  case Trailer extends ResponseHeader("trailer")
  case TransferEncoding extends ResponseHeader("transfer-encoding")
  case Tk extends ResponseHeader("tk")
  case Upgrade extends ResponseHeader("upgrade")
  case Vary extends ResponseHeader("vary")
  case Via extends ResponseHeader("via")
  case Warning extends ResponseHeader("warning")
  case WwwAuthenticate extends ResponseHeader("www-authenticate")
  case XFrameOptions extends ResponseHeader("x-frame-options")
