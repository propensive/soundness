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

import rudiments.*
import anticipation.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8
import gastronomy.*
import spectacular.*

trait RequestHeader[LabelType <: Label]():
  def header: Text
  
  def apply
      [ValueType]
      (content: ValueType)
      (using ghrp: GenericHttpRequestParam[LabelType, ValueType])
      : RequestHeader.Value =
    RequestHeader.Value(this, ghrp(content))

class SimpleRequestHeader[LabelType <: Label: ValueOf]() extends RequestHeader[LabelType]():
  def header: Text = Text(summon[ValueOf[LabelType]].value)

object RequestHeader:
  lazy val standard: Map[Text, RequestHeader[?]] = Set(AIm, Accept, AcceptCh, AcceptDatetime,
      AcceptEncoding, AcceptLanguage, AccessControlRequestMethod, AccessControlRequestHeaders,
      Authorization, CacheControl, Connection, ContentEncoding, ContentLength, ContentMd5,
      ContentType, Cookie, Date, Expect, Forwarded, From, Host, Http2Settings, IfMatch,
      IfModifiedSince, IfNoneMatch, IfRange, IfUnmodifiedSince, MaxForwards, Origin, Pragma, Prefer,
      ProxyAuthorization, Range, Referer, Te, Trailer, TransferEncoding, UserAgent, Upgrade, Via,
      Warning).bi.map(_.header -> _).to(Map)

  def unapply(str: Text): Some[RequestHeader[?]] =
    Some(standard.get(str.lower).getOrElse(RequestHeader(str.s)))
  
  object Value:
    given Show[Value] = value => t"${value.header}: ${value.value}"

  case class Value(header: RequestHeader[?], value: Text)

  object nonStandard:
    case object UpgradeInsecureRequests extends SimpleRequestHeader["upgrade-insecure-requests"]()
    case object XRequestedWith extends SimpleRequestHeader["x-requested-with"]()
    case object Dnt extends SimpleRequestHeader["dnt"]()
    case object XForwardedFor extends SimpleRequestHeader["x-forwarded-for"]()
    case object XForwardedHost extends SimpleRequestHeader["x-forwarded-host"]()
    case object XForwardedProto extends SimpleRequestHeader["x-forwarded-proto"]()
    case object FrontEndHttps extends SimpleRequestHeader["front-end-https"]()
    case object XHttpMethodOverride extends SimpleRequestHeader["x-http-method-override"]()
    case object XattDeviceId extends SimpleRequestHeader["x-att-deviceid"]()
    case object XWapProfile extends SimpleRequestHeader["x-wap-profile"]()
    case object ProxyConnection extends SimpleRequestHeader["proxy-connection"]()
    case object Xuidh extends SimpleRequestHeader["x-uidh"]()
    case object XCsrfToken extends SimpleRequestHeader["x-csrf-token"]()
    case object XSimpleRequestId extends SimpleRequestHeader["x-request-id"]()
    case object XCorrelationId extends SimpleRequestHeader["x-correlation-id"]()
    case object SaveData extends SimpleRequestHeader["save-data"]()

  given Show[RequestHeader[?]] = _.header

  def apply(paramName: String): RequestHeader[paramName.type] =
    new RequestHeader[paramName.type]():
      def header: Text = Text(paramName)

  case object AIm extends SimpleRequestHeader["a-im"]()
  case object Accept extends SimpleRequestHeader["accept"]()
  case object AcceptCh extends SimpleRequestHeader["accept-ch"]()
  case object AcceptDatetime extends SimpleRequestHeader["accept-datetime"]()
  case object AcceptEncoding extends SimpleRequestHeader["accept-encoding"]()
  case object AcceptLanguage extends SimpleRequestHeader["accept-language"]()
  case object AccessControlRequestMethod extends SimpleRequestHeader["access-control-request-method"]()
  case object AccessControlRequestHeaders extends SimpleRequestHeader["access-control-request-headers"]()
  case object Authorization extends SimpleRequestHeader["authorization"]()
  case object CacheControl extends SimpleRequestHeader["cache-control"]()
  case object Connection extends SimpleRequestHeader["connection"]()
  case object ContentEncoding extends SimpleRequestHeader["content-encoding"]()
  case object ContentLength extends SimpleRequestHeader["content-length"]()
  case object ContentMd5 extends SimpleRequestHeader["content-md5"]()
  case object ContentType extends SimpleRequestHeader["content-type"]()
  case object Cookie extends SimpleRequestHeader["cookie"]()
  case object Date extends SimpleRequestHeader["date"]()
  case object Expect extends SimpleRequestHeader["expect"]()
  case object Forwarded extends SimpleRequestHeader["forwarded"]()
  case object From extends SimpleRequestHeader["from"]()
  case object Host extends SimpleRequestHeader["host"]()
  case object Http2Settings extends SimpleRequestHeader["http2-settings"]()
  case object IfMatch extends SimpleRequestHeader["if-match"]()
  case object IfModifiedSince extends SimpleRequestHeader["if-modified-since"]()
  case object IfNoneMatch extends SimpleRequestHeader["if-none-match"]()
  case object IfRange extends SimpleRequestHeader["if-range"]()
  case object IfUnmodifiedSince extends SimpleRequestHeader["if-unmodified-since"]()
  case object MaxForwards extends SimpleRequestHeader["max-forwards"]()
  case object Origin extends SimpleRequestHeader["origin"]()
  case object Pragma extends SimpleRequestHeader["pragma"]()
  case object Prefer extends SimpleRequestHeader["prefer"]()
  case object ProxyAuthorization extends SimpleRequestHeader["proxy-authorization"]()
  case object Range extends SimpleRequestHeader["range"]()
  case object Referer extends SimpleRequestHeader["referer"]()
  case object Te extends SimpleRequestHeader["te"]()
  case object Trailer extends SimpleRequestHeader["trailer"]()
  case object TransferEncoding extends SimpleRequestHeader["transfer-encoding"]()
  case object UserAgent extends SimpleRequestHeader["user-agent"]()
  case object Upgrade extends SimpleRequestHeader["upgrade"]()
  case object Via extends SimpleRequestHeader["via"]()
  case object Warning extends SimpleRequestHeader["warning"]()
  
object ResponseHeader:
  lazy val standard: Map[Text, ResponseHeader] = List(AcceptCharset, AccessControlAllowOrigin,
      AccessControlAllowCredentials, AccessControlExposeHeaders, AccessControlMaxAge,
      AccessControlAllowMethods, AccessControlAllowHeaders, AcceptPatch, AcceptRanges, Age, Allow,
      AltSvc, CacheControl, Connection, ContentDisposition, ContentEncoding, ContentLanguage,
      ContentLength, ContentLocation, ContentMd5, ContentRange, ContentType, Date, DeltaBase, ETag,
      Expires, Im, LastModified, Link, Location, P3p, Pragma, PreferenceApplied, ProxyAuthenticate,
      PublicKeyPins, RetryAfter, Server, SetCookie, StrictTransportSecurity, Trailer,
      TransferEncoding, Tk, Upgrade, Vary, Via, Warning, WwwAuthenticate, XFrameOptions)
    .bi.map(_.header -> _).to(Map)
  
  def unapply(str: Text): Some[ResponseHeader] =
    Some(standard.get(str.lower).getOrElse(Custom(str)))

  given Show[ResponseHeader] = _.header

enum ResponseHeader(val header: Text):
  case AcceptCharset extends ResponseHeader(t"accept-charset")
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
    case Basic(username, password) => t"Basic ${t"$username:$password".bytes.encodeAs[Base64]}"
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
