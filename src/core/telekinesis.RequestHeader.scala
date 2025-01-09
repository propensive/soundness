/*
    Telekinesis, version 0.25.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import spectacular.*
import vacuous.*

trait RequestHeader[LabelType <: Label]():
  def header: Text

  def apply[ValueType](content: ValueType)(using param: LabelType is GenericHttpRequestParam[ValueType])
          : RequestHeader.Value =

    RequestHeader.Value(this, param(content))

object RequestHeader:
  lazy val standard: Map[Text, RequestHeader[?]] = Set(AIm, Accept, AcceptCh, AcceptDatetime,
      AcceptEncoding, AcceptLanguage, AccessControlRequestMethod, AccessControlRequestHeaders,
      Authorization, CacheControl, Connection, ContentEncoding, ContentLength, ContentMd5,
      ContentType, Cookie, Date, Expect, Forwarded, From, Host, Http2Settings, IfMatch,
      IfModifiedSince, IfNoneMatch, IfRange, IfUnmodifiedSince, MaxForwards, Origin, Pragma, Prefer,
      ProxyAuthorization, Range, Referer, Te, Trailer, TransferEncoding, UserAgent, Upgrade, Via,
      Warning).bi.map(_.header -> _).to(Map)

  def parse(text: Text): RequestHeader[?] = standard.at(text.lower).or(RequestHeader(text.s))

  def unapply(str: Text): Some[RequestHeader[?]] =
    Some(standard.get(str.lower).getOrElse(RequestHeader(str.s)))

  object Value:
    given Value is Showable = value => t"${value.header}: ${value.value}"

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

  given RequestHeader[?] is Showable = _.header

  def apply(paramName: String): RequestHeader[paramName.type] =
    new RequestHeader[paramName.type]():
      def header: Text = Text(paramName)

  case object AIm extends SimpleRequestHeader["a-im"]()
  case object Accept extends SimpleRequestHeader["accept"]()
  case object AcceptCh extends SimpleRequestHeader["accept-ch"]()
  case object AcceptDatetime extends SimpleRequestHeader["accept-datetime"]()
  case object AcceptEncoding extends SimpleRequestHeader["accept-encoding"]()
  case object AcceptLanguage extends SimpleRequestHeader["accept-language"]()

  case object AccessControlRequestMethod
  extends SimpleRequestHeader["access-control-request-method"]()

  case object AccessControlRequestHeaders
  extends SimpleRequestHeader["access-control-request-headers"]()

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
