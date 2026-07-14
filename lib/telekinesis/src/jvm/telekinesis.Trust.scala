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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import java.security as js
import java.security.cert as jsc
import javax.net.ssl as jns

import anticipation.*
import fulminate.*
import gastronomy.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

// Contextual TLS acceptance criteria (#676). A `TlsAcceptance` given
// determines which peer configurations a connection will accept, mirroring
// the progressive-deprecation model of gastronomy's cryptographic permits:
// the strict default needs no permission, while each relaxation is
// constructed only under an erased `Permit` for the corresponding
// `Concession`, so a codebase's tolerated weaknesses are auditable from its
// permit imports alone, at no runtime cost.
//
// Certificate-level criteria (trust anchors, expiry, self-signature,
// hostname verification, revocation) are enforced per-connection through a
// wrapping trust manager and `SSLParameters`. Protocol-level re-enablement
// (TLS 1.0/1.1, small key sizes) is different: JSSE treats per-connection
// algorithm constraints as purely additional to the process-wide
// `jdk.tls.disabledAlgorithms` security property, so those relaxations can
// only be unlocked process-wide, once, before JSSE initializes — see
// `Trust.unlock`.
object Trust:
  enum Version:
    case Tls12, Tls13
    case Tls10, Tls11

    def id: Text = this match
      case Tls10 => t"TLSv1"
      case Tls11 => t"TLSv1.1"
      case Tls12 => t"TLSv1.2"
      case Tls13 => t"TLSv1.3"

  enum Revocation:
    case Required, SoftFail, Unchecked

  val Strict: Trust = Trust()

  // Process-wide re-enablement of deprecated protocol versions and key sizes.
  // Must run before any JSSE machinery is initialized; later calls have no
  // effect on already-created contexts. Requires the corresponding permits.
  def unlock(versions: List[Version])
    ( using erased tls10: Permit[Concession.Tls10], erased tls11: Permit[Concession.Tls11] )
  :   Unit =

    val current: String =
      js.Security.getProperty("jdk.tls.disabledAlgorithms") match
        case null           => ""
        case value: String  => value

    val retained = current.split(",").nn.map(_.nn.trim.nn).filter: entry =>
      !versions.exists(_.id.s == entry)

    js.Security.setProperty("jdk.tls.disabledAlgorithms", String.join(", ", retained*))

// Which certificate chains to trust beyond an intact, current chain to a
// platform anchor.
case class Trust
  ( expired:    Boolean = false,
    selfSigned: Boolean = false,
    hostname:   Boolean = true,
    anchors:    List[jsc.X509Certificate] = Nil )

// The runtime acceptance value, resolved contextually wherever a TLS
// connection is made. The companion default is strict; relaxed instances are
// built with the named constructors, each gated on its permit.
object TlsAcceptance:
  given strict: TlsAcceptance = TlsAcceptance()

  // Materialize the JSSE artefacts for this acceptance. Cached by the
  // transport layer, keyed on the acceptance value.
  extension (acceptance: TlsAcceptance)
    def materialize(): (jns.SSLContext, jns.SSLParameters) =
      // The strict default is exactly the platform's own behaviour; only a
      // relaxed acceptance pays for a custom trust manager.
      val context =
        if acceptance == TlsAcceptance() then jns.SSLContext.getDefault.nn else
          val custom = jns.SSLContext.getInstance("TLS").nn
          custom.init(null, Array(trustManager(acceptance)), null)
          custom

      val parameters = context.getDefaultSSLParameters().nn

      if acceptance.versions != Nil then
        parameters.setProtocols(acceptance.versions.map(_.id.s).toArray)

      if acceptance.trust.hostname
      then parameters.setEndpointIdentificationAlgorithm("HTTPS")
      else parameters.setEndpointIdentificationAlgorithm(null)

      (context, parameters)

  private def platformManager(anchors: List[jsc.X509Certificate])
  :   jns.X509ExtendedTrustManager =

    val keystore: js.KeyStore | Null =
      if anchors == Nil then null else
        val store = js.KeyStore.getInstance(js.KeyStore.getDefaultType.nn).nn
        store.load(null, null)

        anchors.zipWithIndex.each: (anchor, index) =>
          store.setCertificateEntry(s"anchor-$index", anchor)

        store

    val factory =
      jns.TrustManagerFactory.getInstance(jns.TrustManagerFactory.getDefaultAlgorithm.nn).nn

    factory.init(keystore)

    factory.getTrustManagers.nn.collectFirst:
      case manager: jns.X509ExtendedTrustManager => manager

    . getOrElse(panic(m"the platform provides no X.509 trust manager"))

  // Wraps the platform trust manager, downgrading precisely the failure
  // causes this acceptance tolerates: an expired chain is retried against
  // validity-blind parameters only when `expired` is set, and an unresolvable
  // anchor is tolerated only when `selfSigned` is set. All other causes
  // (revocation per its own setting, malformed chains, wrong key usage)
  // propagate unchanged.
  private def trustManager(acceptance: TlsAcceptance): jns.X509ExtendedTrustManager =
    val platform = platformManager(acceptance.trust.anchors)

    def tolerable(error: Throwable | Null): Boolean = error match
      case null =>
        false

      case error: jsc.CertificateExpiredException =>
        acceptance.trust.expired

      case error: jsc.CertPathValidatorException =>
        error.getReason match
          case jsc.CertPathValidatorException.BasicReason.EXPIRED =>
            acceptance.trust.expired

          case jsc.CertPathValidatorException.BasicReason.REVOKED =>
            acceptance.revocation != Trust.Revocation.Required

          case jsc.CertPathValidatorException.BasicReason.UNDETERMINED_REVOCATION_STATUS =>
            acceptance.revocation == Trust.Revocation.Unchecked ||
              acceptance.revocation == Trust.Revocation.SoftFail

          case _ =>
            tolerable(error.getCause)

      case error: java.security.InvalidAlgorithmParameterException =>
        // "the trustAnchors parameter must be non-empty" and kin: no path to
        // any anchor — the self-signed case.
        acceptance.trust.selfSigned

      case error =>
        error.getMessage match
          case message: String
            if message.contains("unable to find valid certification path") =>
            acceptance.trust.selfSigned

          case _ =>
            tolerable(error.getCause)

    new jns.X509ExtendedTrustManager:
      import unsafeExceptions.canThrowAny

      private def attempt(check: => Unit): Unit =
        try check catch
          case error: jsc.CertificateException =>
            if !tolerable(error) then throw error

      def getAcceptedIssuers(): Array[jsc.X509Certificate | Null] | Null =
        platform.getAcceptedIssuers()

      def checkClientTrusted
        ( chain: Array[jsc.X509Certificate | Null] | Null, authType: String | Null )
      :   Unit =

        platform.checkClientTrusted(chain, authType)

      def checkClientTrusted
        ( chain:    Array[jsc.X509Certificate | Null] | Null,
          authType: String | Null,
          socket:   java.net.Socket | Null )
      :   Unit =

        platform.checkClientTrusted(chain, authType, socket)

      def checkClientTrusted
        ( chain:    Array[jsc.X509Certificate | Null] | Null,
          authType: String | Null,
          engine:   jns.SSLEngine | Null )
      :   Unit =

        platform.checkClientTrusted(chain, authType, engine)

      def checkServerTrusted
        ( chain: Array[jsc.X509Certificate | Null] | Null, authType: String | Null )
      :   Unit =

        attempt(platform.checkServerTrusted(chain, authType))

      def checkServerTrusted
        ( chain:    Array[jsc.X509Certificate | Null] | Null,
          authType: String | Null,
          socket:   java.net.Socket | Null )
      :   Unit =

        attempt(platform.checkServerTrusted(chain, authType, socket))

      def checkServerTrusted
        ( chain:    Array[jsc.X509Certificate | Null] | Null,
          authType: String | Null,
          engine:   jns.SSLEngine | Null )
      :   Unit =

        attempt(platform.checkServerTrusted(chain, authType, engine))

case class TlsAcceptance
  ( versions:   List[Trust.Version] = Nil, // Nil selects the platform default set
    trust:      Trust = Trust.Strict,
    revocation: Trust.Revocation = Trust.Revocation.SoftFail ):

  def permitExpired(using erased permit: Permit[Concession.ExpiredCertificate]): TlsAcceptance =
    copy(trust = trust.copy(expired = true))

  def permitSelfSigned(using erased permit: Permit[Concession.SelfSignedCertificate]): TlsAcceptance =
    copy(trust = trust.copy(selfSigned = true))

  def permitHostnameMismatch(using erased permit: Permit[Concession.UnverifiedHostname]): TlsAcceptance =
    copy(trust = trust.copy(hostname = false))

  def requireRevocationChecks: TlsAcceptance = copy(revocation = Trust.Revocation.Required)

  def permitRevoked(using erased permit: Permit[Concession.UncheckedRevocation]): TlsAcceptance =
    copy(revocation = Trust.Revocation.Unchecked)

  def trusting(anchors: List[jsc.X509Certificate]): TlsAcceptance =
    copy(trust = trust.copy(anchors = anchors))
