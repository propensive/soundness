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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package coaxial

import java.io as ji

import scala.scalanative.unsafe.*

// TLS setup failures surface as `IOException`s, matching what the JVM `SecureEndpoint`'s
// `SSLSocket` calls throw at runtime; `Connectable.connect` offers no typed error channel.
import unsafeExceptions.canThrowAny

import anticipation.*
import gigantism.*
import urticose.*
import vacuous.*

// A secure (TLS) TCP endpoint — a `host:port` reached over TLS, the counterpart of the
// plaintext `Endpoint[TcpPort]`, mirroring the JVM `SecureEndpoint`. Scala Native has no
// `javax.net.ssl`, so its `Connectable` speaks to OpenSSL directly through `extern` calls:
// an `SSL_CTX` trusting the system default certificate store, a *connect BIO* — OpenSSL's
// own resolve-connect-handshake state machine, which owns the underlying socket — with SNI
// set to `host`, hostname verification (`SSL_set1_host`) unless `Tls.verify` is off, and
// the handshaken BIO presented as a `Duplex` over `BIO_read`/`BIO_write`.
//
// The OpenSSL "functions" that are really C macros (`BIO_do_connect`, `BIO_get_ssl`,
// `SSL_set_tlsext_host_name`, …) have no linkable symbols; each is re-expressed here as the
// `BIO_ctrl`/`SSL_ctrl` call the macro expands to, with the command constants restated below.
object SecureEndpoint:
  // Honestly tracked, like `Connectable.tcpEndpoint`: the instance is resolvable only with
  // `Online` permission, so it is a capability carrying that evidence in its capture set.
  given connectable: (online: Online)
  =>  ( options: Every[SocketOption.Tcp], tls: Tls )
  =>  ( (SecureEndpoint is Connectable)^{online, caps.any} ) =

    new Connectable:
      type Self = SecureEndpoint

      def connect(endpoint: SecureEndpoint, interface: Optional[MacAddress]): Duplex =
        // A connect BIO resolves and opens its socket internally, so neither a local-interface
        // bind nor per-socket `options` can be applied; both are ignored (as `listenUdp`
        // ignores `interface`).
        val nullPtr = null.asInstanceOf[Ptr[Byte]]
        val context = libssl.SSL_CTX_new(libssl.TLS_client_method())
        if context == null then throw ji.IOException("TLS: SSL_CTX_new failed: "+opensslError())

        try
          if libssl.SSL_CTX_set_default_verify_paths(context) != 1
          then throw ji.IOException("TLS: no default certificate store: "+opensslError())

          val mode = if tls.verify then SSL_VERIFY_PEER else SSL_VERIFY_NONE
          libssl.SSL_CTX_set_verify(context, mode, nullPtr)

          val bio = libssl.BIO_new_ssl_connect(context)

          if bio == null
          then throw ji.IOException("TLS: BIO_new_ssl_connect failed: "+opensslError())

          try
            val sslHolder = stackalloc[Ptr[Byte]]()
            libcrypto.BIO_ctrl(bio, BIO_C_GET_SSL, 0L, sslHolder.asInstanceOf[Ptr[Byte]])
            val ssl = !sslHolder
            if ssl == null then throw ji.IOException("TLS: BIO_get_ssl failed: "+opensslError())

            Zone.acquire: zone =>
              val host = toCString(endpoint.host.s)(using zone)

              // SNI lets the server select its certificate (OpenSSL copies the name)...
              libssl.SSL_ctrl
                ( ssl, SSL_CTRL_SET_TLSEXT_HOSTNAME, TLSEXT_NAMETYPE_host_name,
                  host.asInstanceOf[Ptr[Byte]] )

              // ...and `SSL_set1_host` then checks that certificate against the hostname
              // during the handshake (skipped only when verification is off).
              if tls.verify && libssl.SSL_set1_host(ssl, host) != 1
              then throw ji.IOException("TLS: SSL_set1_host failed: "+opensslError())

              val target = toCString(endpoint.host.s+":"+endpoint.port.toString)(using zone)
              libcrypto.BIO_ctrl(bio, BIO_C_SET_CONNECT, 0L, target.asInstanceOf[Ptr[Byte]])

            // Resolve, connect and handshake in one step; with `SSL_VERIFY_PEER` set, an
            // untrusted or mismatched certificate fails here.
            if libcrypto.BIO_ctrl(bio, BIO_C_DO_STATE_MACHINE, 0L, nullPtr) <= 0 then
              val where = endpoint.host.s+":"+endpoint.port.toString
              throw ji.IOException("TLS: connection to "+where+" failed: "+opensslError())

            bioDuplex(bio, context)

          catch case error: Throwable =>
            libcrypto.BIO_free_all(bio)
            throw error

        catch case error: Throwable =>
          libssl.SSL_CTX_free(context)
          throw error

case class SecureEndpoint(host: Text, port: Int)
