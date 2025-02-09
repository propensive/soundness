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

import soundness.*

import errorDiagnostics.stackTraces
import logging.silent
import strategies.throwUnsafely

case class Address(house: Int, street: Text, city: Text, country: Text)
case class Person(name: Text, address: Address)

object Tests extends Suite(t"Telekinesis tests"):
  def run(): Unit =
    import internetAccess.enabled
    suite(t"Fetching tests"):

      test(t"Fetch a URL"):
        url"https://httpbin.org/post".submit
         (Http.Post,
          contentEncoding = enc"UTF-8",
          accept          = media"application/json")
         (t"Hello world")

        . tap: response =>
          println(response)
          unsafely:
            println(response.receive[Text])

      . assert()

      test(t"Fetch another URL without header names"):
        url"https://httpbin.org/post".submit(Http.Post, enc"UTF-8", accept = media"application/json")
         (t"Hello world")

        . tap: response =>
          println(response)
          unsafely:
            println(response.receive[Text])

      . assert()

      test(t"Fetch another URL with just a method"):
        url"https://httpbin.org/put".submit(Http.Put)
         (t"Hello world")

        . tap: response =>
          println(response)
          unsafely:
            println(response.receive[Text])

      . assert()

      test(t"Fetch another URL with defaults"):
        url"https://httpbin.org/post".submit()(t"Hello world")

        . tap: response =>
          println(response)
          unsafely:
            println(response.receive[Text])

      . assert()

    suite(t"DNS Errors"):
      test(t"Nonexistent DNS"):
        capture[TcpError](url"http://www.asorbkxoreuatoehudncak.com/".fetch())
        .tap(println(_))

      . assert(_ == TcpError(TcpError.Reason.Dns))

    suite(t"badssl.com SSL certificate tests"):
      import TcpError.Reason.*, TcpError.Reason.Ssl.Reason.*

      suite(t"Certificate Validation"):
        test(t"Expired SSL certificate"):
          capture[TcpError](url"https://expired.badssl.com/".fetch())

        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"SSL certificate with wrong host"):
          capture[TcpError](url"https://wrong.host.badssl.com/".fetch())

        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"Self-signed certificate"):
          capture[TcpError](url"https://self-signed.badssl.com/".fetch())

        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"SSL certificate with untrusted root"):
          capture[TcpError](url"https://untrusted-root.badssl.com/".fetch())

        . assert(_ == TcpError(Ssl(Handshake)))

      suite(t"Interception Certificates"):
        test(t"superfish")(capture[TcpError](url"https://superfish.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"edellroot")(capture[TcpError](url"https://edellroot.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"dsdtestprovider")(capture[TcpError](url"https://dsdtestprovider.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"preact-cli")(capture[TcpError](url"https://preact-cli.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"webpack-dev-server")(capture[TcpError](url"https://webpack-dev-server.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

      suite(t"Broken cryptography"):
        test(t"rc4")(capture[TcpError](url"https://rc4.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"rc4-md5")(capture[TcpError](url"https://rc4-md5.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"dh480")(capture[TcpError](url"https://dh480.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"dh512")(capture[TcpError](url"https://dh512.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"dh1024")(capture[TcpError](url"https://dh1024.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"null")(capture[TcpError](url"https://null.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"null")(capture[TcpError](url"https://null.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

      suite(t"Legacy cryptography"):
        test(t"tls-v1-0")(capture[TcpError](url"https://tls-v1-0.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"tls-v1-1")(capture[TcpError](url"https://tls-v1-1.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"cbc")(capture[TcpError](url"https://cbc.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"3des")(capture[TcpError](url"https://3des.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"dh2048")(capture[TcpError](url"https://dh2048.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

      suite(t"Domain Security Policies"):
        test(t"revoked")(capture[TcpError](url"https://revoked.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"pinning-test")(capture[TcpError](url"https://pinning-test.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))

        test(t"no-sct")(capture[TcpError](url"https://no-sct.badssl.com/".fetch()))
        . assert(_ == TcpError(Ssl(Handshake)))
