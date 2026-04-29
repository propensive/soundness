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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import soundness.*

import errorDiagnostics.stackTraces
import logging.silent
import strategies.throwUnsafely
import charEncoders.utf8

case class Address(house: Int, street: Text, city: Text, country: Text)
case class Person(name: Text, address: Address)

object Tests extends Suite(m"Telekinesis tests"):
  def run(): Unit =
    import internetAccess.enabled

    suite(m"Response construction tests"):
      test(m"Create HTTP response with status and content type"):
        Http.Response(Http.Ok, contentType = media"application/json")(t"hello")

      . assert()

      test(m"Create HTTP response with status only"):
        Http.Response(Http.Found)

      . assert()

      test(m"Create HTTP response with content type only"):
        Http.Response(contentType = media"image/jpeg")

      . assert()

    suite(m"Response construction tests"):
      test(m"Construct a Query"):
        inline given key: ("key" is Parametric to Text) = !!
        inline given param: ("param" is Parametric to Int) = !!
        Query.make(key = t"hello world", param = 24).show

      . assert(_ == t"key=hello+world&param=24")

      case class Person(name: Text, age: Int)
      case class Couple(first: Person, second: Person)

      test(m"Construct a Query by generic derivation"):
        Person(t"Jack", 12).query.show

      . assert(_ == t"name=Jack&age=12")

      test(m"Construct a Query by partial generic derivation"):
        // This import seems to be required
        import queryParameters.arbitrary
        Query.make(person = Person(t"Ken", 39)).show

      . assert(_ == t"person.name=Ken&person.age=39")

      test(m"Construct a Query by nested generic derivation"):
        Couple(Person(t"Jack", 12), Person(t"Jill", 11)).query.show

      . assert(_ == t"first.name=Jack&first.age=12&second.name=Jill&second.age=11")

      inline given second: ("second" is Parametric to Person) = !!

      val query = Query(List(t"first.name"  -> t"Jack",
                             t"first.age"   -> t"12",
                             t"second.name" -> t"Jill",
                             t"second.age"  -> t"11"))

      test(m"Dereference a query")(query.second)
      . assert(_ == Query(List(t"name" -> t"Jill", t"age"  -> t"11")))

      test(m"Decode a query"):
        summon[Couple is Decodable in Query].decoded(query)

      . assert(_ == Couple(Person(t"Jack", 12), Person(t"Jill", 11)))


    suite(m"Response parsing"):
      def chunks(text: Text, size: Int): Stream[Data] =
        val data: Data = text.data
        def go(offset: Int): Stream[Data] =
          if offset >= data.length then Stream() else
            val end = math.min(offset + size, data.length)
            data.slice(offset, end) #:: go(end)
        go(0)

      def bodyText(response: Http.Response): Text =
        response.body.stream.read[Data].utf8

      val blockSizes = List(1, 2, 3, 7, 13, 4096)

      suite(m"Status line"):
        val fixture = t"HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nbody"
        for blockSize <- blockSizes do
          test(m"Parse 200 OK at block size $blockSize"):
            Http.Response.parse(chunks(fixture, blockSize)).status

          . assert(_ == Http.Ok)

          test(m"Parse HTTP/1.1 version at block size $blockSize"):
            Http.Response.parse(chunks(fixture, blockSize)).version

          . assert(_ == 1.1)

          test(m"Parse body at block size $blockSize"):
            bodyText(Http.Response.parse(chunks(fixture, blockSize)))

          . assert(_ == t"body")

        for status <- List(Http.Continue, Http.Ok, Http.NoContent, Http.MovedPermanently,
                           Http.NotModified, Http.BadRequest, Http.NotFound,
                           Http.InternalServerError, Http.ServiceUnavailable) do
          test(m"Parse status code ${status.code}"):
            val fixture = t"HTTP/1.1 ${status.code} ${status.description}\r\n\r\n"
            Http.Response.parse(chunks(fixture, 4096)).status

          . assert(_ == status)

        test(m"Parse HTTP/1.0 version"):
          val fixture = t"HTTP/1.0 200 OK\r\n\r\n"
          Http.Response.parse(chunks(fixture, 4096)).version

        . assert(_ == 1.0)

        test(m"Parse status with long reason phrase"):
          val fixture = t"HTTP/1.1 503 Service Temporarily Unavailable, please try again later\r\n\r\n"
          Http.Response.parse(chunks(fixture, 4096)).status

        . assert(_ == Http.ServiceUnavailable)

      suite(m"Headers"):
        for blockSize <- blockSizes do
          test(m"Zero headers at block size $blockSize"):
            val fixture = t"HTTP/1.1 200 OK\r\n\r\nbody"
            Http.Response.parse(chunks(fixture, blockSize)).textHeaders

          . assert(_ == Nil)

          test(m"Single header at block size $blockSize"):
            val fixture = t"HTTP/1.1 200 OK\r\nX-Foo: bar\r\n\r\nbody"
            Http.Response.parse(chunks(fixture, blockSize)).textHeaders

          . assert(_ == List(Http.Header(t"X-Foo", t"bar")))

          test(m"Multiple headers at block size $blockSize"):
            val fixture =
              t"HTTP/1.1 200 OK\r\nX-Foo: a\r\nX-Bar: b\r\nX-Baz: c\r\n\r\nbody"
            Http.Response.parse(chunks(fixture, blockSize)).textHeaders

          . assert(_ == List(Http.Header(t"X-Foo", t"a"),
                             Http.Header(t"X-Bar", t"b"),
                             Http.Header(t"X-Baz", t"c")))

          test(m"Header value with leading whitespace at block size $blockSize"):
            val fixture = t"HTTP/1.1 200 OK\r\nX-Foo:    spacey\r\n\r\n"
            Http.Response.parse(chunks(fixture, blockSize)).textHeaders

          . assert(_ == List(Http.Header(t"X-Foo", t"spacey")))

          test(m"Header value with leading tab at block size $blockSize"):
            val fixture = t"HTTP/1.1 200 OK\r\nX-Foo:\tspacey\r\n\r\n"
            Http.Response.parse(chunks(fixture, blockSize)).textHeaders

          . assert(_ == List(Http.Header(t"X-Foo", t"spacey")))

        test(m"Header with hyphenated mixed-case name"):
          val fixture = t"HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n"
          Http.Response.parse(chunks(fixture, 4096)).textHeaders

        . assert(_ == List(Http.Header(t"Content-Type", t"text/html")))

        test(m"Long header value"):
          val long = t"a"*500
          val fixture = t"HTTP/1.1 200 OK\r\nX-Long: $long\r\n\r\n"
          Http.Response.parse(chunks(fixture, 4096)).textHeaders.head.value

        . assert(_ == t"a"*500)

        test(m"Duplicate header keys preserved"):
          val fixture = t"HTTP/1.1 200 OK\r\nX-Same: a\r\nX-Same: b\r\n\r\n"
          Http.Response.parse(chunks(fixture, 4096)).textHeaders

        . assert(_ == List(Http.Header(t"X-Same", t"a"), Http.Header(t"X-Same", t"b")))

      suite(m"Body"):
        for blockSize <- blockSizes do
          test(m"Empty body at block size $blockSize"):
            val fixture = t"HTTP/1.1 204 No Content\r\nContent-Length: 0\r\n\r\n"
            bodyText(Http.Response.parse(chunks(fixture, blockSize)))

          . assert(_ == t"")

          test(m"Multi-byte body at block size $blockSize"):
            val fixture = t"HTTP/1.1 200 OK\r\n\r\nHello, world!"
            bodyText(Http.Response.parse(chunks(fixture, blockSize)))

          . assert(_ == t"Hello, world!")

          test(m"Body containing CRLF at block size $blockSize"):
            val fixture = t"HTTP/1.1 200 OK\r\n\r\nline1\r\nline2\r\n"
            bodyText(Http.Response.parse(chunks(fixture, blockSize)))

          . assert(_ == t"line1\r\nline2\r\n")

          test(m"Long body at block size $blockSize"):
            val payload = (0 until 1000).map(i => (i%10).toString.tt).reduce(_ + _)
            val fixture = t"HTTP/1.1 200 OK\r\n\r\n$payload"
            bodyText(Http.Response.parse(chunks(fixture, blockSize)))

          . assert(_.length == 1000)

      suite(m"Malformed input"):
        test(m"Wrong protocol prefix raises Expectation"):
          capture[HttpResponseError]:
            Http.Response.parse(chunks(t"XTTP/1.1 200 OK\r\n\r\n", 4096))
          . reason

        . assert:
          case HttpResponseError.Reason.Expectation('H', 'X') => true
          case _                                              => false

        test(m"Non-digit in status code raises Status"):
          capture[HttpResponseError]:
            Http.Response.parse(chunks(t"HTTP/1.1 2X0 OK\r\n\r\n", 4096))
          . reason

        . assert:
          case HttpResponseError.Reason.Status(_) => true
          case _                                  => false

        test(m"Status code first digit out of range raises Status"):
          capture[HttpResponseError]:
            Http.Response.parse(chunks(t"HTTP/1.1 700 Weird\r\n\r\n", 4096))
          . reason

        . assert:
          case HttpResponseError.Reason.Status(_) => true
          case _                                  => false

      test(m"Fetch a URL"):
        url"https://httpbin.org/post"
        . submit(Http.Post, contentEncoding = enc"UTF-8", accept = media"application/json")
        . apply(t"Hello world")

      . assert()

      test(m"Fetch another URL without header names"):
        url"https://httpbin.org/post".submit(Http.Post, enc"UTF-8", accept = media"application/json")
          ( t"Hello world" )

      . assert()

      test(m"Fetch another URL with just a method"):
        url"https://httpbin.org/put".submit(Http.Put)
          ( t"Hello world" )

      . assert()

      test(m"Fetch another URL with defaults"):
        url"https://httpbin.org/post".submit()(t"Hello world")

      . assert()

    suite(m"DNS Errors"):
      test(m"Nonexistent DNS"):
        capture[ConnectError](url"http://www.asorbkxoreuatoehudncak.com/".fetch())

      . assert(_ == ConnectError(ConnectError.Reason.Dns))

    suite(m"badssl.com SSL certificate tests"):
      import ConnectError.Reason.*, ConnectError.Reason.Ssl.Reason.*

      suite(m"Certificate Validation"):
        test(m"Expired SSL certificate"):
          capture[ConnectError](url"https://expired.badssl.com/".fetch())

        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"SSL certificate with wrong host"):
          capture[ConnectError](url"https://wrong.host.badssl.com/".fetch())

        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"Self-signed certificate"):
          capture[ConnectError](url"https://self-signed.badssl.com/".fetch())

        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"SSL certificate with untrusted root"):
          capture[ConnectError](url"https://untrusted-root.badssl.com/".fetch())

        . assert(_ == ConnectError(Ssl(Handshake)))

      suite(m"Interception Certificates"):
        test(m"superfish")(capture[ConnectError](url"https://superfish.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"edellroot")(capture[ConnectError](url"https://edellroot.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"dsdtestprovider")(capture[ConnectError](url"https://dsdtestprovider.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"preact-cli")(capture[ConnectError](url"https://preact-cli.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"webpack-dev-server")(capture[ConnectError](url"https://webpack-dev-server.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

      suite(m"Broken cryptography"):
        test(m"rc4")(capture[ConnectError](url"https://rc4.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"rc4-md5")(capture[ConnectError](url"https://rc4-md5.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"dh480")(capture[ConnectError](url"https://dh480.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"dh512")(capture[ConnectError](url"https://dh512.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"dh1024")(capture[ConnectError](url"https://dh1024.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"null")(capture[ConnectError](url"https://null.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

      suite(m"Legacy cryptography"):
        test(m"tls-v1-0")(capture[ConnectError](url"https://tls-v1-0.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"tls-v1-1")(capture[ConnectError](url"https://tls-v1-1.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"cbc")(capture[ConnectError](url"https://cbc.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"3des")(capture[ConnectError](url"https://3des.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"dh2048")(capture[ConnectError](url"https://dh2048.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

      suite(m"Domain Security Policies"):
        test(m"revoked")(capture[ConnectError](url"https://revoked.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"pinning-test")(capture[ConnectError](url"https://pinning-test.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(m"no-sct")(capture[ConnectError](url"https://no-sct.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))
