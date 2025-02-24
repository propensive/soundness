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
┃    Soundness, version 0.27.0.                                                                    ┃
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

object Tests extends Suite(t"Telekinesis tests"):
  def run(): Unit =
    import internetAccess.enabled

    suite(t"Response construction tests"):
      test(t"Create a new HTTP response"):
        Http.Response(Http.Ok, contentType = media"application/json")(t"hello").tap: response =>
          println(response)

      . assert()

      test(t"Create a new HTTP response"):
        Http.Response(Http.Found)

      . assert()

      test(t"Create a new HTTP response"):
        Http.Response(contentType = media"image/jpeg")

      . assert()

    suite(t"Response construction tests"):
      test(t"Construct a Query"):
        erased given key: ("key" is Parameter of Text) = !!
        erased given param: ("param" is Parameter of Int) = !!
        Query(key = t"hello world", param = 24).show

      . assert(_ == t"key=hello+world&param=24")

      case class Person(name: Text, age: Int)
      case class Couple(first: Person, second: Person)

      test(t"Construct a Query by generic derivation"):
        Person(t"Jack", 12).query.show

      . assert(_ == t"name=Jack&age=12")

      test(t"Construct a Query by partial generic derivation"):
        import queryParameters.arbitrary
        Query(person = Person(t"Ken", 39)).show

      . assert(_ == t"person.name=Ken&person.age=39")

      test(t"Construct a Query by nested generic derivation"):
        Couple(Person(t"Jack", 12), Person(t"Jill", 11)).query.show

      . assert(_ == t"first.name=Jack&first.age=12&second.name=Jill&second.age=11")

      val query = Query.of(List
                            (t"first.name"  -> t"Jack",
                             t"first.age"   -> t"12",
                             t"second.name" -> t"Jill",
                             t"second.age"  -> t"11"))

      test(t"Dereference a query")(query.second)
      . assert(_ == Query.of(List(t"name" -> t"Jill", t"age"  -> t"11")))

      test(t"Decode a query"):
        summon[Couple is Decodable in Query].decoded(query)

      . assert(_ == Couple(Person(t"Jack", 12), Person(t"Jill", 11)))


    suite(t"Fetching tests"):

      test(t"Fetch a URL"):
        url"https://httpbin.org/post"
        . submit(Http.Post, contentEncoding = enc"UTF-8", accept = media"application/json")
        . apply(t"Hello world")
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
        capture[ConnectError](url"http://www.asorbkxoreuatoehudncak.com/".fetch())
        .tap(println(_))

      . assert(_ == ConnectError(ConnectError.Reason.Dns))

    suite(t"badssl.com SSL certificate tests"):
      import ConnectError.Reason.*, ConnectError.Reason.Ssl.Reason.*

      suite(t"Certificate Validation"):
        test(t"Expired SSL certificate"):
          capture[ConnectError](url"https://expired.badssl.com/".fetch())

        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"SSL certificate with wrong host"):
          capture[ConnectError](url"https://wrong.host.badssl.com/".fetch())

        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"Self-signed certificate"):
          capture[ConnectError](url"https://self-signed.badssl.com/".fetch())

        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"SSL certificate with untrusted root"):
          capture[ConnectError](url"https://untrusted-root.badssl.com/".fetch())

        . assert(_ == ConnectError(Ssl(Handshake)))

      suite(t"Interception Certificates"):
        test(t"superfish")(capture[ConnectError](url"https://superfish.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"edellroot")(capture[ConnectError](url"https://edellroot.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"dsdtestprovider")(capture[ConnectError](url"https://dsdtestprovider.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"preact-cli")(capture[ConnectError](url"https://preact-cli.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"webpack-dev-server")(capture[ConnectError](url"https://webpack-dev-server.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

      suite(t"Broken cryptography"):
        test(t"rc4")(capture[ConnectError](url"https://rc4.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"rc4-md5")(capture[ConnectError](url"https://rc4-md5.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"dh480")(capture[ConnectError](url"https://dh480.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"dh512")(capture[ConnectError](url"https://dh512.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"dh1024")(capture[ConnectError](url"https://dh1024.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"null")(capture[ConnectError](url"https://null.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"null")(capture[ConnectError](url"https://null.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

      suite(t"Legacy cryptography"):
        test(t"tls-v1-0")(capture[ConnectError](url"https://tls-v1-0.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"tls-v1-1")(capture[ConnectError](url"https://tls-v1-1.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"cbc")(capture[ConnectError](url"https://cbc.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"3des")(capture[ConnectError](url"https://3des.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"dh2048")(capture[ConnectError](url"https://dh2048.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

      suite(t"Domain Security Policies"):
        test(t"revoked")(capture[ConnectError](url"https://revoked.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"pinning-test")(capture[ConnectError](url"https://pinning-test.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))

        test(t"no-sct")(capture[ConnectError](url"https://no-sct.badssl.com/".fetch()))
        . assert(_ == ConnectError(Ssl(Handshake)))
