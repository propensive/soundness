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
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package scintillate

// The five workloads every server in the real-socket comparison serves, all on one port.
// The request for each is serialized once, here, and the expected response body is held
// alongside it, so `HttpRivals.verify` can check a server's answers byte-for-byte before any
// row is measured.
//
//   plaintext  GET /bench                         → `Hello, World!`
//   json       GET /json                          → `{"message":"Hello, World!"}`, encoded
//                                                   per request by the server's own JSON library
//   large      GET /large                         → a precomputed 1 MiB body
//   echo       POST /echo (64 KiB body)           → the same bytes, echoed
//   route      GET /user/12345, X-Request-Id: 42  → `12345:42`, from the path capture and header
//
// The echo body is exactly scintillate `Reactor`'s inline-body limit (a body larger than it
// escalates to the thread-per-connection path), so the reactor row measures the inline path.
object HttpWorkload:
  val Plaintext: Int = 0
  val Json: Int = 1
  val Large: Int = 2
  val Echo: Int = 3
  val Route: Int = 4

  val count: Int = 5

  val hello: String = "Hello, World!"
  val greeting: String = """{"message":"Hello, World!"}"""
  val userId: String = "12345"
  val requestId: String = "42"
  val requestIdHeader: String = "X-Request-Id"

  private def ascii(text: String): scala.Array[Byte] = text.getBytes("US-ASCII").nn

  // A non-repeating-looking but deterministic fill, so a server that truncated or reordered
  // the body could not pass verification by accident.
  private def pattern(size: Int): scala.Array[Byte] =
    scala.Array.tabulate[Byte](size)(index => ((index*31) ^ (index >>> 7)).toByte)

  val largeBody: scala.Array[Byte] = pattern(1024*1024)
  val echoBody: scala.Array[Byte] = pattern(64*1024)

  private val headers: String = "Host: localhost\r\nUser-Agent: bench\r\n"

  val requests: scala.Array[scala.Array[Byte]] = scala.Array
    ( ascii(s"GET /bench HTTP/1.1\r\n${headers}Accept: text/plain\r\n\r\n"),
      ascii(s"GET /json HTTP/1.1\r\n${headers}Accept: application/json\r\n\r\n"),
      ascii(s"GET /large HTTP/1.1\r\n${headers}Accept: application/octet-stream\r\n\r\n"),
      ascii
        ( s"POST /echo HTTP/1.1\r\n${headers}Content-Type: application/octet-stream\r\n"
          + s"Content-Length: ${echoBody.length}\r\n\r\n" )
      ++ echoBody,
      ascii
        ( s"GET /user/$userId HTTP/1.1\r\n${headers}Accept: text/plain\r\n"
          + s"$requestIdHeader: $requestId\r\n\r\n" ) )

  val expected: scala.Array[scala.Array[Byte]] = scala.Array
    ( ascii(hello), ascii(greeting), largeBody, echoBody, ascii(s"$userId:$requestId") )

  val names: scala.Array[String] = scala.Array("plaintext", "json", "large", "echo", "route")
