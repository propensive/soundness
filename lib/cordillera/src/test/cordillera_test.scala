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
package cordillera

import soundness.*

import strategies.throwUnsafely

object Tests extends Suite(m"Cordillera HTTP/2 Tests"):
  def run(): Unit =
    def bytes(hex: Text): Data =
      IArray.from(hex.s.grouped(2).map(Integer.parseInt(_, 16).toByte).to(List))

    def hex(data: Data): Text =
      data.to(List).map(b => String.format("%02x", (b & 0xff): Integer).nn).mkString.tt

    def ascii(text: Text): Data = IArray.from(text.s.getBytes("US-ASCII").nn.to(List))

    suite(m"Huffman (RFC 7541 Appendix C)"):
      // C.4.1: "www.example.com" → Huffman
      test(m"encode www.example.com"):
        hex(Huffman.encode(ascii(t"www.example.com")))
      . assert(_ == t"f1e3c2e5f23a6ba0ab90f4ff")

      test(m"decode www.example.com"):
        Huffman.decode(bytes(t"f1e3c2e5f23a6ba0ab90f4ff")).to(List)
      . assert(_ == ascii(t"www.example.com").to(List))

      // C.4.2: "no-cache"
      test(m"encode no-cache"):
        hex(Huffman.encode(ascii(t"no-cache")))
      . assert(_ == t"a8eb10649cbf")

      test(m"decode no-cache"):
        Huffman.decode(bytes(t"a8eb10649cbf")).to(List)
      . assert(_ == ascii(t"no-cache").to(List))

      // C.4.3: "custom-key" and "custom-value"
      test(m"encode custom-key"):
        hex(Huffman.encode(ascii(t"custom-key")))
      . assert(_ == t"25a849e95ba97d7f")

      test(m"encode custom-value"):
        hex(Huffman.encode(ascii(t"custom-value")))
      . assert(_ == t"25a849e95bb8e8b4bf")

      // C.6.1: "302" status, "private", a date, and a URL — exercises digits + EOS pad
      test(m"encode 302"):
        hex(Huffman.encode(ascii(t"302")))
      . assert(_ == t"6402")

      test(m"encode private"):
        hex(Huffman.encode(ascii(t"private")))
      . assert(_ == t"aec3771a4b")

      test(m"decode private"):
        Huffman.decode(bytes(t"aec3771a4b")).to(List)
      . assert(_ == ascii(t"private").to(List))

      test(m"round-trip a long date string"):
        val date = ascii(t"Mon, 21 Oct 2013 20:13:21 GMT")
        Huffman.decode(Huffman.encode(date)).to(List) == date.to(List)
      . assert(_ == true)

      test(m"round-trip all 256 byte values"):
        val every = IArray.from((0 until 256).map(_.toByte))
        Huffman.decode(Huffman.encode(every)).to(List) == every.to(List)
      . assert(_ == true)

    suite(m"HPACK decode (RFC 7541 Appendix C.3 — without Huffman)"):
      // A single decoder fed the three successive request header blocks from C.3,
      // sharing one dynamic table across requests (the point of the example).
      val hpack = Hpack()

      def fields(hex: Text): List[(Text, Text)] =
        hpack.decode(bytes(hex)).map(e => (e.name, e.value))

      test(m"C.3.1 first request"):
        fields(t"828684410f7777772e6578616d706c652e636f6d")
      . assert(_ == List((t":method", t"GET"), (t":scheme", t"http"), (t":path", t"/"),
          (t":authority", t"www.example.com")))

      test(m"C.3.2 second request (uses dynamic table + new header)"):
        fields(t"828684be58086e6f2d6361636865")
      . assert(_ == List((t":method", t"GET"), (t":scheme", t"http"), (t":path", t"/"),
          (t":authority", t"www.example.com"), (t"cache-control", t"no-cache")))

      test(m"C.3.3 third request (custom header)"):
        fields(t"828785bf400a637573746f6d2d6b65790c637573746f6d2d76616c7565")
      . assert(_ == List((t":method", t"GET"), (t":scheme", t"https"), (t":path", t"/index.html"),
          (t":authority", t"www.example.com"), (t"custom-key", t"custom-value")))

    suite(m"HPACK decode (RFC 7541 Appendix C.4 — with Huffman)"):
      val hpack = Hpack()

      def fields(hex: Text): List[(Text, Text)] =
        hpack.decode(bytes(hex)).map(e => (e.name, e.value))

      test(m"C.4.1 first request, Huffman-coded authority"):
        fields(t"828684418cf1e3c2e5f23a6ba0ab90f4ff")
      . assert(_ == List((t":method", t"GET"), (t":scheme", t"http"), (t":path", t"/"),
          (t":authority", t"www.example.com")))

      test(m"C.4.2 second request, Huffman-coded no-cache"):
        fields(t"828684be5886a8eb10649cbf")
      . assert(_ == List((t":method", t"GET"), (t":scheme", t"http"), (t":path", t"/"),
          (t":authority", t"www.example.com"), (t"cache-control", t"no-cache")))

    suite(m"HPACK encode → decode round-trip"):
      val headers = List(
        HpackEntry(t":method", t"POST"),
        HpackEntry(t":scheme", t"http"),
        HpackEntry(t":path", t"/foo/bar"),
        HpackEntry(t":authority", t"unix"),
        HpackEntry(t"content-type", t"application/grpc"),
        HpackEntry(t"te", t"trailers"))

      test(m"a request's pseudo-headers + headers survive a round-trip"):
        val encoded = Hpack().encode(headers)
        Hpack().decode(encoded).map(e => (e.name, e.value))
      . assert(_ == headers.map(e => (e.name, e.value)))
