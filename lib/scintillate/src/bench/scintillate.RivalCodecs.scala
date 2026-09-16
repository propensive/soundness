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

import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.handler.codec.http as netty
import io.netty.util.ReferenceCountUtil
import org.eclipse.jetty.http as jetty
import org.eclipse.jetty.util.BufferUtil

import java.nio.ByteBuffer

// The in-memory wire-codec rows' rivals: the request-head parsers and response
// serializers of Netty (which zio-http and Vert.x also run on) and Jetty, fed the same
// request bytes and asked for the same `text/plain` response as scintillate's rows. Each
// codec is built once and reset between operations, as its own server does per connection;
// a benchmark runs single-threaded, so the codecs need no confinement.
object RivalCodecs:
  import unsafeExceptions.canThrowAny

  private lazy val request: Array[Byte] = scala.Array.from(Benchmarks.getRequestBytes)
  private val contentType = "text/plain"

  // ── Netty ──────────────────────────────────────────────────────────────────

  private lazy val nettyDecoder = EmbeddedChannel(netty.HttpRequestDecoder())
  private lazy val nettyEncoder = EmbeddedChannel(netty.HttpResponseEncoder())

  // Decode one request head, returning the length of its method name.
  def nettyParse(): Int =
    nettyDecoder.writeInbound(Unpooled.wrappedBuffer(request))
    var method = -1
    var message: AnyRef | Null = nettyDecoder.readInbound[AnyRef]()

    while message != null do
      message match
        case head: netty.HttpRequest => method = head.method.nn.name.nn.length
        case _                       => ()

      ReferenceCountUtil.release(message)
      message = nettyDecoder.readInbound[AnyRef]()

    method

  // Encode the fixed response, returning its size on the wire.
  def nettySerialize(): Int =
    val response = netty.DefaultFullHttpResponse
      ( netty.HttpVersion.HTTP_1_1,
        netty.HttpResponseStatus.OK,
        Unpooled.wrappedBuffer(RivalJackson.hello) )

    response.headers.nn
    . set(netty.HttpHeaderNames.CONTENT_TYPE, contentType)
    . set(netty.HttpHeaderNames.CONTENT_LENGTH, RivalJackson.hello.length)

    nettyEncoder.writeOutbound(response)
    var size = 0
    var buffer: ByteBuf | Null = nettyEncoder.readOutbound[ByteBuf]()

    while buffer != null do
      size += buffer.readableBytes
      buffer.release()
      buffer = nettyEncoder.readOutbound[ByteBuf]()

    size

  // ── Jetty ──────────────────────────────────────────────────────────────────

  private final class Head extends jetty.HttpParser.RequestHandler:
    var method: Int = -1
    def startRequest(name: String, uri: String, version: jetty.HttpVersion): Unit =
      method = name.length

    def parsedHeader(field: jetty.HttpField): Unit = ()
    def headerComplete(): Boolean = false
    def content(buffer: ByteBuffer): Boolean = false
    def contentComplete(): Boolean = false
    def messageComplete(): Boolean = true
    def earlyEOF(): Unit = ()

  private lazy val jettyHead = Head()
  private lazy val jettyParser = jetty.HttpParser(jettyHead)

  def jettyParse(): Int =
    jettyParser.reset()
    jettyHead.method = -1
    jettyParser.parseNext(ByteBuffer.wrap(request))
    jettyHead.method

  private lazy val jettyGenerator = jetty.HttpGenerator()
  private lazy val jettyHeader = BufferUtil.allocate(4096).nn

  def jettySerialize(): Int =
    val fields = jetty.HttpFields.build().nn
      . add(jetty.HttpHeader.CONTENT_TYPE, contentType).nn
      . asImmutable()

    val info = jetty.MetaData.Response
      ( 200, null, jetty.HttpVersion.HTTP_1_1, fields, RivalJackson.hello.length.toLong )

    val content = ByteBuffer.wrap(RivalJackson.hello).nn
    jettyGenerator.reset()
    BufferUtil.clear(jettyHeader)
    var size = 0
    var done = false

    while !done do
      jettyGenerator.generateResponse(info, false, jettyHeader, null, content, true) match
        case jetty.HttpGenerator.Result.FLUSH =>
          size += jettyHeader.remaining + content.remaining
          BufferUtil.clear(jettyHeader)
          content.position(content.limit)

        case jetty.HttpGenerator.Result.CONTINUE =>
          ()

        case jetty.HttpGenerator.Result.DONE | jetty.HttpGenerator.Result.SHUTDOWN_OUT =>
          done = true

        case other =>
          throw new java.lang.IllegalStateException(s"Jetty's generator asked for $other")

    size
