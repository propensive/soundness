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

import io.netty.bootstrap.ServerBootstrap
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel.*
import io.netty.channel.nio.NioIoHandler
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.handler.codec.http.*

// Raw Netty on NIO: `HttpServerCodec`, an aggregator for the echo body, and one handler
// dispatching on the URI by hand, in the shape of TechEmpower's Netty entry.
object RivalNetty:
  @ChannelHandler.Sharable
  final class Handler extends SimpleChannelInboundHandler[FullHttpRequest]:
    override def channelRead0(context: ChannelHandlerContext, request: FullHttpRequest): Unit =
      val uri = request.uri.nn

      def respond(status: HttpResponseStatus, body: ByteBuf, contentType: String): Unit =
        val response = DefaultFullHttpResponse(request.protocolVersion, status, body)
        response.headers.nn
        . set(HttpHeaderNames.CONTENT_TYPE, contentType)
        . set(HttpHeaderNames.CONTENT_LENGTH, body.readableBytes)

        if HttpUtil.isKeepAlive(request) then context.writeAndFlush(response)
        else context.writeAndFlush(response).nn.addListener(ChannelFutureListener.CLOSE)

      uri match
        case "/bench" =>
          respond(HttpResponseStatus.OK, Unpooled.wrappedBuffer(RivalJackson.hello), "text/plain")

        case "/json" =>
          respond
            ( HttpResponseStatus.OK,
              Unpooled.wrappedBuffer(RivalJackson.greeting()),
              "application/json" )

        case "/large" =>
          respond
            ( HttpResponseStatus.OK,
              Unpooled.wrappedBuffer(HttpWorkload.largeBody),
              "application/octet-stream" )

        case "/echo" =>
          respond(HttpResponseStatus.OK, request.content.nn.retain(), "application/octet-stream")

        case _ if uri.startsWith(RivalJackson.userPrefix) =>
          val id = uri.substring(RivalJackson.userPrefix.length)
          val requestId = request.headers.nn.get(HttpWorkload.requestIdHeader)
          val body = s"$id:${if requestId == null then "" else requestId}"
          respond(HttpResponseStatus.OK, Unpooled.copiedBuffer(body.getBytes("UTF-8")), "text/plain")

        case _ =>
          respond(HttpResponseStatus.NOT_FOUND, Unpooled.EMPTY_BUFFER, "text/plain")

  lazy val server: Unit =
    val boss = MultiThreadIoEventLoopGroup(1, NioIoHandler.newFactory())
    val workers = MultiThreadIoEventLoopGroup(NioIoHandler.newFactory())
    val handler = Handler()

    val initializer = new ChannelInitializer[SocketChannel]:
      override def initChannel(channel: SocketChannel): Unit =
        channel.pipeline.nn.addLast(HttpServerCodec(), HttpObjectAggregator(1 << 20), handler)

    ServerBootstrap()
    . group(boss, workers).nn
    . channel(classOf[NioServerSocketChannel]).nn
    . childOption(ChannelOption.TCP_NODELAY, true).nn
    . childHandler(initializer).nn
    . bind(HttpRivals.port(HttpRivals.Netty)).nn
    . sync()

    HttpRivals.ready(HttpRivals.Netty)
