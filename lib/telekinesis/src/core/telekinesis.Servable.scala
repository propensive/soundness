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

import anticipation.*
import contingency.*
import gesticulate.*
import gossamer.*
import prepositional.*
import proscenium.*
import spectacular.*
import turbulence.*

object Servable:
  def apply[response](mediaType: response => MediaType)(lambda: response => Http.Body)
  :   response is Servable = response =>

    val headers = List(Http.Header(t"content-type", mediaType(response).show))
    Http.Ok(headers, lambda(response))


  given content: Content is Servable:
    def serve(content: Content): Http.Response =
      val headers = List(Http.Header(t"content-type", content.media.show))

      Http.Ok(headers, Http.Body.Streaming(content.stream))


  given bytes: [response: Abstractable across HttpStreams to HttpStreams.Content]
  =>  response is Servable =

    Servable[response](value => unsafely(Media.parse(response.generic(value)(0)))): value =>
      Http.Body.Streaming(response.generic(value)(1))


  given data: Data is Servable = Servable[Data](_ => media"application/octet-stream"):
    Http.Body.Fixed(_)

  inline given media: [media: Media] => media is Servable = compiletime.summonFrom:
    case encodable: (`media` is Encodable in Data) =>
      value =>
        val headers = List(Http.Header(t"content-type", media.mediaType(value).show))
        Http.Ok(headers, Http.Body.Fixed(encodable.encode(value)))

    case given (`media` is Streamable by Data) =>
      value =>
        val headers = List(Http.Header(t"content-type", media.mediaType(value).show))
        Http.Ok(headers, Http.Body.Streaming(value.stream[Data]))

    case given (`media` is Streamable by Text) =>
      value =>
        val headers = List(Http.Header(t"content-type", media.mediaType(value).show))
        Http.Ok(headers, Http.Body.Streaming(value.stream[Data]))

trait Servable extends Typeclass:
  def serve(content: Self): Http.Response
  def contramap[self2](lambda: self2 => Self): self2 is Servable = content => serve(lambda(content))
