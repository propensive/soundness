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

import java.text as jt

import anticipation.*
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import urticose.*
import vacuous.*

object Cookie:
  val dateFormat: jt.SimpleDateFormat = jt.SimpleDateFormat("dd MMM yyyy HH:mm:ss")

  // For some reason it seems necessary to use `DummyImplicit` instead of `Void` here
  def apply[value: {Encodable in Text, Decodable in Text}](using DummyImplicit)
    [ duration: Abstractable across Durations to Long ]
    ( name:     Text,
      domain:   Optional[Hostname] = Unset,
      expiry:   Optional[duration] = Unset,
      secure:   Boolean            = false,
      httpOnly: Boolean            = false,
      path:     Optional[Text]     = Unset ) =

    new Cookie[value](name, domain, expiry.let(_.generic/1_000_000L), secure, httpOnly, path)


  object Value:
    given showable: Value is Showable = cookie =>
      List
        ( t"${cookie.name}=${cookie.value}",
          cookie.expiry.let { expiry => t"Max-Age=$expiry" },
          cookie.domain.let { domain => t"Domain=$domain" },
          cookie.path.let { path => t"Path=$path" },
          if cookie.secure then t"Secure" else Unset,
          if cookie.httpOnly then t"HttpOnly" else Unset )

      . compact.join(t"; ")

    given encodable: Cookie.Value is Encodable in Http.Header = cookie =>
      Http.Header("Set-Cookie", cookie.show)

    given addable: Http.Response is Addable by Cookie.Value to Http.Response =
      (response, cookie) =>
        val header = Http.Header(t"set-cookie", cookie.show)
        response.status(header :: response.textHeaders, response.body)

    given decodable: List[Cookie.Value] is Decodable in Text = value =>
      value.cut(t"; ").flatMap:
        _.cut(t"=", 2) match
          case List(key, value) => List(Cookie.Value(key.urlDecode, value.urlDecode))
          case _                => Nil

  case class Value
    ( name:     Text,
      value:    Text,
      domain:   Optional[Text] = Unset,
      path:     Optional[Text] = Unset,
      expiry:   Optional[Long] = Unset,
      secure:   Boolean        = false,
      httpOnly: Boolean        = false )

case class Cookie[value: {Encodable in Text, Decodable in Text}]
  ( name:     Text,
    domain:   Optional[Hostname],
    expiry:   Optional[Long],
    secure:   Boolean,
    httpOnly: Boolean,
    path:     Optional[Text] ):

  def apply(value: value): Cookie.Value =
    Cookie.Value(name, value.encode, domain.let(_.show), path, expiry.let(_/1000), secure, httpOnly)

  inline def apply()(using Http.Request): Optional[value] =
    summon[Http.Request].textCookies.at(name).let(_.decode)

  object Session:
    def unapply(using request: Http.Request)[result](lambda: value ?=> result): Option[result] =
      request.textCookies.at(name).let(_.decode).letGiven(lambda).option
