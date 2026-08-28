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
package telekinesis

import scala.caps

import anticipation.*
import distillate.*
import fulminate.*
import gossamer.*
import inimitable.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import urticose.*
import vacuous.*

object Cookie:
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

  // A `Cookie` is the template a value is written into, not a cookie which has been sent, so
  // its rendering shows the attributes it will impose: the name, and each optional attribute
  // which is set. `Optional` fields are rendered only when present, and the two flags appear
  // as bare words, so the rendering says exactly which attributes the template carries.
  given inspectable: [cookie <: Cookie[?]] => cookie is Inspectable = cookie =>
    Iterable
     ( cookie.name.inspect,
       cookie.domain.let { domain => t"domain:${domain.inspect}" },
       cookie.path.let { path => t"path:${path.inspect}" },
       cookie.expiry.let { expiry => t"expiry:${expiry.inspect}" },
       if cookie.secure then t"secure" else Unset,
       if cookie.httpOnly then t"httpOnly" else Unset )

    . compact.join(t"Cookie(", t" ╱ ", t")")

  object Value:
    given showable: Value is Showable = cookie =>
      Iterable
        ( t"${cookie.name}=${cookie.value}",
          cookie.expiry.let { expiry => t"Max-Age=$expiry" },
          cookie.domain.let { domain => t"Domain=$domain" },
          cookie.path.let { path => t"Path=$path" },
          if cookie.secure then t"Secure" else Unset,
          if cookie.httpOnly then t"HttpOnly" else Unset )

      . compact.join(t"; ")

    // `showable` renders the `Set-Cookie` form, which already shows every attribute the value
    // carries and omits only those which are unset; wrapping it names the type, so the
    // rendering cannot be mistaken for the header text itself.
    given inspectable: [value <: Cookie.Value] => value is Inspectable = value =>
      t"Cookie.Value(${showable.text(value)})"

    given encodable: Cookie.Value is Encodable in Http.Header = cookie =>
      Http.Header("Set-Cookie", cookie.show)

    given addable: Http.Response is Addable by Cookie.Value to Http.Response =
      Addable: (response, cookie) =>
        val header = Http.Header(t"set-cookie", cookie.show)

        // `response` is pure here, so its body is pure; the seal only discharges
        // the field's capture-polymorphic declared type (see `Protoresponse`).
        val body = caps.unsafe.unsafeAssumePure(response.body)
        response.status(header :: response.textHeaders, body)

    given decodable: List[Cookie.Value] is Decodable in Text = value =>
      value.cut(t"; ").bind:
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

  extension (cookie: Cookie[Session])
    def session(lambda: Session ?=> Http.Response)(using Http.Request): Http.Response =
      val session = cookie().or(Session(Uuid().show))
      lambda(using session) + cookie(session)

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
    summon[Http.Request].textCookies(name).let(_.as)

  object Session:
    def unapply(using request: Http.Request)[result](lambda: value ?=> result): Option[result] =
      request.textCookies(name).let(_.as).letGiven(lambda).option
