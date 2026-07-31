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

import scala.language.dynamics

import java.net as jn

import scala.util.NotGiven

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import urticose.*
import zephyrine.*

object HttpClient:
  // Log a received response at a level reflecting its status: a server error is a `Fail`, a client
  // error a `Warn`, and anything else (informational, success, redirect) routine `Fine` detail.
  private def logResponse(response: Http.Response): Http.Response =
    response.status.category match
      case Http.Status.Category.ServerError => Log.fail(HttpEvent.Response(response.status))
      case Http.Status.Category.ClientError => Log.warn(HttpEvent.Response(response.status))
      case _                                => Log.fine(HttpEvent.Response(response.status))

    response

  // 301/302/303 historically downgrade the method to GET and drop the body;
  // 307/308 preserve both. Matches Java's `Redirect.NORMAL` and the WHATWG
  // fetch spec.
  private def redirectMethod(code: Int, original: Http.Method): Http.Method = code match
    case 301 | 302 | 303 => Http.Get
    case _               => original

  private def isRedirect(code: Int): Boolean = code match
    case 301 | 302 | 303 | 307 | 308 => true
    case _                           => false

  given httpStrict: (tactic: Tactic[ConnectError])
  =>  Online
  =>  Redirects.Disabled
  =>  ( backend: Http.Backend )
  =>  ( (HttpClient { type Target = Origin["http" | "https"] })^{tactic, caps.any} ) =
    new HttpClient:
      type Target = Origin["http" | "https"]

      def request(httpRequest: Http.Request, origin: Origin["http" | "https"])
        ( using (HttpEvent is Loggable)^ )
      :   Http.Response =

        val url = httpRequest.on(origin)
        Log.info(HttpEvent.Send(httpRequest.method, url, httpRequest.textHeaders))

        logResponse:
          backend.request(url.show, httpRequest.method, httpRequest.textHeaders, httpRequest.body)

  given http: (tactic: Tactic[ConnectError])
  =>  Online
  =>  NotGiven[Redirects.Disabled]
  =>  ( redirection: HttpRedirection )
  =>  ( backend: Http.Backend )
  =>  ( (HttpClient { type Target = Origin["http" | "https"] })^{tactic, caps.any} ) =
    new HttpClient:
      type Target = Origin["http" | "https"]

      def request(httpRequest: Http.Request, origin: Origin["http" | "https"])
        ( using (HttpEvent is Loggable)^ )
      :   Http.Response =

        val url = httpRequest.on(origin)
        Log.info(HttpEvent.Send(httpRequest.method, url, httpRequest.textHeaders))

        // The spring crosses the recursion as a neutral reference: a
        // capability-typed binding of a parameter would hide it from the
        // recursive call under the statement rule.
        def loop(uri: jn.URI, method: Http.Method, bodyRef: AnyRef, remaining: Int)
        :   Http.Response =

          val bodyFn = bodyRef.asInstanceOf[Spring[Data]^]
          val response = backend.request(uri.toString.tt, method, httpRequest.textHeaders, bodyFn)
          val code = response.status.code

          if !isRedirect(code) || remaining <= 0 then response else
            response.textHeaders.stdlib.find(_.key.lower == t"location") match
              case None =>
                response

              case Some(header) =>
                // Drain the discarded intermediate body to free its connection.
                safely(response.body.stream.sweep { (_, _, _) => () })

                val nextUri = uri.resolve(jn.URI.create(header.value.s).nn).nn
                Log.fine(HttpEvent.Redirect(uri.toString.tt, nextUri.toString.tt))
                val nextMethod = redirectMethod(code, method)

                val nextBody: AnyRef =
                  if nextMethod == method then bodyRef else
                    val empty: Spring[Data] = () => Http.emptyBody()
                    empty.asInstanceOf[AnyRef]

                loop(nextUri, nextMethod, nextBody, remaining - 1)

        logResponse:
          loop(jn.URI.create(url.show.s).nn, httpRequest.method,
              httpRequest.body.asInstanceOf[AnyRef], redirection.value)

// An `HttpClient` is a capability: its instances are constructed from other capabilities (a
// `Tactic`, an `Online` token, a backend) which they retain — a given that takes capabilities
// as parameters produces a capability (Jon, 2026-07-06; see rep/DECISIONS.md).
trait HttpClient extends Targetable, caps.ExclusiveCapability:
  def request(request: Http.Request, target: Target)(using (HttpEvent is Loggable)^): Http.Response
