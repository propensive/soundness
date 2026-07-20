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
package soundness

// `as` (decode an HTTP request body, via the `Acceptable` typeclass) clashes in
// the umbrella with distillate's generic `Decodable`-based `as`; reach this one
// via `scintillate.as`.
export
  scintillate
  . { Acceptable, basicAuth, cookie, HttpConnection, HttpServer, HttpServerEvent, NoCache, NotFound,
      Redirect, request, RequestServable, Responder, ServerError, SocketServer, Unfulfilled,
      WebserverErrorPage }

package httpServers:
  // Hand-written forwarders rather than an `export`: synthesized export forwarders lose the
  // givens' capture-annotated refinement types (the zephyrine through/accepting finding).
  given stdlibHttpServer: [port <: (80 | 443 | 8080 | 8000)]
  =>  ( tactic:  contingency.Tactic[scintillate.ServerError],
        monitor: parasite.Monitor,
        probate: parasite.Probate )
  =>  ( loggable:  scintillate.HttpServerEvent is anticipation.Loggable,
        errorPage: scintillate.WebserverErrorPage )
  =>  ((scintillate.httpServers.HttpServerFor[port])^{tactic, monitor, caps.any}) =
    // One erasing cast at the forwarding boundary (the wisteria `fieldInstance` pattern):
    // resolution finds the annotated instance, but its capture roots do not re-root through
    // a second given; the declared result type above restores the honest captures.
    scintillate.httpServers.stdlibHttpServer[port]
    . asInstanceOf[scintillate.httpServers.HttpServerFor[port]]

  given stdlibPublicHttpServer: [port <: (80 | 443 | 8080 | 8000)]
  =>  ( tactic:  contingency.Tactic[scintillate.ServerError],
        monitor: parasite.Monitor,
        probate: parasite.Probate )
  =>  ( loggable:  scintillate.HttpServerEvent is anticipation.Loggable,
        errorPage: scintillate.WebserverErrorPage )
  =>  ((scintillate.httpServers.HttpServerFor[port])^{tactic, monitor, caps.any}) =
    scintillate.httpServers.stdlibPublicHttpServer[port]
    . asInstanceOf[scintillate.httpServers.HttpServerFor[port]]

package webserverErrorPages:
  export scintillate.webserverErrorPages.{minimalErrorPage, stackTracesErrorPage, standardErrorPage}
