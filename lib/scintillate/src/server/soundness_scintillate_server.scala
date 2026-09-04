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

import scala.caps

// `as` (decode an HTTP request body, via the `Acceptable` typeclass) clashes in
// the umbrella with distillate's generic `Decodable`-based `as`; reach this one
// via `scintillate.as`.
export
  scintillate
  . { Acceptable, basicAuth, cookie, Frontend, Httpd, NoCache, NotFound, Reactor,
      Redirect, request, RequestServable, Responder, SocketServer, Unfulfilled,
      WebserverErrorPage }

package frontends:
  export scintillate.frontends.{threadPerConnectionFrontend, reactiveFrontend}

package httpServers:
  // Hand-written forwarders rather than an `export`: synthesized export forwarders lose the
  // givens' capture-annotated refinement types (the zephyrine through/accepting finding).
  given jdkHttpserver: [port <: (80 | 443 | 8080 | 8000)]
  =>  ( tactic:  contingency.Tactic[scintillate.Httpd.Error],
        monitor: parasite.Monitor,
        probate: parasite.Probate )
  =>  ( loggable:  scintillate.Httpd.Event is anticipation.Loggable,
        errorPage: scintillate.WebserverErrorPage )
  =>  ((scintillate.httpServers.HttpdFor[port])^{tactic, monitor, caps.any}) =
    // One erasing cast at the forwarding boundary (the wisteria `fieldInstance` pattern):
    // resolution finds the annotated instance, but its capture roots do not re-root through
    // a second given; the declared result type above restores the honest captures.
    scintillate.httpServers.jdkHttpserver[port]
    . asInstanceOf[scintillate.httpServers.HttpdFor[port]]

  given jdkHttpserverPublic: [port <: (80 | 443 | 8080 | 8000)]
  =>  ( tactic:  contingency.Tactic[scintillate.Httpd.Error],
        monitor: parasite.Monitor,
        probate: parasite.Probate )
  =>  ( loggable:  scintillate.Httpd.Event is anticipation.Loggable,
        errorPage: scintillate.WebserverErrorPage )
  =>  ((scintillate.httpServers.HttpdFor[port])^{tactic, monitor, caps.any}) =
    scintillate.httpServers.jdkHttpserverPublic[port]
    . asInstanceOf[scintillate.httpServers.HttpdFor[port]]

  given soundnessHttpd: [port <: (80 | 443 | 8080 | 8000)]
  =>  ( tactic:  contingency.Tactic[scintillate.Httpd.Error],
        monitor: parasite.Monitor,
        probate: parasite.Probate )
  =>  ( loggable:  scintillate.Httpd.Event is anticipation.Loggable,
        errorPage: scintillate.WebserverErrorPage )
  =>  ((scintillate.httpServers.HttpdFor[port])^{tactic, monitor, caps.any}) =
    scintillate.httpServers.soundnessHttpd[port]
    . asInstanceOf[scintillate.httpServers.HttpdFor[port]]

  given soundnessHttpdPublic: [port <: (80 | 443 | 8080 | 8000)]
  =>  ( tactic:  contingency.Tactic[scintillate.Httpd.Error],
        monitor: parasite.Monitor,
        probate: parasite.Probate )
  =>  ( loggable:  scintillate.Httpd.Event is anticipation.Loggable,
        errorPage: scintillate.WebserverErrorPage )
  =>  ((scintillate.httpServers.HttpdFor[port])^{tactic, monitor, caps.any}) =
    scintillate.httpServers.soundnessHttpdPublic[port]
    . asInstanceOf[scintillate.httpServers.HttpdFor[port]]

package webserverErrorPages:
  export scintillate.webserverErrorPages.{minimalErrorPage, stackTracesErrorPage, styledErrorPage}
