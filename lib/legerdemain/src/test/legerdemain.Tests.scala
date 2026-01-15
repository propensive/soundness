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
┃    Soundness, version 0.51.0.                                                                    ┃
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
package legerdemain

import soundness.*
import telekinesis.*

import formulations.default
import strategies.throwUnsafely
import httpServers.stdlib
import codicils.cancel
import logging.silent
import doms.html.whatwg, whatwg.*
import charEncoders.utf8
import threading.platform
import errorDiagnostics.stackTraces

import doms.html.whatwg, whatwg.*

case class Group(org: Organization)
case class Organization(leader: Person, name: Name[Person])

object Person:
  erased given Person is Nominative under MustMatch["[A-Z][a-z]+"] = !!

case class Person(name: Name[Person], description: Text, email: EmailAddress)

object Tests extends Suite(m"Legerdemain tests"):
  def run(): Unit =

    // test(m"Create a simple form"):
    //   val person = Person(t"John", t"A generic person", true)
    //   elicit[Person](t"Sample form", Query(person))
    // .assert(_ == whatwge.Form)

    // test(m"Create a form from nested fields"):
    //   val organization = Organization(Person(t"Paul", t"Generic", false), t"My org")
    //   elicit[Organization](t"Sample", Query(organization))
    // .assert(_ == whatwge.Form)

    summon[Boolean is Decodable in Query]

    supervise:
      tcp"8082".serve[Http]:
        orchestrate[Group]:
          case Submission.Complete(group) =>
            Http.Response(Http.Ok)(HtmlDoc(Html(Head(Title(t"Page")), Body(H1(group.inspect)))))

          case Submission.Incomplete(form) =>
            Http.Response(Http.Ok)(HtmlDoc(Html(Head(Title(t"Page")), Body(form))))

          case Submission.Invalid(query) =>
            val errors =
              validate[Text](Errors()):
                case error@EmailAddressError(_) => accrual + (focus.or(t"unknown"), error)
                case error@NameError(_, _, _)   => accrual + (focus.or(t"unknown"), error)

              . within(query.as[Group])

            abort(errors)
