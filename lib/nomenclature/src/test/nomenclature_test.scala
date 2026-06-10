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
package nomenclature

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTraces
import classloaders.threadContext

sealed trait Id
sealed trait Id2
sealed trait Session
sealed trait Other

object Tests extends Suite(m"Nomenclature tests"):
  def run(): Unit =
    inline given id: Id is Nominative under MustEnd["!"] & MustNotStart["0"] & MustNotContain["."] = !!
    inline given id2: Id2 is Nominative under MustNotEqual["."] & MustNotEqual[".."] = !!

    test(m"Create a successful new name"):
      Name[Id](t"hello!")
    . assert(_ == t"hello!")

    test(m"Create a successful new name with inference"):
      val name: Name[Id] = Name[Id](t"hello!")
      name
    . assert(_ == t"hello!")

    test(m"Name must not start with 0"):
      capture[NameError](Name[Id](t"0hello!")).message.show
    . assert(_ == t"the name 0hello! is not valid because it must not start with 0")

    test(m"Name must end with !"):
      capture[NameError](Name[Id](t"hello!9")).message.show
    . assert(_ == t"the name hello!9 is not valid because it must end with !")

    test(m"Name must not contain ."):
      capture[NameError](Name[Id](t"hello.world!")).message.show
    . assert(_ == t"the name hello.world! is not valid because it must not contain .")

    test(m"Name must not equal ."):
      capture[NameError](Name[Id2](t".")).message.show
    . assert(_ == t"the name . is not valid because it must not equal .")

    test(m"Name must not equal .."):
      capture[NameError](Name[Id2](t"..")).message.show
    . assert(_ == t"the name .. is not valid because it must not equal ..")

    test(m"Construct a new name at compiletime"):
      n"hello": Name[Id2]
    . assert(_ == t"hello")

    test(m"Name is required"):
      capture[NameError](Name[Required](t"")).message.show
    . assert(_ == t"""the name “” is not valid because it must not be empty""")

    test(m"A valid CSS class name is accepted"):
      Name[CssClass](t"main-nav")
    . assert(_ == t"main-nav")

    test(m"A CSS class name may not start with a digit"):
      capture[NameError](Name[CssClass](t"1col")).message.show
    . assert(_ == t"the name 1col is not valid because it must be a valid CSS identifier")

    test(m"A CSS class name is constructed at compiletime"):
      n"button": Name[CssClass]
    . assert(_ == t"button")

    test(m"A valid DOM id is accepted"):
      Name[DomId](t"main-content")
    . assert(_ == t"main-content")

    test(m"A DOM id may not contain whitespace"):
      capture[NameError](Name[DomId](t"a b")).message.show
    . assert(_ == t"the name a b is not valid because it must be a valid DOM id")

    val adjectives = cp"/nomenclature/adjectives.txt"
    val animals = cp"/nomenclature/animals.txt"

    test(m"Encode a moniker with an explicit plane"):
      given (Vocabulary over Session) = Vocabulary(adjectives, animals)
      Moniker[Session](10351).encode
    . assert(_ == t"brilliant-leopard")

    test(m"Infer the plane from the single vocabulary in scope"):
      given (Vocabulary over Session) = Vocabulary(adjectives, animals)
      Moniker(10351).encode
    . assert(_ == t"brilliant-leopard")

    test(m"Infer the plane from a single unplaned vocabulary"):
      given Vocabulary = Vocabulary(adjectives, animals)
      Moniker(10351).encode
    . assert(_ == t"brilliant-leopard")

    test(m"Decode a friendly name to a moniker"):
      given (Vocabulary over Session) = Vocabulary(adjectives, animals)
      t"brilliant-leopard".decode[Moniker over Session].ordinal
    . assert(_ == 10351)

    test(m"Round-trip a moniker through its name"):
      given (Vocabulary over Session) = Vocabulary(adjectives, animals)
      t"brilliant-leopard".decode[Moniker over Session].encode
    . assert(_ == t"brilliant-leopard")

    test(m"Vocabulary size is the product of the word counts"):
      Vocabulary(adjectives, animals).size
    . assert(_ == 100000)

    test(m"Select between planes explicitly"):
      given (Vocabulary over Session) = Vocabulary(adjectives, animals)
      given (Vocabulary over Other) = Vocabulary(animals, adjectives)
      (Moniker[Session](10351).encode, Moniker[Other](10351).encode)
    . assert(_ == (t"brilliant-leopard", t"capybara-tart"))

    test(m"An out-of-range number cannot be encoded"):
      given (Vocabulary over Session) = Vocabulary(adjectives, animals)
      capture[MonikerError](Moniker[Session](100000).encode).message.show
    . assert(_ == t"the moniker is not valid because the number 100000 is outside the representable range")

    test(m"A malformed name cannot be decoded"):
      given (Vocabulary over Session) = Vocabulary(adjectives, animals)
      capture[MonikerError](t"justoneword".decode[Moniker over Session]).message.show
    . assert(_ == t"the moniker is not valid because justoneword is not of the form <adjective>-<animal>")

    test(m"An unknown word cannot be decoded"):
      given (Vocabulary over Session) = Vocabulary(adjectives, animals)
      capture[MonikerError](t"notathing-leopard".decode[Moniker over Session]).message.show
    . assert(_ == t"the moniker is not valid because the word notathing does not appear in the vocabulary")
