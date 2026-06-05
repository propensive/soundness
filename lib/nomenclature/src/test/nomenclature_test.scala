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

    val adverbs = cp"/nomenclature/adverbs.txt"
    val adjectives = cp"/nomenclature/adjectives.txt"
    val animals = cp"/nomenclature/animals.txt"

    test(m"Low monikers are just an animal"):
      given (Vocabulary over Session) = Vocabulary(adverbs, adjectives, animals)
      (Moniker[Session](0).encode, Moniker[Session](249).encode)
    . assert(_ == (t"aardvark", t"ocelot"))

    test(m"Mid monikers add an adjective prefix"):
      given (Vocabulary over Session) = Vocabulary(adverbs, adjectives, animals)
      (Moniker[Session](250).encode, Moniker[Session](10351).encode)
    . assert(_ == (t"able-aardvark", t"bright-leopard"))

    test(m"High monikers add an adverb prefix too"):
      given (Vocabulary over Session) = Vocabulary(adverbs, adjectives, animals)
      (Moniker[Session](100250).encode, Moniker[Session](310351).encode)
    . assert(_ == (t"barely-able-aardvark", t"slightly-bright-leopard"))

    test(m"Infer the plane from the single vocabulary in scope"):
      given (Vocabulary over Session) = Vocabulary(adverbs, adjectives, animals)
      Moniker(310351).encode
    . assert(_ == t"slightly-bright-leopard")

    test(m"Infer the plane from a single unplaned vocabulary"):
      given Vocabulary = Vocabulary(adverbs, adjectives, animals)
      Moniker(250).encode
    . assert(_ == t"able-aardvark")

    test(m"Decode names from each tier back to numbers"):
      given (Vocabulary over Session) = Vocabulary(adverbs, adjectives, animals)
      ( t"aardvark".decode[Moniker over Session].ordinal,
        t"able-aardvark".decode[Moniker over Session].ordinal,
        t"slightly-bright-leopard".decode[Moniker over Session].ordinal )
    . assert(_ == (0, 250, 310351))

    test(m"Round-trip a moniker through its name"):
      given (Vocabulary over Session) = Vocabulary(adverbs, adjectives, animals)
      t"slightly-bright-leopard".decode[Moniker over Session].encode
    . assert(_ == t"slightly-bright-leopard")

    test(m"Vocabulary size spans all three tiers"):
      Vocabulary(adverbs, adjectives, animals).size
    . assert(_ == 250 + 400*250 + 55*400*250)

    test(m"Select a plane explicitly when several are in scope"):
      given (Vocabulary over Session) = Vocabulary(adverbs, adjectives, animals)
      given (Vocabulary over Other) = Vocabulary(adverbs, animals, adjectives)
      (Moniker[Session](310351).encode, Moniker[Other](310351).encode == Moniker[Session](310351).encode)
    . assert(_ == (t"slightly-bright-leopard", false))

    test(m"An out-of-range number cannot be encoded"):
      given (Vocabulary over Session) = Vocabulary(adverbs, adjectives, animals)
      capture[MonikerError](Moniker[Session](5600250).encode).message.show
    . assert(_ == t"the moniker is not valid because the number 5600250 is outside the representable range")

    test(m"A malformed name cannot be decoded"):
      given (Vocabulary over Session) = Vocabulary(adverbs, adjectives, animals)
      capture[MonikerError](t"too-many-words-here".decode[Moniker over Session]).message.show
    . assert(_ == t"the moniker is not valid because too-many-words-here is not a valid moniker")

    test(m"An unknown word cannot be decoded"):
      given (Vocabulary over Session) = Vocabulary(adverbs, adjectives, animals)
      capture[MonikerError](t"notathing-leopard".decode[Moniker over Session]).message.show
    . assert(_ == t"the moniker is not valid because the word notathing does not appear in the vocabulary")
