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
package nomenclature

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import classloaders.threadContextClassloader

sealed trait Id
sealed trait Id2
sealed trait EndsO
sealed trait Session
sealed trait Other

// Planes for the five rules that had no test. Their `Nominative` givens are declared inside the
// `Rules` suite rather than here, so that they stay out of scope for the `n"…"` plane-inference
// tests above, which assert over the exact set of planes visible to them.
sealed trait Prefixed
sealed trait Containing
sealed trait NotEndingZ
sealed trait Lowercase
sealed trait NotDigits
sealed trait Repeated

object Tests extends Suite(m"Nomenclature tests"):
  def run(): Unit =
    inline given id: Id is Nominative under MustEnd["!"] & MustNotStart["0"] & MustNotContain["."] = !!
    inline given id2: Id2 is Nominative under MustNotEqual["."] & MustNotEqual[".."] = !!
    inline given endsO: EndsO is Nominative under MustEnd["o"] = !!

    test(m"Create a successful new name"):
      Name[Id](t"hello!")
    . assert(_ == t"hello!")

    test(m"Create a successful new name with inference"):
      val name: Name[Id] = Name[Id](t"hello!")
      name
    . assert(_ == t"hello!")

    test(m"Name must not start with 0"):
      capture[Name.Error](Name[Id](t"0hello!")).message.show
    . assert(_ == t"the name 0hello! is not valid because it must not start with 0")

    test(m"Name must end with !"):
      capture[Name.Error](Name[Id](t"hello!9")).message.show
    . assert(_ == t"the name hello!9 is not valid because it must end with !")

    test(m"Name must not contain ."):
      capture[Name.Error](Name[Id](t"hello.world!")).message.show
    . assert(_ == t"the name hello.world! is not valid because it must not contain .")

    test(m"Name must not equal ."):
      capture[Name.Error](Name[Id2](t".")).message.show
    . assert(_ == t"the name . is not valid because it must not equal .")

    test(m"Name must not equal .."):
      capture[Name.Error](Name[Id2](t"..")).message.show
    . assert(_ == t"the name .. is not valid because it must not equal ..")

    test(m"Covariance probe: a wider plane intersection is a subtype"):
      val wide: Name[Id2 & EndsO] = t"hello".asInstanceOf[Name[Id2 & EndsO]]
      val narrow: Name[EndsO] = wide
      narrow
    . assert(_ == t"hello")

    test(m"Construct a name at compiletime with no expected type"):
      val name = n"hello"
      name
    . assert(_ == t"hello")

    test(m"An inferred name is usable where one of its planes is required"):
      val name: Name[EndsO] = n"hello"
      name
    . assert(_ == t"hello")

    test(m"An inferred name conforms to the intersection of all its planes"):
      val name: Name[Id2 & EndsO] = n"hello"
      name
    . assert(_ == t"hello")

    test(m"An inferred name is usable where another of its planes is required"):
      val name: Name[Id2] = n"world"
      name
    . assert(_ == t"world")

    test(m"An inferred name is rejected where a non-matching plane is required"):
      demilitarize:
        val name: Name[EndsO] = n"world"
    . assert(_.nonEmpty)

    test(m"An identifier valid in no plane in scope is a compile error"):
      demilitarize:
        val name = n"."
    . assert(_.nonEmpty)

    test(m"Name is required"):
      capture[Name.Error](Name[Required](t"")).message.show
    . assert(_ == t"""the name “” is not valid because it must not be empty""")

    test(m"A valid CSS class name is accepted"):
      Name[CssClass](t"main-nav")
    . assert(_ == t"main-nav")

    test(m"A CSS class name may not start with a digit"):
      capture[Name.Error](Name[CssClass](t"1col")).message.show
    . assert(_ == t"the name 1col is not valid because it must be a valid CSS identifier")

    test(m"A valid DOM id is accepted"):
      Name[DomId](t"main-content")
    . assert(_ == t"main-content")

    test(m"A DOM id may not contain whitespace"):
      capture[Name.Error](Name[DomId](t"a b")).message.show
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
      t"brilliant-leopard".as[Moniker over Session].ordinal
    . assert(_ == 10351)

    test(m"Round-trip a moniker through its name"):
      given (Vocabulary over Session) = Vocabulary(adjectives, animals)
      t"brilliant-leopard".as[Moniker over Session].encode
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
      capture[Moniker.Error](Moniker[Session](100000).encode).message.show
    . assert(_ == t"the moniker is not valid because the number 100000 is outside the representable range")

    test(m"A malformed name cannot be decoded"):
      given (Vocabulary over Session) = Vocabulary(adjectives, animals)
      capture[Moniker.Error](t"justoneword".as[Moniker over Session]).message.show
    . assert(_ == t"the moniker is not valid because justoneword is not of the form <adjective>-<animal>")

    test(m"An unknown word cannot be decoded"):
      given (Vocabulary over Session) = Vocabulary(adjectives, animals)
      capture[Moniker.Error](t"notathing-leopard".as[Moniker over Session]).message.show
    . assert(_ == t"the moniker is not valid because the word notathing does not appear in the vocabulary")

    // A missing `Inspectable` is never a compile error — `derived` always succeeds and
    // substitutes a marked `toString`, `Showable` or `Encodable` rendering — so coverage can
    // only be held in place by asserting on the renderings themselves.
    suite(m"Native-rendering coverage"):
      test(m"nomenclature's types inspect natively"):
        given (Vocabulary over Session) = Vocabulary(adjectives, animals)
        Inspectable.fallbacks(Name[EndsO](t"foo").inspect, Moniker[Session](10351).inspect)
      . assert(_ == Nil)

      test(m"a name is distinguishable from the text it is"):
        Name[EndsO](t"foo").inspect
      . assert(_ == t"n\"foo\"")

      test(m"a moniker inspects as its ordinal"):
        given (Vocabulary over Session) = Vocabulary(adjectives, animals)
        Moniker[Session](10351).inspect
      . assert(_ == Text("10351ᵐᵏ"))

    suite(m"MustStart"):
      inline given prefixed: Prefixed is Nominative under MustStart["x-"] = !!

      test(m"a name with the required prefix is accepted"):
        Name[Prefixed](t"x-ray")
      . assert(_ == t"x-ray")

      test(m"a name without the required prefix is rejected"):
        capture[Name.Error](Name[Prefixed](t"ray")).message.show
      . assert(_ == t"the name ray is not valid because it must start with x-")

      test(m"the prefix must be at the start, not merely present"):
        capture[Name.Error](Name[Prefixed](t"ray-x-")).message.show
      . assert(_ == t"the name ray-x- is not valid because it must start with x-")

      test(m"the prefix alone is a name"):
        Name[Prefixed](t"x-")
      . assert(_ == t"x-")

      test(m"a conforming literal compiles"):
        demilitarize:
          val name: Name[Prefixed] = n"x-ray"
      . assert(_ == Nil)

      test(m"a non-conforming literal is a compile error"):
        demilitarize:
          val name: Name[Prefixed] = n"ray"
      . assert(_.nonEmpty)

    suite(m"MustContain"):
      inline given containing: Containing is Nominative under MustContain["-"] = !!

      test(m"a name containing the required text is accepted"):
        Name[Containing](t"main-nav")
      . assert(_ == t"main-nav")

      test(m"a name not containing the required text is rejected"):
        capture[Name.Error](Name[Containing](t"mainnav")).message.show
      . assert(_ == t"the name mainnav is not valid because it must contain -")

      test(m"the required text may be at the start"):
        Name[Containing](t"-nav")
      . assert(_ == t"-nav")

      test(m"the required text may be at the end"):
        Name[Containing](t"main-")
      . assert(_ == t"main-")

      test(m"a conforming literal compiles"):
        demilitarize:
          val name: Name[Containing] = n"main-nav"
      . assert(_ == Nil)

      test(m"a non-conforming literal is a compile error"):
        demilitarize:
          val name: Name[Containing] = n"mainnav"
      . assert(_.nonEmpty)

    suite(m"MustNotEnd"):
      inline given notEndingZ: NotEndingZ is Nominative under MustNotEnd["z"] = !!

      // `MustNotEnd` is defined as `Rule(…, !_.ends(_))`, where the prefix `!` sits outside a
      // two-placeholder lambda. These two tests pin the sense of that expansion: an inverted
      // predicate would pass one and fail the other.
      test(m"a name not ending with the forbidden text is accepted"):
        Name[NotEndingZ](t"quartz-a")
      . assert(_ == t"quartz-a")

      test(m"a name ending with the forbidden text is rejected"):
        capture[Name.Error](Name[NotEndingZ](t"quartz")).message.show
      . assert(_ == t"the name quartz is not valid because it must not end with z")

      test(m"the forbidden text is permitted away from the end"):
        Name[NotEndingZ](t"zebra")
      . assert(_ == t"zebra")

      test(m"a conforming literal compiles"):
        demilitarize:
          val name: Name[NotEndingZ] = n"zebra"
      . assert(_ == Nil)

      test(m"a non-conforming literal is a compile error"):
        demilitarize:
          val name: Name[NotEndingZ] = n"quartz"
      . assert(_.nonEmpty)

    suite(m"MustMatch"):
      inline given lowercase: Lowercase is Nominative under MustMatch["[a-z]+"] = !!

      test(m"a name matching the pattern is accepted"):
        Name[Lowercase](t"abc")
      . assert(_ == t"abc")

      test(m"a name not matching the pattern is rejected"):
        capture[Name.Error](Name[Lowercase](t"abc1")).message.show
      . assert(_ == t"the name abc1 is not valid because it must match [a-z]+")

      test(m"the empty name does not match a one-or-more pattern"):
        capture[Name.Error](Name[Lowercase](t"")).message.show
      . assert(_ == t"""the name “” is not valid because it must match [a-z]+""")

      test(m"a conforming literal compiles"):
        demilitarize:
          val name: Name[Lowercase] = n"abc"
      . assert(_ == Nil)

      test(m"a non-conforming literal is a compile error"):
        demilitarize:
          val name: Name[Lowercase] = n"abc1"
      . assert(_.nonEmpty)

    suite(m"MustMatch anchoring"):
      inline given repeated: Repeated is Nominative under MustMatch["a+"] = !!

      // `MustMatch` delegates to `String.matches`, which anchors at both ends. That is invisible
      // from the type, so it is pinned here: a pattern found *within* the name is not a match.
      test(m"the pattern must match the whole name, not a substring"):
        capture[Name.Error](Name[Repeated](t"baa")).message.show
      . assert(_ == t"the name baa is not valid because it must match a+")

      test(m"a trailing substring match is also not enough"):
        capture[Name.Error](Name[Repeated](t"aab")).message.show
      . assert(_ == t"the name aab is not valid because it must match a+")

      test(m"the whole name matching is accepted"):
        Name[Repeated](t"aaa")
      . assert(_ == t"aaa")

    suite(m"MustNotMatch"):
      inline given notDigits: NotDigits is Nominative under MustNotMatch["[0-9]+"] = !!

      test(m"a name not matching the forbidden pattern is accepted"):
        Name[NotDigits](t"abc")
      . assert(_ == t"abc")

      test(m"a name matching the forbidden pattern is rejected"):
        capture[Name.Error](Name[NotDigits](t"123")).message.show
      . assert(_ == t"the name 123 is not valid because it must not match [0-9]+")

      test(m"a partial match is permitted, since matching is anchored"):
        Name[NotDigits](t"1a3")
      . assert(_ == t"1a3")

      test(m"a conforming literal compiles"):
        demilitarize:
          val name: Name[NotDigits] = n"abc"
      . assert(_ == Nil)

      test(m"a non-conforming literal is a compile error"):
        demilitarize:
          val name: Name[NotDigits] = n"123"
      . assert(_.nonEmpty)
