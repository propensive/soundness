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
package chiaroscuro

import soundness.*

import textMetrics.uniformMetric
import chiaroscuro.teletypeables.juxtapositionTeletype

case class Person(name: Text, age: Int)
case class Organization(name: Text, ceo: Person, staff: List[Person])

case class IdName(id: Text, name: Text)

import Decomposition.*

object Tests extends Suite(m"Chiaroscuro tests"):
  def run(): Unit =
    suite(m"Decomposition tests"):
      test(m"Decompose an unknown type"):
        24.decompose

      . assert(_ == Primitive(t"Int", t"24", 24))

      test(m"Decompose a known type"):
        t"hello".decompose

      . assert(_ == Primitive(t"Text", t"hello", t"hello"))


      test(m"Decompose a person"):
        Person(t"Bill", 29).decompose

      . assert: value =>
          value == Product
            ( t"Person",
              Map(t"name" -> Primitive(t"Text", t"Bill", t"Bill"),
                  t"age"  -> Primitive(t"Int", t"29", 29)),
              Person(t"Bill", 29))

      test(m"Decompose an organization"):
        Organization(t"Acme", Person(t"Bill", 29), Nil).decompose

      . assert:
          _ == Product
                (t"Organization",
                 Map(t"name"  -> Primitive(t"Text", t"Acme", t"Acme"),
                     t"ceo"   -> Product
                                  (t"Person",
                                   Map(t"name" -> Primitive(t"Text", t"Bill", t"Bill"),
                                       t"age"  -> Primitive(t"Int", t"29", 29)),
                                   Person(t"Bill", 29)),
                     t"staff" -> Decomposition.Sequence(t"List", Nil, Nil)),
                 Organization(t"Acme", Person(t"Bill", 29), Nil))

      test(m"Decompose a sequence"):
        val list: List[Char] = List('a', 'b')
        list.decompose

      . assert:
          _ == Decomposition.Sequence(t"List", List(Primitive(t"Char", t"a", 'a'),
                                      Primitive(t"Char", t"b", 'b')), List('a', 'b'))

      test(m"Decompose an optional value"):
        val x: Optional[Int] = 12
        x.decompose

      . assert(_ == Sum(t"Optional", Primitive(t"Int", t"12", 12), 12))

      test(m"Decompose an unset optional"):
        val x: Optional[Int] = Unset
        x.decompose

      . assert(_ == Sum(t"Optional", Primitive(t"Unset", t"∅", Unset), Unset))

      test(m"Decompose a non-showable value"):
        3.1415926.decompose

      . assert(_ == Primitive(t"Double", t"3.1415926", 3.1415926))

      test(m"Decompose a showable value"):
        given Decimalizer(3)
        3.1415926.decompose

      . assert(_ == Primitive(t"Double", t"3.14", 3.1415926))

      test(m"Decompose an Any-typed value"):
        val x: Any = t"hello"
        x.decompose

      . assert(_ == Primitive(t"Any", t"hello", t"hello"))

      test(m"Decompose list of Any-typed value"):
        val x: List[Any] = List(t"hello")
        x.decompose

      . assert(_ == Decomposition.Sequence(t"List", List(Primitive(t"Any", t"hello", t"hello")), List(t"hello")))

      test(m"Decompose list of list of text"):
        val x: List[List[Text]] = List(List(t"hello"))
        x.decompose

      . assert(_ == Decomposition.Sequence(t"List", List(Decomposition.Sequence(t"List", List(Primitive(t"Text", t"hello", t"hello")), List(t"hello"))), List(List(t"hello"))))


      test(m"Structural comparison"):
        Organization(t"Acme", Person(t"John", 49), List(Person(t"Janet", 19), Person(t"Paweł", 32)))

      . aspire:
          _ == Organization(t"Acme", Person(t"John", 43), List(Person(t"Paul", 32), Person(t"Janet", 19)))

      test(m"Text comparison"):
        t"The quick brown fox jumps over the lazy dog"
      . aspire(_ == t"The quick brown foxes jumped over the dog")

    suite(m"Juxtaposition rendering tests"):
      // `Text is Measurable` is derived generically from `Char is Measurable`, as escritoire does.
      given charMeasurable: Char is Measurable = _.toString.tt.metrics

      given palette: JuxtapositionPalette = new JuxtapositionPalette:
        def background: Color in Srgb  = WebColors.Black
        def foreground: Color in Srgb  = WebColors.White
        def unaccented: Color in Srgb  = WebColors.DimGray
        def informative: Color in Srgb = WebColors.Silver
        def subdued: Color in Srgb     = WebColors.DimGray
        def positive: Color in Srgb    = WebColors.LimeGreen
        def negative: Color in Srgb    = WebColors.Red

      // `juxtapositionTeletype` is no longer in `Juxtaposition`'s implicit scope, and escapade
      // renders any `Showable` value unstyled, so a call site which fails to import it degrades
      // silently rather than failing to compile. These assertions pin the real renderer's output.
      // `juxtapositionTeletype` is no longer in `Juxtaposition`'s implicit scope, and escapade
      // renders any `Showable` value unstyled, so a call site which fails to import it degrades
      // silently — printing `Collation(Text,List(…))` — rather than failing to compile.
      test(m"A differing pair renders as a difference table, not as the enum's text"):
        t"alpha".contrast(t"alpha".sub(t"a", t"b")).teletype.plain.trim
      . assert(_ == t"""────┬─────┬────
   ⁰│alpha│⁵   
   ₀│blphb│₅   
────┴─────┴────""")

      test(m"An equal pair renders as the expected value"):
        24.contrast(24).teletype.plain
      . assert(_ == t"The value 24 was expected")
