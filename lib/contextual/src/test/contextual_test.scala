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
package contextual

import soundness.*

import proscenium.List.stdlib

import scala.compiletime

case class Slug(text: Text)

object Slug:
  // The assembly runs on the stdlib view of the parts: `join` is ambiguous on the opaque `List`
  // (both the `List` and the `Iterable` overload in `gossamer` match it), and the opaque `::`
  // does not survive separation checking here.
  def assemble(parts: List[Any], insertions: List[Any]): Text =
    val values = parts.stdlib
    val head = values.head.toString.tt

    val rest = values.tail.zip(insertions.stdlib).map: (part, insertion) =>
      insertion.toString.tt+part.toString.tt

    (head :: rest).join

  given interpolable: Slug is Interpolable:
    transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
                            (inline insertions: Any*)
    :   Slug =

      Slug(assemble(compiletime.constValueTuple[parts].toList.reverse.to(List),
                    insertions.to(List)))

  given extrapolable: Slug is Extrapolable:
    transparent inline def extrapolate[parts <: Tuple, origins <: Tuple](scrutinee: Slug)
    :   Extrapolation[Slug] =

      scrutinee.text == assemble(compiletime.constValueTuple[parts].toList.reverse.to(List), Nil)

case class Parts(values: List[Text])

object Parts:
  given interpolable: Parts is Interpolable:
    transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
                            (inline insertions: Any*)
    :   Parts =

      Parts(Parts.texts(compiletime.constValueTuple[parts].toList.to(List)))

  def texts(values: List[Any]): List[Text] = values.map(_.toString.tt)

case class Tag(name: Text)

object Tag:
  given embeddable: Tag is Embeddable by Text in Slug = _.name

extension (inline context: StringContext)
  transparent inline def slug: Interpolation = interpolation[Slug](context)
  transparent inline def rawParts: Interpolation = interpolation[Parts](context)

object Tests extends Suite(m"Contextual Tests"):
  def run(): Unit =
    suite(m"Interpolation tests"):
      test(m"Interpolate a literal with no substitutions"):
        slug"hello"
      . assert(_ == Slug(t"hello"))

      test(m"Interpolate an empty literal"):
        slug""
      . assert(_ == Slug(t""))

      test(m"Interpolate a single substitution"):
        val name = t"world"
        slug"hello $name"
      . assert(_ == Slug(t"hello world"))

      test(m"Interpolate several substitutions"):
        val first = 1
        val second = 2
        slug"$first and $second"
      . assert(_ == Slug(t"1 and 2"))

      test(m"A substitution may start the literal"):
        val prefix = t"pre"
        slug"${prefix}fix"
      . assert(_ == Slug(t"prefix"))

      test(m"A doubled dollar is a literal dollar"):
        slug"cost: $$5"
      . assert(_ == Slug(t"cost: $$5"))

      test(m"The transport tuple holds the parts in reverse order"):
        val hole = 1
        rawParts"a${hole}b${hole}c".values
      . assert(_ == List(t"c", t"b", t"a"))

      test(m"The transport tuple includes empty leading parts"):
        val hole = 1
        rawParts"$hole".values
      . assert(_ == List(t"", t""))

    suite(m"Extrapolation tests"):
      test(m"A matching literal pattern succeeds"):
        Slug(t"hello") match
          case slug"hello" => t"matched"
          case _           => t"unmatched"
      . assert(_ == t"matched")

      test(m"A non-matching literal pattern fails"):
        Slug(t"goodbye") match
          case slug"hello" => t"matched"
          case _           => t"unmatched"
      . assert(_ == t"unmatched")

      test(m"An empty pattern matches only an empty value"):
        Slug(t"") match
          case slug"" => t"matched"
          case _      => t"unmatched"
      . assert(_ == t"matched")

    suite(m"Embeddable tests"):
      test(m"Embed a value into its operand type"):
        Tag.embeddable.embed(Tag(t"widget"))
      . assert(_ == t"widget")

      test(m"Contramap an embedding onto another type"):
        val embeddable = Tag.embeddable.contramap[Int](count => Tag(count.show))
        embeddable.embed(42)
      . assert(_ == t"42")

      test(m"An Embeddable value provides a Substitution"):
        summon[Substitution[Text, Tag, "x"]].embed(Tag(t"widget"))
      . assert(_ == t"widget")

    suite(m"Source-position mapping tests"):
      test(m"Unescaped text maps to itself"):
        val mapping = Interpolation.buildMapping("hello", "hello")
        List(0, 1, 4, 5).map(mapping)
      . assert(_ == List(0, 1, 4, 5))

      test(m"An empty value maps to the start"):
        Interpolation.buildMapping("", "")(0)
      . assert(_ == 0)

      test(m"A doubled dollar consumes two source characters"):
        val mapping = Interpolation.buildMapping("a$$b", "a$b")
        List(0, 1, 2, 3).map(mapping)
      . assert(_ == List(0, 1, 3, 4))

      test(m"A unicode escape consumes six source characters"):
        val mapping = Interpolation.buildMapping("\\u0041b", "Ab")
        List(0, 1, 2).map(mapping)
      . assert(_ == List(0, 6, 7))

      test(m"A negative index maps to the start"):
        Interpolation.buildMapping("hello", "hello")(-1)
      . assert(_ == 0)

      test(m"An index beyond the end maps to the last position"):
        Interpolation.buildMapping("hello", "hello")(99)
      . assert(_ == 5)
