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
package gossamer

import anticipation.*
import rudiments.*
import vacuous.*

object Dictionary:
  def apply[element](pairs: (Text, element)*): Dictionary[element] =
    pairs.foldLeft(Dictionary.Empty): (dictionary, next) =>
      dictionary.add(next(0), next(1), 0)

enum Dictionary[+element]:
  case Empty
  case Just(tail: Text, value: element)
  case Branch(value: Optional[element], map: Map[Char, Dictionary[element]])

  def size: Int = this match
    case Empty              => 0
    case Just(_, _)         => 1
    case Branch(Unset, map) => map.sumBy(_(1).size)
    case Branch(_, map)     => map.sumBy(_(1).size) + 1

  def branches: Set[Char] = this match
    case Empty          => Set()
    case Just(tail, _)  => tail.prim.let(Set(_)).or(Set())
    case Branch(_, map) => map.keySet

  def iterator: Iterable[element] = this match
    case Empty          => Nil
    case Just(_, value) => List(value)

    case Branch(value, map) =>
      val values = map.values.flatMap(_.iterator)
      value.lay(values): value => Iterable(value) ++ values

  def element: Optional[element] = this match
    case Empty            => Unset
    case Just("", value)  => value
    case Branch(value, _) => value
    case Just(_, value)   => Unset

  def add[element2 >: element](entry: Text, value: element2, offset: Int): Dictionary[element2] =
    this match
      case Empty => Just(entry, value)

      case just@Just(tail, value0) =>
        if matches(tail, entry, offset) then Just(tail, value) else
          if entry.length == offset
          then Branch(value, Map(tail.s.head -> child(just)))
          else
            val next: Char = entry.s.charAt(offset)
            val rest: Text = entry.s.drop(offset + 1).tt

            if tail.length == 0 then Branch(value0, Map(next -> Just(rest, value)))
            else if tail.s.head == next
            then Branch(Unset, Map(next -> child(just).add(entry, value, offset + 1)))
            else Branch(Unset, Map(next -> Just(rest, value), tail.s.head -> child(just)))

      case Branch(value0, map) =>
        if entry.length == offset then Branch(value, map) else
          val next = entry.s.charAt(offset)

          val child =
            if map.contains(next) then map(next).add(entry, value, offset + 1)
            else Just(entry.s.drop(offset + 1).tt, value)

          Branch(value0, map.updated(next, child))

  private def child(just: Just[element]): Just[element] = Just(just.tail.s.drop(1).tt, just.value)

  private def matches(tail: Text, entry: Text, offset: Int): Boolean =
    tail.length + offset == entry.length && that:
      var matches = true
      var i = 0

      while matches && i < tail.length do
        matches &&= tail.s.charAt(i) == entry.s.charAt(i + offset)
        i += 1

      matches

  protected def lookup(entry: Text, offset: Int): Optional[element] = this match
    case Empty             => Unset
    case Just(tail, value) => if matches(tail, entry, offset) then value else Unset

    case Branch(value, map) =>
      if entry.length > offset then map.at(entry.s.charAt(offset)).let(_.lookup(entry, offset + 1))
      else if entry.length == offset then value
      else Unset

  inline def apply(entry: Text): Optional[element] = lookup(entry, 0)

  def apply(char: Char): Dictionary[element] = this match
    case Empty          => Empty
    case Branch(_, map) => map.at(char).or(Empty)

    case Just(word, value) =>
      if word.length > 0 && word.s.head == char then Just(word.s.drop(1).tt, value) else Empty
