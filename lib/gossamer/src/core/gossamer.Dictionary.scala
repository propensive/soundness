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

import scala.annotation.tailrec

import anticipation.*
import rudiments.*
import vacuous.*

object Dictionary:
  def apply[element](pairs: (Text, element)*): Dictionary[element] =
    pairs.foldLeft(Dictionary.Empty): (dictionary, next) =>
      dictionary.add(next(0), next(1), 0)

  // Build a `Just` whose suffix begins at the given offset of `text`. Avoids
  // allocating a fresh substring at construction time; the offset is consumed
  // virtually as the dictionary is walked.
  private[gossamer] inline def just[element](text: Text, offset: Int, value: element)
  :   Just[element] =

    Just(text, offset, value)

  extension [element](just: Just[element])
    // Number of characters remaining in the virtual suffix.
    inline def tailLength: Int = just.text.length - just.offset

    // True if the virtual suffix has been fully consumed.
    inline def tailEmpty: Boolean = just.offset >= just.text.length

    // First character of the virtual suffix, if any.
    inline def head: Optional[Char] =
      if just.offset < just.text.length then just.text.s.charAt(just.offset) else Unset

    // Materialize the virtual suffix as a `Text`. Allocates only when called.
    def tail: Text =
      if just.offset == 0 then just.text
      else just.text.s.substring(just.offset).nn.tt

// `Just(text, offset, value)` represents a unique remaining suffix `text[offset:]`
// in the trie. Walking one character forward returns a sibling `Just` with the
// same shared `text` and `offset + 1` — no `String.substring` allocation per
// step. The `tail`, `head`, `tailLength` and `tailEmpty` accessors expose the
// virtual suffix without forcing materialization.
enum Dictionary[+element]:
  case Empty
  case Just(text: Text, offset: Int, value: element)
  case Branch(value: Optional[element], map: Map[Char, Dictionary[element]])

  def size: Int = this match
    case Empty              => 0
    case Just(_, _, _)      => 1
    case Branch(Unset, map) => map.sumBy(_(1).size)
    case Branch(_, map)     => map.sumBy(_(1).size) + 1

  def branches: Set[Char] = this match
    case Empty                  => Set()
    case just: Just[element]    => just.head.let(Set(_)).or(Set())
    case Branch(_, map)         => map.keySet

  def iterator: Iterable[element] = this match
    case Empty             => Nil
    case Just(_, _, value) => List(value)

    case Branch(value, map) =>
      val values = map.values.flatMap(_.iterator)
      value.lay(values): value => Iterable(value) ++ values

  def element: Optional[element] = this match
    case Empty                                       => Unset
    case just: Just[element] if just.tailEmpty       => just.value
    case Branch(value, _)                            => value
    case _: Just[element]                            => Unset

  def add[element2 >: element](entry: Text, value: element2, offset: Int): Dictionary[element2] =
    this match
      case Empty => Just(entry, offset, value)

      case just: Just[element] =>
        if matchesEntry(just.text, just.offset, entry, offset) then Just(entry, offset, value) else
          if entry.length == offset
          then Branch(value, Map(just.head.vouch -> child(just)))
          else
            val next: Char = entry.s.charAt(offset)
            val tailLen: Int = just.tailLength

            if tailLen == 0 then Branch(just.value, Map(next -> Just(entry, offset + 1, value)))
            else if just.head.vouch == next
            then Branch(Unset, Map(next -> child(just).add(entry, value, offset + 1)))
            else Branch
              ( Unset,
                Map( next             -> Just(entry, offset + 1, value),
                     just.head.vouch  -> child(just) ) )

      case Branch(value0, map) =>
        if entry.length == offset then Branch(value, map) else
          val next = entry.s.charAt(offset)

          val child =
            if map.contains(next) then map(next).add(entry, value, offset + 1)
            else Just(entry, offset + 1, value)

          Branch(value0, map.updated(next, child))

  private def child(just: Just[element]): Just[element] =
    Just(just.text, just.offset + 1, just.value)

  // Compares the virtual suffix `justText[justOffset:]` against `entry[entryOffset:]`
  // for full equality without materializing either substring.
  private def matchesEntry(justText: Text, justOffset: Int, entry: Text, entryOffset: Int)
  :   Boolean =

    val justLen: Int = justText.length - justOffset
    val entryLen: Int = entry.length - entryOffset
    if justLen != entryLen then false
    else
      var ok = true
      var i = 0
      while ok && i < justLen do
        ok = justText.s.charAt(justOffset + i) == entry.s.charAt(entryOffset + i)
        i += 1
      ok

  protected def lookup(entry: Text, offset: Int): Optional[element] = this match
    case Empty                  => Unset

    case just: Just[element] =>
      if matchesEntry(just.text, just.offset, entry, offset) then just.value else Unset

    case Branch(value, map) =>
      if entry.length > offset then map.at(entry.s.charAt(offset)).let(_.lookup(entry, offset + 1))
      else if entry.length == offset then value
      else Unset

  inline def apply(entry: Text): Optional[element] = lookup(entry, 0)

  // Case-insensitive (ASCII-folded) lookup over a slice of an existing char
  // buffer. Performs the entire trie traversal without allocating: the input is
  // not materialized into a `Text`, and the `Just` arm is walked by tracking an
  // extra offset against the existing node rather than constructing fresh
  // dictionary nodes per character. Designed for hot-path callers (e.g. HTML
  // tag/attribute name lookups) where the input is already in a `char[]`.
  def lookupAscii(buffer: Array[Char], offset: Int, length: Int): Optional[element] =
    val stop: Int = offset + length

    @tailrec
    def step(node: Dictionary[element], extra: Int, position: Int): Optional[element] =
      if position >= stop then node match
        case Empty                  => Unset

        case just: Just[element] =>
          if just.offset + extra == just.text.length then just.value else Unset

        case Branch(value, _)       => value
      else
        val raw: Char = buffer(position)
        val char: Char = if raw >= 'A' && raw <= 'Z' then (raw + 32).toChar else raw
        node match
          case Empty                  => Unset

          case Branch(_, map) => map.at(char) match
            case next: Dictionary[element] @unchecked => step(next, 0, position + 1)
            case _                                    => Unset

          case just: Just[element] =>
            val pos: Int = just.offset + extra
            if pos < just.text.length && just.text.s.charAt(pos) == char
            then step(just, extra + 1, position + 1)
            else Unset

    step(this, 0, offset)

  // Walk one character forward in the trie. For `Just`, this avoids the
  // historical `String.substring` allocation by reusing the existing `text`
  // reference and advancing the virtual `offset` by one — only the wrapping
  // `Just` is allocated, not its underlying tail.
  def apply(char: Char): Dictionary[element] = this match
    case Empty               => Empty
    case Branch(_, map)      => map.at(char).or(Empty)

    case just: Just[element] =>
      if just.offset < just.text.length && just.text.s.charAt(just.offset) == char
      then Just(just.text, just.offset + 1, just.value)
      else Empty
