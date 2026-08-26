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
package praxinoscope

import scala.collection.mutable.ArrayBuffer

import anticipation.*
import vacuous.*

import Motif.Node

object Pike:
  // A priority-ordered thread list for one input position. The `dense`/`sparse` pair is a
  // sparse set over instruction pcs, recording every pc reached during ε-closure so each is
  // expanded at most once per position; insertion order is preference order, which is what
  // makes matching leftmost-first deterministic.
  private class Threads(limit: Int):
    private val dense: scala.Array[Int] = new scala.Array[Int](limit)
    private val sparse: scala.Array[Int] = new scala.Array[Int](limit)
    private var marks: Int = 0

    val pcs: ArrayBuffer[Int] = ArrayBuffer()
    val regs: ArrayBuffer[scala.Array[Int]] = ArrayBuffer()

    def clear(): Unit =
      marks = 0
      pcs.clear()
      regs.clear()

    def marked(pc: Int): Boolean = sparse(pc) < marks && dense(sparse(pc)) == pc

    def mark(pc: Int): Unit =
      sparse(pc) = marks
      dense(marks) = pc
      marks += 1

    def push(pc: Int, saved: scala.Array[Int]): Unit =
      pcs += pc
      regs += saved

    def size: Int = pcs.size

  private def within(bounds: scala.IArray[Int], symbol: Int): Boolean =
    var index = 0
    var found = false

    while !found && index < bounds.length do
      found = symbol >= bounds(index) && symbol <= bounds(index + 1)
      index += 2

    found

  // Runs `program` over `input` from UTF-16 offset `start`, returning the winning thread's
  // capture slots, or `Unset` if nothing matches. Guaranteed O(|input| × |program|): each pc
  // joins each position's thread list at most once, and threads never backtrack. When
  // `anchoredStart` is unset, a fresh lowest-priority thread joins at each position until a
  // match is found, giving leftmost-first `seek` semantics; `anchoredEnd` requires the match
  // to reach the end of the input.
  def run[input: Symbolizer as symbolizer]
    ( program: Program, input: input, start: Int, anchoredStart: Boolean, anchoredEnd: Boolean )
  :   Optional[scala.IArray[Int]] =

    val ops = program.ops
    val length = symbolizer.length(input)
    var clist = Threads(ops.length)
    var nlist = Threads(ops.length)
    var matched: Optional[scala.Array[Int]] = Unset
    var index = start

    def word(at: Int): Boolean = at < length && symbolizer.word(symbolizer.symbol(input, at))
    def wordBefore(at: Int): Boolean = at > 0 && symbolizer.word(symbolizer.before(input, at))

    def test(anchor: Node.Anchor, at: Int): Boolean = anchor match
      case Node.Anchor.Start           => at == 0
      case Node.Anchor.End             => at == length
      case Node.Anchor.WordBoundary    => word(at) != wordBefore(at)
      case Node.Anchor.NonWordBoundary => word(at) == wordBefore(at)

      // Under the `m` flag, `^` and `$` match either end of the input or a newline boundary.
      case Node.Anchor.LineStart =>
        at == 0 || symbolizer.before(input, at) == '\n'.toInt

      case Node.Anchor.LineEnd =>
        at == length || symbolizer.symbol(input, at) == '\n'.toInt

    def add(list: Threads, pc: Int, saved: scala.Array[Int], at: Int): Unit =
      if !list.marked(pc) then
        list.mark(pc)

        ops(pc) match
          case Program.Op.Jump(target) =>
            add(list, target, saved, at)

          case Program.Op.Split(preferred, alternate) =>
            add(list, preferred, saved, at)
            add(list, alternate, saved, at)

          case Program.Op.Save(slot, next) =>
            val copy = saved.clone()
            copy(slot) = at
            add(list, next, copy, at)

          case Program.Op.Test(anchor, next) =>
            if test(anchor, at) then add(list, next, saved, at)

          case _ =>
            list.push(pc, saved)

    def fresh: scala.Array[Int] =
      val array = new scala.Array[Int](program.slots)
      java.util.Arrays.fill(array, -1)
      array

    // Unanchored seeking skips positions at which no match can begin: by jumping between
    // occurrences of the pattern's forced literal prefix (a vectorized `indexOf`), or failing
    // that, by scanning for a symbol admissible as a first symbol. Sound because a position
    // whose symbol no opening op accepts cannot begin a match; disabled for nullable patterns,
    // which match (emptily) everywhere.
    val skipping = !anchoredStart && !program.nullable && program.firstBounds.length > 0
    val prefix = if skipping then program.prefix else "".tt

    def candidate(from: Int): Int =
      if !skipping || from > length then from
      else if prefix.s.length > 0 then
        val found = symbolizer.find(input, prefix, from)
        if found < 0 then length + 1 else found
      else
        var at = from

        while at < length && !within(program.firstBounds, symbolizer.symbol(input, at))
        do at = symbolizer.next(input, at)

        if at < length then at else length + 1

    if anchoredStart then add(clist, program.entry, fresh, index) else
      index = candidate(index)
      if index <= length then add(clist, program.entry, fresh, index)

    var done = false

    while !done do
      val following = if index < length then symbolizer.next(input, index) else index + 1
      var thread = 0
      var cut = false

      while !cut && thread < clist.size do
        val pc = clist.pcs(thread)
        val saved = clist.regs(thread)

        ops(pc) match
          case Program.Op.Symbol(bounds, next) =>
            if index < length && within(bounds, symbolizer.symbol(input, index))
            then add(nlist, next, saved, following)

          case Program.Op.Accept =>
            // Threads later in the list are alternatives a backtracking matcher would try
            // only after this one, so a match here supersedes all of them.
            if !anchoredEnd || index == length then
              matched = saved
              cut = true

          case _ =>
            ()

        thread += 1

      if index >= length then done = true else
        index = following
        val swap = clist
        clist = nlist
        nlist = swap
        nlist.clear()

        if !anchoredStart && matched.absent then
          if clist.size == 0 then index = candidate(index)
          if index <= length then add(clist, program.entry, fresh, index) else done = true

        if (anchoredStart || matched.present) && clist.size == 0 then done = true

    matched.let: saved => scala.IArray.unsafeFromArray(saved.clone())
