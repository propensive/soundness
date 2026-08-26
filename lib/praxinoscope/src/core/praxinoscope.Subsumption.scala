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

import scala.collection.mutable.{ArrayBuffer, HashSet}

import contingency.*
import rudiments.*

import Motif.Error.Reason.*
import Motif.Node

object Subsumption:
  // Containment of regular languages is decidable but worst-case exponential in the size of the
  // subsuming program (the exploration determinizes it on the fly), so exploration is budgeted;
  // exceeding the budget raises `BudgetExceeded` rather than running unbounded.
  val maxStates: Int = 100000

  // Wraps one program for exploration. `symbols` is the ε-closure restricted to consuming ops
  // (`$` is impassable mid-input); `accepts` asks whether `Accept` is ε-reachable if the input
  // is permitted to end here. `first` gates `^`, which only passes before any symbol has been
  // consumed. Word boundaries are context-dependent in a way this analysis does not model, so
  // programs containing them are rejected as `Unverifiable` upstream.
  private class Analysis(program: Program):
    val ops: scala.IArray[Program.Op] = program.ops

    private def traverse(pcs: List[Int], first: Boolean, ending: Boolean): (List[Int], Boolean) =
      val visited = new scala.Array[Boolean](ops.length)
      var stack: List[Int] = pcs
      var symbols: List[Int] = Nil
      var accepted = false

      while stack != Nil do stack match
        case pc :: rest =>
          stack = rest

          if !visited(pc) then
            visited(pc) = true

            ops(pc) match
              case Program.Op.Jump(target)        => stack = target :: stack
              case Program.Op.Split(left, right)  => stack = left :: right :: stack
              case Program.Op.Save(_, next)       => stack = next :: stack
              case Program.Op.Symbol(_, _)        => symbols = pc :: symbols
              case Program.Op.Accept              => accepted = true

              case Program.Op.Test(anchor, next) => anchor match
                case Node.Anchor.Start => if first then stack = next :: stack
                case Node.Anchor.End   => if ending then stack = next :: stack
                case _                 => ()

        case _ =>
          ()

      (symbols, accepted)

    def symbols(pcs: List[Int], first: Boolean): List[Int] =
      canonical(traverse(pcs, first, false)(0))

    def accepts(pcs: List[Int], first: Boolean): Boolean = traverse(pcs, first, true)(1)

    // The raw successor pcs of `symbolPcs` on `symbol`.
    def step(symbolPcs: List[Int], symbol: Int): List[Int] =
      var successors: List[Int] = Nil

      symbolPcs.each: pc =>
        ops(pc).absolve match
          case Program.Op.Symbol(bounds, next) =>
            if within(bounds, symbol) then successors = next :: successors

      successors

    def bounds(pc: Int): scala.IArray[Int] = ops(pc).absolve match
      case Program.Op.Symbol(bounds, _) => bounds

  private def canonical(pcs: List[Int]): List[Int] = pcs.stdlib.distinct.sorted.to(List)

  private def within(bounds: scala.IArray[Int], symbol: Int): Boolean =
    var index = 0
    var found = false

    while !found && index < bounds.length do
      found = symbol >= bounds(index) && symbol <= bounds(index + 1)
      index += 2

    found

  private def check(program: Program): Unit raises Motif.Error =
    var index = 0

    while index < program.ops.length do
      program.ops(index) match
        case Program.Op.Test(Node.Anchor.WordBoundary | Node.Anchor.NonWordBoundary, _) =>
          abort(Motif.Error(0, Unverifiable))

        case _ =>
          ()

      index += 1

  // One representative symbol per equivalence class of the programs' transition ranges: within
  // a class, every `Symbol` op either accepts all members or none, so exploring one
  // representative per class covers the whole alphabet.
  private def representatives(programs: scala.List[Program]): scala.Array[Int] =
    val cuts = HashSet[Int](0)

    def collect(program: Program): Unit =
      var index = 0

      while index < program.ops.length do
        program.ops(index) match
          case Program.Op.Symbol(bounds, _) =>
            var span = 0

            while span < bounds.length do
              cuts += bounds(span)
              if bounds(span + 1) < Ranges.maxSymbol then cuts += bounds(span + 1) + 1
              span += 2

          case _ =>
            ()

        index += 1

    programs.foreach(collect(_))
    val result = cuts.toArray
    java.util.Arrays.sort(result)
    result

  // Every combination taking one pc from each candidate's option list: the states of the
  // candidates' product automaton. Empty if any candidate has no option, which is exactly the
  // case where no word can continue through all of them at once.
  private def combinations(options: scala.Array[scala.List[Int]]): scala.List[scala.List[Int]] =
    var result: scala.List[scala.List[Int]] = scala.List(scala.Nil)
    var index = options.length - 1

    while index >= 0 do
      val tails = result
      var built: scala.List[scala.List[Int]] = scala.Nil
      var choices = options(index)

      while choices != scala.Nil do
        var rest = tails

        while rest != scala.Nil do
          built = (choices.head :: rest.head) :: built
          rest = rest.tail

        choices = choices.tail

      result = built
      index -= 1

    result

  // Decides whether every input matched by `second` is also matched by `first`.
  def subsumes(first: Program, second: Program): Boolean raises Motif.Error =
    subsumes(first, second :: Nil)

  // Decides whether every input matched by *all* of `candidates` is also matched by `cover`,
  // i.e. `L(⋂candidates) ⊆ L(cover)`. RE2 has no intersection operator, so an intersection can
  // only arise from several patterns applied together; deciding containment for one directly
  // avoids having to express `⋂candidates` as a single pattern, which is not always possible.
  //
  // Each candidate is explored pc-by-pc while `cover` is determinized on the fly. That asymmetry
  // is the point: a word is in `⋂candidates` iff *every* candidate has some accepting path on
  // it, so a tuple of individual candidate paths is exactly the right witness; whereas a word
  // escapes `cover` only if *no* cover path accepts, which needs the subset construction.
  // Containment fails iff some reachable product state accepts in every candidate but not in
  // `cover`.
  def subsumes(cover: Program, candidates: List[Program]): Boolean raises Motif.Error =
    check(cover)
    candidates.each(check(_))

    val covering = Analysis(cover)
    val candidacy = candidates.stdlib.map(Analysis(_)).toArray
    val reps = representatives(cover :: candidates.stdlib.to(scala.List))
    val width = candidacy.length

    def everyCandidateAccepts(raw: scala.Array[List[Int]]): Boolean =
      var index = 0
      var accepted = true

      while accepted && index < width do
        accepted = candidacy(index).accepts(raw(index), false)
        index += 1

      accepted

    // An empty candidate list intersects to Σ*, which only an everything-matching `cover`
    // contains. Rather than decide that case, report not-contained: callers reach this
    // only by asking a degenerate question, and failing closed is always the safe answer.
    if width == 0 then false
    else if candidacy.forall(_.accepts(0 :: Nil, true)) && !covering.accepts(0 :: Nil, true)
    then false
    else
      val visited = HashSet[(scala.List[Int], List[Int])]()
      val queue = ArrayBuffer[(scala.List[Int], List[Int])]()
      var head = 0
      var result = true

      val coverStart = covering.symbols(0 :: Nil, true)

      combinations(candidacy.map(_.symbols(0 :: Nil, true).stdlib.to(scala.List))).foreach: pcs =>
        if visited.add((pcs, coverStart)) then queue += ((pcs, coverStart))

      while result && head < queue.size do
        if visited.size > maxStates then abort(Motif.Error(0, BudgetExceeded))
        val (pcs, coverSet) = queue(head)
        head += 1
        val current = pcs.toArray
        var rep = 0

        while result && rep < reps.length do
          val symbol = reps(rep)
          rep += 1

          // A word continues through the intersection only where every candidate admits the
          // symbol; where one does not, no word in `⋂candidates` passes through this state.
          var index = 0
          var admitted = true

          while admitted && index < width do
            admitted = within(candidacy(index).bounds(current(index)), symbol)
            index += 1

          if admitted then
            val nextRaw = scala.Array.tabulate(width): index =>
              candidacy(index).step(current(index) :: Nil, symbol)

            val coverRaw = covering.step(coverSet, symbol)

            if everyCandidateAccepts(nextRaw) && !covering.accepts(coverRaw, false)
            then result = false
            else
              val coverNext = covering.symbols(coverRaw, false)

              val onward = scala.Array.tabulate(width): index =>
                candidacy(index).symbols(nextRaw(index), false).stdlib.to(scala.List)

              combinations(onward).foreach: pcs2 =>
                if visited.add((pcs2, coverNext)) then queue += ((pcs2, coverNext))

      result

  // Decides whether some input is matched by both programs: reachability, in the product of
  // the two NFAs, of a point where both accept. Polynomial, unlike `subsumes`.
  def intersects(first: Program, second: Program): Boolean raises Motif.Error =
    check(first)
    check(second)

    val left = Analysis(first)
    val right = Analysis(second)
    val reps = representatives(scala.List(first, second))

    if left.accepts(0 :: Nil, true) && right.accepts(0 :: Nil, true) then true else
      val visited = HashSet[(Int, Int)]()
      val queue = ArrayBuffer[(Int, Int)]()
      var head = 0
      var result = false

      val leftStart = left.symbols(0 :: Nil, true)
      val rightStart = right.symbols(0 :: Nil, true)

      leftStart.each: pcLeft =>
        rightStart.each: pcRight =>
          if visited.add((pcLeft, pcRight)) then queue += ((pcLeft, pcRight))

      while !result && head < queue.size do
        if visited.size > maxStates then abort(Motif.Error(0, BudgetExceeded))
        val (pcLeft, pcRight) = queue(head)
        head += 1
        val leftBounds = left.bounds(pcLeft)
        val rightBounds = right.bounds(pcRight)
        var rep = 0

        while !result && rep < reps.length do
          val symbol = reps(rep)
          rep += 1

          if within(leftBounds, symbol) && within(rightBounds, symbol) then
            val leftRaw = left.step(pcLeft :: Nil, symbol)
            val rightRaw = right.step(pcRight :: Nil, symbol)

            if left.accepts(leftRaw, false) && right.accepts(rightRaw, false)
            then result = true
            else
              left.symbols(leftRaw, false).each: pcLeft2 =>
                right.symbols(rightRaw, false).each: pcRight2 =>
                  if visited.add((pcLeft2, pcRight2)) then queue += ((pcLeft2, pcRight2))

      result
