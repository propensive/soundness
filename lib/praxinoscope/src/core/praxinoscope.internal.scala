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

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.quoted.*

import anticipation.*
import gigantism.Lifts
import vacuous.*

import Motif.Node

object internal:
  private def integers(values: Array[Int])(using Quotes): Expr[Array[Int]] =
    var exprs: List[Expr[Int]] = Nil
    var index = values.length - 1

    while index >= 0 do
      exprs = Expr(values(index)) :: exprs
      index -= 1

    '{Array(${Lifts.varargs(exprs)}*)}

  private def booleans(values: Array[Boolean])(using Quotes): Expr[Array[Boolean]] =
    var exprs: List[Expr[Boolean]] = Nil
    var index = values.length - 1

    while index >= 0 do
      exprs = Expr(values(index)) :: exprs
      index -= 1

    '{Array(${Lifts.varargs(exprs)}*)}

  given anchor: ToExpr[Node.Anchor]:
    def apply(anchor: Node.Anchor)(using Quotes): Expr[Node.Anchor] = anchor match
      case Node.Anchor.Start           => '{Node.Anchor.Start}
      case Node.Anchor.End             => '{Node.Anchor.End}
      case Node.Anchor.WordBoundary    => '{Node.Anchor.WordBoundary}
      case Node.Anchor.NonWordBoundary => '{Node.Anchor.NonWordBoundary}
      case Node.Anchor.LineStart       => '{Node.Anchor.LineStart}
      case Node.Anchor.LineEnd         => '{Node.Anchor.LineEnd}

  given op: ToExpr[Program.Op]:
    def apply(op: Program.Op)(using Quotes): Expr[Program.Op] = op match
      case Program.Op.Symbol(bounds, next) =>
        '{Program.Op.Symbol(${integers(bounds)}, ${Expr(next)})}

      case Program.Op.Split(preferred, alternate) =>
        '{Program.Op.Split(${Expr(preferred)}, ${Expr(alternate)})}

      case Program.Op.Jump(target)       => '{Program.Op.Jump(${Expr(target)})}
      case Program.Op.Save(slot, next)   => '{Program.Op.Save(${Expr(slot)}, ${Expr(next)})}
      case Program.Op.Test(anchor, next) => '{Program.Op.Test(${Expr(anchor)}, ${Expr(next)})}
      case Program.Op.Accept             => '{Program.Op.Accept}

  given program: ToExpr[Program]:
    def apply(program: Program)(using Quotes): Expr[Program] =
      var exprs: List[Expr[Program.Op]] = Nil
      var index = program.ops.length - 1

      while index >= 0 do
        exprs = Expr(program.ops.readUnchecked(index)) :: exprs
        index -= 1

      ' {
          Program
            ( Array(${Lifts.varargs(exprs)}*),
              ${Expr(program.slots)},
              ${Expr(program.captures)} )
        }

  given motif: ToExpr[Motif]:
    def apply(motif: Motif)(using Quotes): Expr[Motif] =
      '{Motif.staged(${Expr(motif.pattern.s)}.tt, ${Expr(motif.captures)}, ${Expr(motif.program)})}

  // The cap on DFA size, in states, above which `matcher` declines to generate code and the
  // caller falls back to the staged Pike VM program.
  val maxStates: Int = 512

  // Generates a specialized matcher for `program`: a whole-input boolean match compiled by
  // subset construction to a dense DFA transition table over the pattern's symbol classes,
  // executed by a tight, allocation-free loop. Returns `Unset` when the program uses word
  // boundaries (which the DFA does not model) or when subset construction exceeds `maxStates`.
  def matcher(program: Program)(using Quotes): Optional[Expr[Fsa]] =
    val ops = program.ops
    var unsupported = false

    var opIndex = 0

    while opIndex < ops.length do
      ops.readUnchecked(opIndex) match
        // The generated DFA has no notion of the symbol before the cursor, so a
        // context-dependent anchor falls back to the Pike VM.
        case Program.Op.Test
          ( Node.Anchor.WordBoundary | Node.Anchor.NonWordBoundary | Node.Anchor.LineStart
            | Node.Anchor.LineEnd, _ ) =>
          unsupported = true

        case _ =>
          ()

      opIndex += 1

    if unsupported then Unset else
      // Partition the alphabet into classes over which every transition is uniform.
      val cutSet = scala.collection.mutable.SortedSet[Int](0)

      opIndex = 0

      while opIndex < ops.length do
        ops.readUnchecked(opIndex) match
          case Program.Op.Symbol(bounds, _) =>
            var span = 0

            while span < bounds.length do
              cutSet += bounds.readUnchecked(span)
              val upper = bounds.readUnchecked(span + 1)
              if upper < Ranges.maxSymbol then cutSet += upper + 1
              span += 2

          case _ =>
            ()

        opIndex += 1

      val cuts: Array[Int] = Array.from(cutSet)
      val classes = cuts.length

      // The ε-closure of `pcs`: reachable `Symbol` pcs, and whether `Accept` is reachable when
      // the input may end here. `first` gates `^`; `$` is followed only for acceptance.
      def close(pcs: scala.List[Int], first: Boolean): (scala.List[Int], Boolean) =
        val visited = new scala.Array[Boolean](ops.length)
        var stack = pcs
        var symbols: scala.List[Int] = scala.Nil
        var accepts = false

        while stack.nonEmpty do
          val pc = stack.head
          stack = stack.tail

          if !visited(pc) then
            visited(pc) = true

            ops.readUnchecked(pc) match
              case Program.Op.Jump(target)       => stack = target :: stack
              case Program.Op.Split(left, right) => stack = left :: right :: stack
              case Program.Op.Save(_, next)      => stack = next :: stack
              case Program.Op.Symbol(_, _)       => symbols = pc :: symbols
              case Program.Op.Accept             => accepts = true

              case Program.Op.Test(anchor, next) => anchor match
                case Node.Anchor.Start => if first then stack = next :: stack
                case Node.Anchor.End   => stack = next :: stack
                case _                 => ()

        (symbols.sorted, accepts)

      // `$` must not admit further consumption, so acceptance comes from an End-permissive
      // closure while transitions come only from Symbol pcs reachable without passing `$`.
      def interior(pcs: scala.List[Int], first: Boolean): scala.List[Int] =
        val visited = new scala.Array[Boolean](ops.length)
        var stack = pcs
        var symbols: scala.List[Int] = scala.Nil

        while stack.nonEmpty do
          val pc = stack.head
          stack = stack.tail

          if !visited(pc) then
            visited(pc) = true

            ops.readUnchecked(pc) match
              case Program.Op.Jump(target)       => stack = target :: stack
              case Program.Op.Split(left, right) => stack = left :: right :: stack
              case Program.Op.Save(_, next)      => stack = next :: stack
              case Program.Op.Symbol(_, _)       => symbols = pc :: symbols

              case Program.Op.Test(anchor, next) =>
                if anchor == Node.Anchor.Start && first then stack = next :: stack

              case _ =>
                ()

        symbols.sorted

      def within(bounds: Array[Int], symbol: Int): Boolean =
        var index = 0
        var found = false

        while !found && index < bounds.length do
          found = symbol >= bounds.readUnchecked(index) && symbol <= bounds.readUnchecked(index + 1)
          index += 2

        found

      // A state is its interior Symbol pcs plus its acceptance: two subsets with identical
      // transitions but different acceptance must not merge.
      val ids = HashMap[(scala.List[Int], Boolean, Boolean), Int]()
      val work = ArrayBuffer[(scala.List[Int], Boolean)]()
      val transitions = ArrayBuffer[scala.Array[Int]]()
      val accepting = ArrayBuffer[Boolean]()
      var overflow = false

      def stateId(pcs: scala.List[Int], first: Boolean): Int =
        val interiorPcs = interior(pcs, first)
        val accepts = close(pcs, first)(1)

        ids.getOrElseUpdate
          ( (interiorPcs, first, accepts),
            { val id = ids.size
              work += ((interiorPcs, first))
              transitions += new scala.Array[Int](classes)
              accepting += accepts
              id } )

      stateId(0 :: scala.Nil, true)
      var next = 0

      while !overflow && next < work.size do
        val (pcs, first) = work(next)
        val id = next
        next += 1

        var klass = 0

        while klass < classes do
          val symbol = cuts.readUnchecked(klass)
          var successors: scala.List[Int] = scala.Nil

          pcs.foreach: pc =>
            ops.readUnchecked(pc) match
              case Program.Op.Symbol(bounds, target) =>
                if within(bounds, symbol) then successors = target :: successors

              case _ =>
                ()

          transitions(id)(klass) =
            if successors.isEmpty then -1 else stateId(successors, false)

          if ids.size > maxStates then overflow = true
          klass += 1

      if overflow then Unset else
        val states = ids.size
        val table = new scala.Array[Int](states*classes)

        var state = 0

        while state < states do
          java.lang.System.arraycopy(transitions(state), 0, table, state*classes, classes)
          state += 1

        // Direct-indexed class map for the ASCII range, so the common case classifies a symbol
        // with one array load; only symbols ≥ 128 fall back to binary search over `cuts`.
        val ascii = new scala.Array[Int](128)
        var char = 0

        while char < 128 do
          var lower = 0
          var upper = cuts.length - 1

          while lower < upper do
            val middle = (lower + upper + 1) >> 1
            if cuts.readUnchecked(middle) <= char then lower = middle else upper = middle - 1

          ascii(char) = lower
          char += 1

        val asciiExpr = integers(Array.unsafeFrozen(ascii))
        val tableExpr = integers(Array.unsafeFrozen(table))
        val cutsExpr = integers(cuts)

        val acceptingExpr =
          val array = new scala.Array[Boolean](states)
          var index = 0

          while index < states do
            array(index) = accepting(index)
            index += 1

          booleans(Array.unsafeFrozen(array))

        val result: Expr[Fsa] =
          ' {
              val cuts = $cutsExpr
              val ascii = $asciiExpr
              val table = $tableExpr
              val accepts = $acceptingExpr

              Fsa: text =>
                val input = text.s
                val length = input.length
                var index = 0
                var state = 0

                while state >= 0 && index < length do
                  val symbol = input.codePointAt(index)
                  index += Character.charCount(symbol)

                  val klass =
                    if symbol < 128 then ascii.readUnchecked(symbol) else
                      var lower = 0
                      var upper = cuts.length - 1

                      while lower < upper do
                        val middle = (lower + upper + 1) >> 1

                        if cuts.readUnchecked(middle) <= symbol then lower = middle
                        else upper = middle - 1

                      lower

                  state = table.readUnchecked(state*${Expr(classes)} + klass)

                state >= 0 && accepts.readUnchecked(state)
            }

        result
