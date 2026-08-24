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
import contingency.*
import rudiments.*

import Motif.Error.Reason.*
import Motif.Node

object Program:
  // A ceiling on compiled size: bounded repetitions compile by expansion, so nested `{n,m}`
  // forms can grow geometrically. RE2 imposes a similar program-size limit.
  val maxOps: Int = 100000

  def compile(node: Node, captures: Int): Program raises Motif.Error =
    val ops: ArrayBuffer[Program.Op] = ArrayBuffer()

    def emit(op: Program.Op): Int =
      if ops.size >= maxOps then abort(Motif.Error(0, RepetitionTooLarge))
      ops += op
      ops.size - 1

    // Fills a `-1` hole in a previously-emitted branching op.
    def patch(at: Int, target: Int): Unit = ops(at) = ops(at) match
      case Program.Op.Split(preferred, alternate) =>
        Program.Op.Split
          ( if preferred == -1 then target else preferred,
            if alternate == -1 then target else alternate )

      case Program.Op.Jump(-1) => Program.Op.Jump(target)
      case other               => other

    def bounds(ranges: Ranges): scala.IArray[Int] = scala.IArray.from(ranges.spans.stdlib)

    // Emits the code for one node. Fragments are contiguous and fall through to whatever is
    // emitted next, so a consuming op's `next` is always its own pc + 1; only loops and
    // alternations jump.
    def walk(node: Node): Unit = node match
      case Node.Empty =>
        ()

      case Node.Literal(codepoint) =>
        emit(Program.Op.Symbol(scala.IArray(codepoint, codepoint), ops.size + 1))

      case Node.Klass(ranges) =>
        emit(Program.Op.Symbol(bounds(ranges), ops.size + 1))

      case Node.Boundary(anchor) =>
        emit(Program.Op.Test(anchor, ops.size + 1))

      case Node.Sequence(nodes) =>
        nodes.each(walk(_))

      case Node.Group(child, index) => index match
        case index: Int =>
          emit(Program.Op.Save(2*index, ops.size + 1))
          walk(child)
          emit(Program.Op.Save(2*index + 1, ops.size + 1))

        case _ =>
          walk(child)

      case Node.Alternation(options) =>
        def recur(options: List[Node]): Unit = options.absolve match
          case only :: Nil =>
            walk(only)

          case head :: tail =>
            val split = emit(Program.Op.Split(ops.size + 1, -1))
            walk(head)
            val jump = emit(Program.Op.Jump(-1))
            patch(split, ops.size)
            recur(tail)
            patch(jump, ops.size)

          case Nil =>
            ()

        recur(options)

      case Node.Repeat(child, minimum, maximum, reluctant) =>
        def split(): Int =
          if reluctant then emit(Program.Op.Split(-1, ops.size + 1))
          else emit(Program.Op.Split(ops.size + 1, -1))

        var count = 0

        while count < minimum do
          walk(child)
          count += 1

        maximum match
          case maximum: Int =>
            var holes: List[Int] = Nil

            while count < maximum do
              holes = split() :: holes
              walk(child)
              count += 1

            holes.each(patch(_, ops.size))

          case _ =>
            val loop = ops.size
            val hole = split()
            walk(child)
            emit(Program.Op.Jump(loop))
            patch(hole, ops.size)

    emit(Program.Op.Save(0, 1))
    walk(node)
    emit(Program.Op.Save(1, ops.size + 1))
    emit(Program.Op.Accept)

    Program(scala.IArray.from(ops), 2*(captures + 1), captures)

  // One instruction of a compiled program. Only `Symbol` consumes input; every other op is
  // followed at no cost during the Pike VM's ε-closure. `bounds` is the interleaved
  // `lo, hi, lo, hi, …` form of a `Ranges`, flattened for allocation-free scanning; `Split`
  // lists its `preferred` target first, which is how greed and reluctance are encoded.
  enum Op:
    case Symbol(bounds: scala.IArray[Int], next: Int)
    case Split(preferred: Int, alternate: Int)
    case Jump(target: Int)
    case Save(slot: Int, next: Int)
    case Test(anchor: Node.Anchor, next: Int)
    case Accept

// A regular expression compiled to a flat, array-indexed instruction sequence, executable by the
// Pike VM and analyzable by the containment checker. Capture group `n` records its bounds in
// slots `2n` and `2n + 1`; slots `0` and `1` are the whole match.
case class Program(ops: scala.IArray[Program.Op], slots: Int, captures: Int):
  def entry: Int = 0

  // The consuming ops reachable from `pcs` without consuming input, and whether `Accept` is
  // among the reachable ops. Zero-width tests are followed unconditionally, so the symbol set
  // is a superset and the nullability flag an overestimate — both are the safe directions for
  // the seek acceleration below, which they exist to serve.
  private def reach(pcs: List[Int]): (List[Int], Boolean) =
    val visited = new scala.Array[Boolean](ops.length)
    var stack: List[Int] = pcs
    var symbols: List[Int] = Nil
    var accepts = false

    while stack != Nil do stack.absolve match
      case pc :: rest =>
        stack = rest

        if !visited(pc) then
          visited(pc) = true

          ops(pc) match
            case Program.Op.Jump(target)       => stack = target :: stack
            case Program.Op.Split(left, right) => stack = left :: right :: stack
            case Program.Op.Save(_, next)      => stack = next :: stack
            case Program.Op.Test(_, next)      => stack = next :: stack
            case Program.Op.Symbol(_, _)       => symbols = pc :: symbols
            case Program.Op.Accept             => accepts = true

    (symbols, accepts)

  private lazy val opening: (List[Int], Boolean) = reach(entry :: Nil)

  // Whether a match may consume nothing, in which case no position can be skipped.
  lazy val nullable: Boolean = opening(1)

  // The interleaved bounds of every symbol which can begin a match, coalesced.
  lazy val firstBounds: scala.IArray[Int] =
    var spans: List[Int] = Nil

    opening(0).each: pc =>
      ops(pc).absolve match
        case Program.Op.Symbol(bounds, _) =>
          var index = 0

          while index < bounds.length do
            spans = Ranges(bounds(index), bounds(index + 1)).spans.absolve match
              case lo :: hi :: Nil => lo :: hi :: spans
              case _               => spans

            index += 2

        case _ =>
          ()

    var merged = Ranges.empty
    var todo = spans

    while todo != Nil do todo.absolve match
      case lo :: hi :: rest =>
        merged = merged.union(Ranges(lo, hi))
        todo = rest

    scala.IArray.from(merged.spans.stdlib)

  // The longest literal every match must begin with (UTF-16-encoded, capped at 32 chars), or
  // empty if the first consuming op is not unique and single-valued. Seeking jumps between
  // occurrences of this literal with `Symbolizer.find` rather than stepping positions.
  lazy val prefix: Text =
    val builder = StringBuilder()

    def recur(pcs: List[Int], accepts: Boolean): Unit =
      if !accepts && builder.length < 32 then pcs match
        case pc :: Nil => ops(pc).absolve match
          case Program.Op.Symbol(bounds, next) =>
            if bounds.length == 2 && bounds(0) == bounds(1) then
              builder.append(new String(Character.toChars(bounds(0))))
              val (symbols, accepts2) = reach(next :: Nil)
              recur(symbols, accepts2)

        case _ =>
          ()

    recur(opening(0), nullable)
    builder.toString.tt
