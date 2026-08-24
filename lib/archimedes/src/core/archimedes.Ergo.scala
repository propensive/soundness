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
package archimedes

import scala.collection.immutable.Seq

import scala.math

// Deliberate stdlib opt-out, as in `Cell`.
import scala.collection.immutable.{Map, Set}

import scala.collection.mutable.ListBuffer

import anticipation.*
import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import rudiments.*
import vacuous.*

import Mathml.*
import denominative.*
import denominative.dysasymptotics.linearSize

// A parser for "ergo": a one-line shorthand for Presentation MathML that emits
// `archimedes` nodes. The whole expression is delimited by a bracket pair; the
// first character chooses which pair (`(`/`[`/`{`/`⟨`) acts as grouping syntax,
// and every other bracket is a literal `Mo`. Grouped runs are `Mrow` units.
//
// Operators, tightest-binding first:
//   √ prefix              → Msqrt        (an index atom immediately before √ → Mroot)
//   ↗ ↘ (corner scripts)  → Msup/Msub    (one of each on a base merges → Msubsup)
//   ↑ ↓ (centred limits)  → Mover/Munder (one of each merges → Munderover)
//   /                     → Mfrac
//   juxtaposition         → Mrow
//
// Introducers ⋱/⋯/⋮ start a matrix / row-vector / column-vector from the run of
// bracket groups that immediately follows. A space separates adjacent atoms
// (`x y` = two `<mi>`, `xy` = one), and any operator glyph with a missing
// operand degrades to a literal `Mo` (so `(↗)` writes a literal ↗).

object Ergo:
  private val pairs: Map[Char, Char] =
    Map('(' -> ')', '[' -> ']', '{' -> '}', '⟨' -> '⟩')

  private final val Sup    = '↗' // ↗
  private final val Sub    = '↘' // ↘
  private final val Over   = '↑' // ↑
  private final val Under  = '↓' // ↓
  private final val Radical = '√' // √
  private final val Matrix = '⋱' // ⋱
  private final val RowVec = '⋯' // ⋯
  private final val ColVec = '⋮' // ⋮
  private final val Hole   = '\uE000' // interpolator substitution placeholder

  // A postfix attribute directive (see `doc/ergo-directives.md`). `Fixed` sets a
  // constant value with a bare glyph (one glyph per enumerated/boolean value);
  // `Param` reads an open value (length/colour/integer) in the active bracket.
  private enum Directive(val attribute: Text):
    case Param(name: Text)              extends Directive(name)
    case Fixed(name: Text, value: Text) extends Directive(name)

  private val directives: List[(Char, Directive)] = List(
    '⧆' -> Directive.Fixed(t"displaystyle", t"true"),
    '⧄' -> Directive.Fixed(t"displaystyle", t"false"),
    '⌄' -> Directive.Param(t"scriptlevel"),
    '◻' -> Directive.Fixed(t"display", t"block"),
    '▭' -> Directive.Fixed(t"display", t"inline"),
    '●' -> Directive.Param(t"mathcolor"),
    '▨' -> Directive.Param(t"mathbackground"),
    '⟑' -> Directive.Param(t"mathsize"),
    '⦱' -> Directive.Fixed(t"mathvariant", t"normal"),
    '⊩' -> Directive.Fixed(t"dir", t"ltr"),
    '⫣' -> Directive.Fixed(t"dir", t"rtl"),
    '⊰' -> Directive.Fixed(t"form", t"prefix"),
    '⊹' -> Directive.Fixed(t"form", t"infix"),
    '⊱' -> Directive.Fixed(t"form", t"postfix"),
    '∥' -> Directive.Fixed(t"fence", t"true"),
    '∤' -> Directive.Fixed(t"fence", t"false"),
    '▮' -> Directive.Fixed(t"separator", t"true"),
    '▯' -> Directive.Fixed(t"separator", t"false"),
    '⇿' -> Directive.Fixed(t"stretchy", t"true"),
    '↮' -> Directive.Fixed(t"stretchy", t"false"),
    '⋈' -> Directive.Fixed(t"symmetric", t"true"),
    '⋊' -> Directive.Fixed(t"symmetric", t"false"),
    '◆' -> Directive.Fixed(t"largeop", t"true"),
    '◇' -> Directive.Fixed(t"largeop", t"false"),
    '⧳' -> Directive.Fixed(t"movablelimits", t"true"),
    '⧯' -> Directive.Fixed(t"movablelimits", t"false"),
    '⧔' -> Directive.Param(t"lspace"),
    '⧕' -> Directive.Param(t"rspace"),
    '⟰' -> Directive.Param(t"maxsize"),
    '⟱' -> Directive.Param(t"minsize"),
    '↔' -> Directive.Param(t"width"),
    '⍏' -> Directive.Param(t"height"),
    '⍖' -> Directive.Param(t"depth"),
    '↕' -> Directive.Param(t"voffset"),
    '═' -> Directive.Param(t"linethickness"),
    '◠' -> Directive.Fixed(t"accent", t"true"),
    '⌢' -> Directive.Fixed(t"accent", t"false"),
    '◡' -> Directive.Fixed(t"accentunder", t"true"),
    '⌣' -> Directive.Fixed(t"accentunder", t"false"),
    '⚙' -> Directive.Param(t"actiontype"))

  private def directive(char: Char): Optional[Directive] =
    directives.stdlib.collectFirst { case (glyph, entry) if glyph == char => entry }.optional

  def parse(input: Text)(using Tactic[Ergo.Error]): Math =
    Parser(input.s, Iterator.empty).parseTop()

  // Parses an interpolated `ergo""` literal: the `parts` are joined with a hole
  // sentinel, and each hole yields the corresponding already-encoded `atoms` node
  // as a single atom, in left-to-right order.
  def interpolate(parts: Seq[Text], atoms: Seq[Mathml])(using Tactic[Ergo.Error]): Math =
    Parser(parts.map(_.s).mkString(Hole.toString), atoms.iterator).parseTop()

  // Parse-or-throw, for callers (in particular the `ergo""` interpolator's
  // generated code) that have already established the literal is well-formed.
  def unsafe(input: Text): Math =
    import strategies.throwUnsafely
    parse(input)

  def unsafeInterpolate(parts: Seq[Text], atoms: Seq[Mathml]): Math =
    import strategies.throwUnsafely
    interpolate(parts, atoms)

  // ── Serialisation: an archimedes `Math` tree → ergo shorthand text ──────────
  //
  // Emits the ergo-expressible subset of Presentation MathML (the elements Ergo
  // has structural syntax for, plus every MathML Core attribute as a directive).
  // An element outside that subset — e.g. `<mtext>`, `<mspace>`, `<mstyle>` — has
  // no ergo form and raises `Ergo.Error`. The root `<math>`'s own attributes are
  // not representable and are dropped.

  private val special: Set[Char] =
    Set('(', ')', '[', ']', '{', '}', '⟨', '⟩', '↗', '↘', '↑', '↓', '/', '√', '⋱', '⋯', '⋮')

  def serialize(math: Math)(using Tactic[Ergo.Error]): Text = t"(${sequence(math.contents)})"

  private def group(core: Text): Text = t"($core)"

  private def operand(node: Mathml)(using Tactic[Ergo.Error]): Text = emit(node, true)

  private def rowOf(nodes: List[Mathml]): Mathml = nodes match
    case one :: Nil => one
    case many       => Mrow(many)

  // Joins a juxtaposed run, inserting a space only where two tokens would
  // otherwise merge (letter+letter into one `<mi>`, or digit+digit into one `<mn>`).
  private def sequence(nodes: List[Mathml])(using Tactic[Ergo.Error]): Text =
    val parts = nodes.stdlib.map(emit(_, false)).filter(!_.s.isEmpty)

    parts.foldLeft(t""): (acc, next) =>
      if acc.s.isEmpty then next
      else if merges(acc.s.charAt(acc.s.length - 1), next.s.charAt(0)) then t"$acc $next"
      else t"$acc$next"

  private def merges(left: Char, right: Char): Boolean =
    Character.isLetter(left) && Character.isLetter(right) ||
      Character.isDigit(left) && Character.isDigit(right)

  // Wraps a compound in the grouping bracket when it is used as an operand or
  // carries attributes (so trailing directives bind to the whole unit).
  private def finish(core: Text, attributes: List[(Text, Text)], asOperand: Boolean, safe: Boolean)
  :   Text =

    if attributes.stdlib.nonEmpty then t"${group(core)}${directivesFor(attributes)}"
    else if asOperand && !safe then group(core)
    else core

  private def emit(node: Mathml, asOperand: Boolean)(using Tactic[Ergo.Error]): Text = node match
    case Mo(value, attributes) if value.s.length == 1 && special(value.s.charAt(0)) =>
      t"${group(value)}${directivesFor(attributes)}"

    case Mi(value, attributes) => t"$value${directivesFor(attributes)}"
    case Mn(value, attributes) => t"$value${directivesFor(attributes)}"
    case Mo(value, attributes) => t"$value${directivesFor(attributes)}"

    case Msqrt(contents, attributes) =>
      finish(t"√${operand(rowOf(contents))}", attributes, asOperand, true)

    case Mroot(base, index, attributes) =>
      finish(t"${operand(index)}√${operand(base)}", attributes, asOperand, true)

    case table: Mtable => finish(serializeTable(table), table.attributes, asOperand, true)

    case Mrow(contents, attributes) => finish(sequence(contents), attributes, asOperand, false)

    case Mfrac(numerator, denominator, attributes) =>
      finish(t"${operand(numerator)}/${operand(denominator)}", attributes, asOperand, false)

    case Msub(base, sub, attributes) =>
      finish(t"${operand(base)}↘${operand(sub)}", attributes, asOperand, false)

    case Msup(base, sup, attributes) =>
      finish(t"${operand(base)}↗${operand(sup)}", attributes, asOperand, false)

    case Msubsup(base, sub, sup, attributes) =>
      finish(t"${operand(base)}↘${operand(sub)}↗${operand(sup)}", attributes, asOperand, false)

    case Munder(base, under, attributes) =>
      val kept = accentless(attributes, under, t"accentunder")
      finish(t"${operand(base)}↓${operand(under)}", kept, asOperand, false)

    case Mover(base, over, attributes) =>
      val kept = accentless(attributes, over, t"accent")
      finish(t"${operand(base)}↑${operand(over)}", kept, asOperand, false)

    case Munderover(base, under, over, attributes) =>
      val kept = accentless(accentless(attributes, under, t"accentunder"), over, t"accent")
      finish(t"${operand(base)}↓${operand(under)}↑${operand(over)}", kept, asOperand, false)

    case other => scala.caps.unsafe.unsafeAssumeSeparate(abort(Ergo.Error(Ergo.Error.Reason.Unsupported(other.label))))

  // A `↑`/`↓` script that is a single `<mo>` re-acquires its accent on parsing, so
  // the corresponding accent attribute is implied and not emitted.
  private def accentless(attributes: List[(Text, Text)], script: Mathml, name: Text)
  :   List[(Text, Text)] =

    val accent = script match
      case _: Mo => true
      case _     => false

    if accent then List.of(attributes.stdlib.filter { pair => pair != (name, t"true") }) else attributes

  private def serializeTable(table: Mtable)(using Tactic[Ergo.Error]): Text =
    val rows: List[List[Text]] =
      // Not `sweep`: `cellText` captures the `Tactic`, so the synthesized partial function
      // cannot be pure. The `.stdlib` partial function has no such requirement.
      List.of:
        table.contents.stdlib.collect:
          case Mtr(cells, _) => cells.map(cellText)

    if rows.size == 1 then
      val row = rows.stdlib.head.join
      t"${RowVec.toString.tt}($row)"
    else if rows.all(_.size == 1) then
      val column = rows.map(_.stdlib.head).join
      t"${ColVec.toString.tt}($column)"
    else
      val body = rows.map { cells => group(cells.join) }.join
      t"${Matrix.toString.tt}($body)"

  private def cellText(node: Mathml)(using Tactic[Ergo.Error]): Text = node match
    case Mtd(contents, _) => group(sequence(contents))
    case other            => group(emit(other, false))

  private def directivesFor(attributes: List[(Text, Text)]): Text =
    List.of(attributes.stdlib.map { case (name, value) => directiveText(name, value) }
    . collect { case text: Text => text }).join

  // Prefers a `Fixed` glyph matching both attribute and value (enums/booleans);
  // otherwise a `Param` glyph for the attribute, with the value in brackets.
  private def directiveText(name: Text, value: Text): Optional[Text] =
    val matched = directives.stdlib.collectFirst:
      case (glyph, Directive.Fixed(n, v)) if n == name && v == value =>
        glyph.toString.tt

      case (glyph, Directive.Param(n)) if n == name =>
        t"${glyph.toString.tt}($value)"

    matched.optional

  private class Parser(s: String, holes: Iterator[Mathml])(using Tactic[Ergo.Error]):
    @scala.caps.unsafe.untrackedCaptures
    private var pos = 0
    @scala.caps.unsafe.untrackedCaptures
    private var open = '('
    @scala.caps.unsafe.untrackedCaptures
    private var close = ')'

    private def peek: Char = if pos < s.length then s.charAt(pos) else '\u0000'

    private def peekAt(offset: Int): Char =
      if pos + offset < s.length then s.charAt(pos + offset) else ' '

    private def advance(): Char =
      val c = peek
      pos += 1
      c

    private def skipSpaces(): Unit = while pos < s.length && s.charAt(pos) == ' ' do pos += 1

    private def letter(c: Char): Boolean = Character.isLetter(c)
    private def digit(c: Char): Boolean = Character.isDigit(c)

    def parseTop(): Math =
      if s.isEmpty then scala.caps.unsafe.unsafeAssumeSeparate(abort(Ergo.Error(Ergo.Error.Reason.Empty, pos)))
      open = s.charAt(0)

      if !pairs.contains(open)
      then scala.caps.unsafe.unsafeAssumeSeparate(abort(Ergo.Error(Ergo.Error.Reason.BadOpener(open.toString.tt), pos)))

      close = pairs(open)

      parseGroup() match
        case Mrow(nodes, _) => Math(nodes)
        case other          => Math(List(other))

    // Consumes `open … close`, returning the inner sequence (unwrapped if single).
    private def parseGroup(): Mathml =
      advance()
      val inner = parseSequence()
      skipSpaces()
      if peek != close then scala.caps.unsafe.unsafeAssumeSeparate(abort(Ergo.Error(Ergo.Error.Reason.Unclosed(close.toString.tt), pos)))
      advance()
      inner

    // Juxtaposition — the loosest binding; a run of fraction-level terms → Mrow.
    private def parseSequence(): Mathml =
      val terms = ListBuffer[Mathml]()
      skipSpaces()

      while pos < s.length && peek != close do
        terms += parseFraction()
        skipSpaces()

      terms.to(List) match
        case one :: Nil => one
        case many       => Mrow(many)

    private def parseFraction(): Mathml =
      var left = parseScripts()
      skipSpaces()

      while peek == '/' do
        advance()
        left = Mfrac(left, parseScripts())
        skipSpaces()

      left

    private def parseScripts(): Mathml =
      val base = parsePrimary()
      var sub, sup, under, over: Optional[Mathml] = Unset
      var scanning = true

      while scanning do
        skipSpaces()

        peek match
          case Sup if sup.absent     => advance(); sup = parsePrimary()
          case Sub if sub.absent     => advance(); sub = parsePrimary()
          case Over if over.absent   => advance(); over = parsePrimary()
          case Under if under.absent => advance(); under = parsePrimary()
          case _                     => scanning = false

      // An `↑`/`↓` script that is a single `Mo` (a bar/hat/vec-style mark) is an
      // accent; an identifier or group is an ordinary limit.
      def accent(script: Mathml, name: Text): List[(Text, Text)] = script match
        case _: Mo => List(name -> t"true")
        case _     => Nil

      val limited = (under, over) match
        case (Unset, Unset) => base

        case (u, Unset) =>
          val script = u.or(base)
          Munder(base, script, accent(script, t"accentunder"))

        case (Unset, o) =>
          val script = o.or(base)
          Mover(base, script, accent(script, t"accent"))

        case (u, o) =>
          val below = u.or(base)
          val above = o.or(base)
          Munderover(base, below, above,
            List.of(accent(below, t"accentunder").stdlib ++ accent(above, t"accent").stdlib))

      (sub, sup) match
        case (Unset, Unset) => limited
        case (b, Unset)     => Msub(limited, b.or(limited))
        case (Unset, p)     => Msup(limited, p.or(limited))
        case (b, p)         => Msubsup(limited, b.or(limited), p.or(limited))

    // A primary is a unit plus any postfix attribute directives bound to it.
    private def parsePrimary(): Mathml = withDirectives(parseUnit())

    // Reads `open … close` as a raw attribute value (its content is not parsed).
    private def readValue(): Text =
      advance() // open
      val start = pos
      var depth = 1

      while pos < s.length && depth > 0 do
        val ch = s.charAt(pos)
        if ch == open then depth += 1 else if ch == close then depth -= 1
        if depth > 0 then pos += 1

      val raw = s.substring(start, pos).nn.tt
      if peek != close then scala.caps.unsafe.unsafeAssumeSeparate(abort(Ergo.Error(Ergo.Error.Reason.Unclosed(close.toString.tt), pos)))
      advance() // close
      raw

    private def withDirectives(unit: Mathml): Mathml =
      val attributes = ListBuffer[(Text, Text)]()
      var scanning = true

      while scanning do
        directive(peek) match
          case Directive.Param(name) =>
            if peekAt(1) == open then { advance(); attributes += name -> readValue() }
            else scanning = false

          case Directive.Fixed(name, value) =>
            advance()
            attributes += name -> value

          case _ =>
            scanning = false

      applyAttributes(unit, attributes.to(List))

    private def applyAttributes(node: Mathml, extra: List[(Text, Text)]): Mathml =
      if extra.stdlib.isEmpty then node else node match
        case n: Mi         => n.copy(attributes = List.of(n.attributes.stdlib ++ extra.stdlib))
        case n: Mn         => n.copy(attributes = List.of(n.attributes.stdlib ++ extra.stdlib))
        case n: Mo         => n.copy(attributes = List.of(n.attributes.stdlib ++ extra.stdlib))
        case n: Mrow       => n.copy(attributes = List.of(n.attributes.stdlib ++ extra.stdlib))
        case n: Mfrac      => n.copy(attributes = List.of(n.attributes.stdlib ++ extra.stdlib))
        case n: Msqrt      => n.copy(attributes = List.of(n.attributes.stdlib ++ extra.stdlib))
        case n: Mroot      => n.copy(attributes = List.of(n.attributes.stdlib ++ extra.stdlib))
        case n: Msub       => n.copy(attributes = List.of(n.attributes.stdlib ++ extra.stdlib))
        case n: Msup       => n.copy(attributes = List.of(n.attributes.stdlib ++ extra.stdlib))
        case n: Msubsup    => n.copy(attributes = List.of(n.attributes.stdlib ++ extra.stdlib))
        case n: Munder     => n.copy(attributes = List.of(n.attributes.stdlib ++ extra.stdlib))
        case n: Mover      => n.copy(attributes = List.of(n.attributes.stdlib ++ extra.stdlib))
        case n: Munderover => n.copy(attributes = List.of(n.attributes.stdlib ++ extra.stdlib))
        case n: Mtable     => n.copy(attributes = List.of(n.attributes.stdlib ++ extra.stdlib))
        case other         => Mrow(List(other), extra)

    private def parseUnit(): Mathml =
      skipSpaces()
      val c = peek

      if c == Hole then { advance(); holes.next() }
      else if c == open then rooted(parseGroup())
      else if c == Radical then { advance(); Msqrt(parsePrimary()) }
      else if c == Matrix then parseMatrix()
      else if c == RowVec then parseVector(row = true)
      else if c == ColVec then parseVector(row = false)
      else if letter(c) then
        val start = pos
        while pos < s.length && letter(s.charAt(pos)) do pos += 1
        rooted(Mi(s.substring(start, pos).nn.tt))
      else if digit(c) then
        val start = pos

        while pos < s.length && (digit(s.charAt(pos)) ||
          s.charAt(pos) == '.' && pos + 1 < s.length && digit(s.charAt(pos + 1)))
        do pos += 1

        rooted(Mn(s.substring(start, pos).nn.tt))
      else if c == '\u0000' then
        scala.caps.unsafe.unsafeAssumeSeparate(abort(Ergo.Error(Ergo.Error.Reason.UnexpectedEnd, pos)))
      else
        // a content glyph, or an operator glyph degraded for want of an operand
        advance()
        Mo(c.toString.tt)

    // If a `√` immediately follows an atom/group (no space), that atom is the
    // index of a root: `3√x` = Mroot(x, 3).
    private def rooted(index: Mathml): Mathml =
      if peek == Radical then { advance(); Mroot(parsePrimary(), index) } else index

    // Consumes the self-delimiting body `open <group>… close` that follows an
    // introducer, returning the parsed child groups. A body with no nested
    // groups is treated as a single element (so `⋯(a)` is a one-cell vector).
    private def parseBody(introducer: Char): List[Mathml] =
      if peek != open then scala.caps.unsafe.unsafeAssumeSeparate(abort(Ergo.Error(Ergo.Error.Reason.MissingBody(introducer.toString.tt), pos)))
      advance() // body open
      val items = ListBuffer[Mathml]()
      while peek == open do items += parseGroup()
      if items.isEmpty && peek != close then items += parseSequence()
      skipSpaces()
      if peek != close then scala.caps.unsafe.unsafeAssumeSeparate(abort(Ergo.Error(Ergo.Error.Reason.Unclosed(close.toString.tt), pos)))
      advance() // body close
      items.to(List)

    private def parseVector(row: Boolean): Mathml =
      val introducer = advance()
      val tds = parseBody(introducer).stdlib.map(Mtd(_))
      if row then Mtable(Mtr(tds*)) else Mtable(tds.map(Mtr(_))*)

    private def parseMatrix(): Mathml =
      val introducer = advance()
      if peek != open then scala.caps.unsafe.unsafeAssumeSeparate(abort(Ergo.Error(Ergo.Error.Reason.MissingBody(introducer.toString.tt), pos)))
      advance() // body open
      val rows = ListBuffer[Mathml]()

      while peek == open do
        advance() // row-group open
        val cells = ListBuffer[Mathml]()
        while peek == open do cells += parseGroup()

        // A row group with no nested cell-groups is a single-cell row.
        if cells.isEmpty && peek != close then cells += parseSequence()
        skipSpaces()
        if peek != close then scala.caps.unsafe.unsafeAssumeSeparate(abort(Ergo.Error(Ergo.Error.Reason.Unclosed(close.toString.tt), pos)))
        advance() // row-group close
        rows += Mtr(cells.toList.map { cell => Mtd(cell) }*)

      skipSpaces()
      if peek != close then scala.caps.unsafe.unsafeAssumeSeparate(abort(Ergo.Error(Ergo.Error.Reason.Unclosed(close.toString.tt), pos)))
      advance() // body close
      Mtable(rows.to(List)*)

  // Ergo.Error → Ergo.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case Empty                    extends Reason(1)
      case BadOpener(char: Text)    extends Reason(2)
      case Unclosed(expected: Text) extends Reason(3)
      case UnexpectedEnd            extends Reason(4)
      case MissingBody(char: Text)  extends Reason(5)
      case Unsupported(label: Text) extends Reason(6)

    given communicable: Reason is Communicable =
      case Reason.Empty              => m"the expression was empty"
      case Reason.BadOpener(char)    => m"the expression must start with a bracket, not $char"
      case Reason.Unclosed(expected) => m"a group was not closed with $expected"
      case Reason.UnexpectedEnd      => m"the expression ended unexpectedly"
      case Reason.MissingBody(char)  => m"the $char introducer must be followed by a group"
      case Reason.Unsupported(label) => m"the <$label> element has no ergo representation"

  // `offset` is the character position within the parsed input at which the error was detected,
  // or -1 when unavailable (the serialization path, which has no cursor).
  case class Error(reason: Ergo.Error.Reason, offset: Int = -1)(using Diagnostics)
  extends fulminate.Error(431, reason.number)
    ( m"the ergo expression could not be parsed because $reason" )
