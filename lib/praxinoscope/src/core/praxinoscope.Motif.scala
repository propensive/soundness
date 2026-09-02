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

import scala.collection.concurrent.TrieMap

import anticipation.*
import contingency.*
import denominative.*
import denominative.dysasymptotics.linearSize
import fulminate.*
import rudiments.*
import vacuous.*

import Motif.Error.Reason.*
import Motif.Node

object Motif:
  // The structural form of a parsed RE2 regular expression. Character classes are already
  // resolved to `Ranges` (negation included), so no node distinguishes `[^…]` from its
  // complement, and a capturing group carries its 1-based index while a non-capturing group
  // carries `Unset`.
  object Node:
    enum Anchor:
      case Start, End, WordBoundary, NonWordBoundary, LineStart, LineEnd

  enum Node:
    case Empty
    case Literal(codepoint: Int)
    case Klass(ranges: Ranges)
    case Sequence(nodes: List[Node])
    case Alternation(options: List[Node])
    case Repeat(child: Node, minimum: Int, maximum: Optional[Int], reluctant: Boolean)
    case Group(child: Node, index: Optional[Int])
    case Boundary(anchor: Node.Anchor)

  // The largest repetition bound RE2 accepts in `{n,m}`.
  val maxRepetition: Int = 1000

  private val stagedMotifs: TrieMap[String, Motif] = TrieMap()

  // Reconstructs a compile-time-staged Motif. A `case r"…" =>` expansion runs its extractor
  // construction on EVERY match, so the program (by-name) is materialized only on the first
  // call for a given pattern and served from a cache thereafter; the pattern is a string
  // literal, so the cache key is interned and the lookup short-circuits on identity. The
  // structural node plays no part in matching once compiled, so it is not staged and a staged
  // Motif's `node` is `Empty`.
  def staged(pattern: Text, captures: Int, program: => Program): Motif =
    stagedMotifs.getOrElseUpdate(pattern.s, Motif(pattern, Node.Empty, captures, program))

  def apply(text: Text): Motif raises Motif.Error = parse(text)

  // A recursive-descent parser for RE2 syntax: literals, `.`, escapes (including `\xHH` and
  // `\x{…}`), the ASCII perl classes, POSIX classes (`[[:alpha:]]`), Unicode general categories
  // and scripts (`\p{Lu}`, `\pL`, `\p{Greek}`, and their `\P` complements), quoted runs
  // (`\Q…\E`), character classes, alternation, groups (capturing, non-capturing and named),
  // the inline flags `i`, `m`, `s` and `U` in both their `(?flags)` and `(?flags:…)` forms, the
  // `* + ? {n} {n,} {n,m}` quantifiers with an optional reluctant `?` suffix, and the anchors
  // `^ $ \A \z \b \B`. Everything RE2 rejects by design is rejected here with a positioned
  // error: backreferences, lookaround, possessive quantifiers and atomic groups.
  def parse(text: Text): Motif raises Motif.Error =
    val input: String = text.s
    val length: Int = input.length
    var index: Int = 0
    var captures: Int = 0

    // RE2's inline flags. Each applies from where it is set to the end of the enclosing group,
    // which `group` implements by saving and restoring them around every group body.
    var foldCase: Boolean = false
    var dotAll: Boolean = false
    var multiline: Boolean = false
    var ungreedy: Boolean = false

    def current: Char = if index >= length then '\u0000' else input.charAt(index)

    def lookahead(offset: Int = 1): Char =
      if index + offset >= length then '\u0000' else input.charAt(index + offset)

    extension [value](value: value) def adv(count: Int = 1): value = value.also { index += count }

    def codepoint(): Int =
      val result = input.codePointAt(index)
      result.adv(Character.charCount(result))

    def alternation(): Node =
      val first = sequence()

      if current != '|' then first else
        var options: List[Node] = first :: Nil

        while current == '|' do
          index += 1
          options = sequence() :: options

        Node.Alternation(options.reverse)

    def sequence(): Node =
      var nodes: List[Node] = Nil

      while index < length && current != '|' && current != ')'
      do nodes = quantified(atom()) :: nodes

      nodes match
        case Nil         => Node.Empty
        case node :: Nil => node
        case _           => Node.Sequence(nodes.reverse)

    // Under `U` the default greediness is swapped, so a bare quantifier is reluctant and a
    // `?`-suffixed one is greedy.
    def reluctant(): Boolean = current match
      case '?' => (!ungreedy).adv()
      case '+' => abort(Motif.Error(index, PossessiveQuantifier))
      case _   => ungreedy

    def quantified(node: Node): Node = current match
      case '*' => index += 1; Node.Repeat(node, 0, Unset, reluctant())
      case '+' => index += 1; Node.Repeat(node, 1, Unset, reluctant())
      case '?' => index += 1; Node.Repeat(node, 0, 1, reluctant())

      case '{' if lookahead().isDigit =>
        index += 1
        val minimum = number()

        current match
          case '}' =>
            index += 1
            Node.Repeat(node, minimum, minimum, reluctant())

          case ',' =>
            index += 1

            if current == '}' then
              index += 1
              Node.Repeat(node, minimum, Unset, reluctant())
            else if !current.isDigit then
              abort(Motif.Error(index, UnexpectedChar))
            else
              val maximum = number()
              if current != '}' then abort(Motif.Error(index, IncompleteRepetition))
              index += 1
              if maximum < minimum then abort(Motif.Error(index - 1, BadRepetition))
              Node.Repeat(node, minimum, maximum, reluctant())

          case '\u0000' => abort(Motif.Error(index, IncompleteRepetition))
          case _        => abort(Motif.Error(index, UnexpectedChar))

      case _ =>
        node

    def number(): Int =
      var count: Int = 0

      while current.isDigit do
        count = count*10 + (current - '0')
        if count > maxRepetition then abort(Motif.Error(index, RepetitionTooLarge))
        index += 1

      count

    // Every class-valued construct goes through here, so `i` folds them all uniformly —
    // `[a-z]`, `\\p{Lu}` and `\\w` alike.
    def klass(ranges: Ranges): Node = Node.Klass(if foldCase then ranges.folded else ranges)

    def atom(): Node = current match
      case '('             => group()
      case '['             => charClass()
      case '.'             => klass(if dotAll then Ranges.anySymbol else Ranges.any).adv()

      case '^' =>
        Node.Boundary(if multiline then Node.Anchor.LineStart else Node.Anchor.Start).adv()

      case '$' =>
        Node.Boundary(if multiline then Node.Anchor.LineEnd else Node.Anchor.End).adv()
      case '\\'            => escape()
      case '*' | '+' | '?' => abort(Motif.Error(index, UnexpectedChar))

      case _ =>
        val symbol = codepoint()

        // Under `i` a literal denotes its whole fold orbit, so it becomes a class.
        if !foldCase then Node.Literal(symbol) else
          val folded = Ranges.point(symbol).folded
          if folded == Ranges.point(symbol) then Node.Literal(symbol) else Node.Klass(folded)

    def closeGroup(): Unit =
      if current == ')' then index += 1 else abort(Motif.Error(index, UnclosedGroup))

    // Applies a run of flag letters, honouring a `-` that switches to clearing them. Returns
    // false if a character is not a flag letter, so the caller can report it in position.
    def applyFlags(): Boolean =
      var setting = true
      var valid = true

      while valid && current != ')' && current != ':' && index < length do
        current match
          case '-' => setting = false.also(index += 1)
          case 'i' => foldCase = setting.also(index += 1)
          case 's' => dotAll = setting.also(index += 1)
          case 'm' => multiline = setting.also(index += 1)
          case 'U' => ungreedy = setting.also(index += 1)
          case _   => valid = false

      valid

    def group(): Node =
      index += 1

      // Flags are scoped to the enclosing group, so every group body restores what it found.
      val savedFold = foldCase
      val savedDot = dotAll
      val savedMulti = multiline
      val savedUngreedy = ungreedy

      def restore(): Unit =
        foldCase = savedFold
        dotAll = savedDot
        multiline = savedMulti
        ungreedy = savedUngreedy

      if current == '?' then lookahead() match
        case ':' =>
          index += 2
          val child = alternation()
          closeGroup()
          restore()
          Node.Group(child, Unset)

        case '=' | '!' =>
          abort(Motif.Error(index - 1, Lookaround))

        case '<' => lookahead(2) match
          case '=' | '!' => abort(Motif.Error(index - 1, Lookaround))

          // `(?<name>…)` is a named capture: the name plays no part in the language, so the
          // group is captured positionally as if unnamed, exactly as RE2 numbers it.
          case _ =>
            val close = input.indexOf('>', index + 2)
            if close < 0 then abort(Motif.Error(index - 1, UnclosedGroup))
            index = close + 1
            captures += 1
            val capture = captures
            val child = alternation()
            closeGroup()
            restore()
            Node.Group(child, capture)

        // `(?P<name>…)` is the same thing under RE2's older spelling.
        case 'P' =>
          if lookahead(2) != '<' then abort(Motif.Error(index - 1, UnsupportedGroup))
          val close = input.indexOf('>', index + 3)
          if close < 0 then abort(Motif.Error(index - 1, UnclosedGroup))
          index = close + 1
          captures += 1
          val capture = captures
          val child = alternation()
          closeGroup()
          restore()
          Node.Group(child, capture)

        case '>' => abort(Motif.Error(index - 1, AtomicGroup))

        case _ =>
          val start = index
          index += 1
          if !applyFlags() then abort(Motif.Error(index, Flag))

          if current == ':' then
            // `(?flags:…)` scopes the flags to this group alone.
            index += 1
            val child = alternation()
            closeGroup()
            restore()
            Node.Group(child, Unset)
          else if current == ')' then
            // `(?flags)` sets them for the rest of the enclosing group, so the saved values
            // are deliberately *not* restored here.
            index += 1
            Node.Empty
          else abort(Motif.Error(start, Flag))

      else
        captures += 1
        val capture = captures
        val child = alternation()
        closeGroup()
        restore()
        Node.Group(child, capture)

    def perlClass(char: Char): Boolean = "dDwWsS".indexOf(char) >= 0

    def perlRanges(): Ranges = current match
      case 'd' => Ranges.digit.adv()
      case 'D' => Ranges.digit.negate().adv()
      case 'w' => Ranges.word.adv()
      case 'W' => Ranges.word.negate().adv()
      case 's' => Ranges.space.adv()
      case 'S' => Ranges.space.negate().adv()
      case _   => abort(Motif.Error(index, InvalidEscape))

    // `\\pN`, `\\p{Name}` and their negated `\\P` forms. The cursor is on the `p` or `P`.
    def unicodeRanges(): Ranges =
      val negated = current == 'P'
      index += 1

      val name =
        if current != '{' then
          val single = String.valueOf(current).nn
          index += 1
          single
        else
          val close = input.indexOf('}', index)
          if close < 0 then abort(Motif.Error(index, UnclosedClass))
          val braced = input.substring(index + 1, close).nn
          index = close + 1
          braced

      // A `\\p{^Name}` is RE2's other spelling of negation.
      val inverted = name.startsWith("^")
      val plain = if inverted then name.substring(1).nn else name

      Ranges.unicode(plain).lay(abort(Motif.Error(index, UnknownUnicodeClass))): ranges =>
        if negated != inverted then ranges.negate() else ranges

    def charClass(): Node =
      index += 1
      val negated = current == '^'
      if negated then index += 1
      if current == ']' then abort(Motif.Error(index, EmptyCharClass))
      var ranges: Ranges = Ranges.empty

      while current != ']' do
        if index >= length then abort(Motif.Error(index, UnclosedClass))
        ranges = ranges.union(classItem())

      index += 1
      klass(if negated then ranges.negate() else ranges)

    // `[:name:]` and its negated `[:^name:]`, valid only inside a character class. RE2 treats a
    // `[:` that does not close with `:]` as ordinary characters, so this only commits once the
    // whole form is present.
    def posixClass(): Optional[Ranges] =
      val close = input.indexOf(":]", index + 2)

      if current != '[' || lookahead() != ':' || close < 0 then Unset else
        val negated = lookahead(2) == '^'
        val from = index + (if negated then 3 else 2)
        val name = input.substring(from, close).nn

        Ranges.posix.at(name) match
          case ranges: Ranges =>
            index = close + 2
            if negated then ranges.negate() else ranges

          case _ =>
            abort(Motif.Error(index, InvalidPosixClass))

    def classItem(): Ranges =
      posixClass().lay(nonPosixClassItem())(identity(_))

    def nonPosixClassItem(): Ranges =
      if current == '\\' && perlClass(lookahead()) then
        index += 1
        perlRanges()
      else if current == '\\' && (lookahead() == 'p' || lookahead() == 'P') then
        index += 1
        unicodeRanges()
      else
        val start = classChar()

        if current == '-' && lookahead() != ']' && index + 1 < length then
          index += 1

          if current == '\\' && perlClass(lookahead())
          then abort(Motif.Error(index, UnexpectedChar))

          val end = classChar()
          if end < start then abort(Motif.Error(index - 1, InvertedRange))
          Ranges(start, end)
        else
          Ranges.point(start)

    def classChar(): Int = current match
      case '\u0000' => abort(Motif.Error(index, UnclosedClass))
      case '\\'     => index += 1; escapedCodepoint()
      case _        => codepoint()

    def escape(): Node =
      index += 1

      current match
        case '\u0000'  => abort(Motif.Error(index, UnclosedEscape))
        case 'd'       => klass(Ranges.digit).adv()
        case 'D'       => klass(Ranges.digit.negate()).adv()
        case 'w'       => klass(Ranges.word).adv()
        case 'W'       => klass(Ranges.word.negate()).adv()
        case 's'       => klass(Ranges.space).adv()
        case 'S'       => klass(Ranges.space.negate()).adv()
        case 'b'       => Node.Boundary(Node.Anchor.WordBoundary).adv()
        case 'B'       => Node.Boundary(Node.Anchor.NonWordBoundary).adv()
        case 'A'       => Node.Boundary(Node.Anchor.Start).adv()
        case 'z'       => Node.Boundary(Node.Anchor.End).adv()
        case 'k'       => abort(Motif.Error(index, Backreference))
        case 'p' | 'P' => klass(unicodeRanges())

        // `\\Q…\\E` quotes a run of literal text, in which no character is special. An
        // unterminated `\\Q` runs to the end of the pattern, as RE2 allows.
        case 'Q' =>
          index += 1
          val close = input.indexOf("\\E", index)
          val end = if close < 0 then length else close
          val literal = input.substring(index, end).nn
          index = if close < 0 then length else close + 2

          if literal.isEmpty then Node.Empty else
            var nodes: List[Node] = Nil
            var at = 0

            while at < literal.length do
              val symbol = literal.codePointAt(at)
              nodes = Node.Literal(symbol) :: nodes
              at += Character.charCount(symbol)

            nodes match
              case node :: Nil => node
              case _           => Node.Sequence(nodes.reverse)

        case char if char >= '1' && char <= '9' =>
          abort(Motif.Error(index, Backreference))

        case _ =>
          Node.Literal(escapedCodepoint())

    def hexDigit(): Int =
      val char = current

      val value =
        if char >= '0' && char <= '9' then char - '0'
        else if char >= 'a' && char <= 'f' then char - 'a' + 10
        else if char >= 'A' && char <= 'F' then char - 'A' + 10
        else abort(Motif.Error(index, InvalidEscape))

      value.adv()

    // Interprets the character(s) following a `\`, which has already been consumed. Perl classes,
    // anchors and backreferences are handled by the callers, so only single-codepoint escapes
    // remain: the named control characters, `\xHH`, `\x{…}` and escaped punctuation. Escaping an
    // alphanumeric character which has no assigned meaning is an error, as it is in RE2.
    def escapedCodepoint(): Int = current match
      case '\u0000' => abort(Motif.Error(index, UnclosedEscape))
      case 'n'      => '\n'.toInt.adv()
      case 't'      => '\t'.toInt.adv()
      case 'r'      => '\r'.toInt.adv()
      case 'f'      => '\f'.toInt.adv()
      case 'a'      => 7.adv()
      case 'v'      => 11.adv()
      case 'e'      => 27.adv()

      case 'x' =>
        index += 1

        if current != '{' then hexDigit()*16 + hexDigit() else
          index += 1
          var value: Int = 0
          var count: Int = 0

          while current != '}' do
            value = value*16 + hexDigit()
            count += 1
            if value > Ranges.maxSymbol || count > 6 then abort(Motif.Error(index, InvalidEscape))

          if count == 0 then abort(Motif.Error(index, InvalidEscape))
          index += 1
          value

      case char if char.isLetter || char.isDigit =>
        abort(Motif.Error(index, InvalidEscape))

      case _ =>
        codepoint()

    val node = alternation()
    if index < length then abort(Motif.Error(index, NotInGroup))

    Motif(text, node, captures, Program.compile(node, captures))

  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case UnclosedGroup => m"a group was not closed"

        case NotInGroup =>
          m"a closing parenthesis was found without a corresponding opening parenthesis"

        case UnexpectedChar       => m"an unexpected character was found"
        case BadRepetition        => m"the maximum number of repetitions is less than the minimum"
        case IncompleteRepetition => m"the repetition range was not closed"
        case EmptyCharClass       => m"the character class is empty"
        case UnclosedClass        => m"the character class was not closed"
        case UnclosedEscape       => m"nothing followed the escape character `\\`"
        case InvalidEscape        => m"the escape sequence is not valid"

        case InvertedRange =>
          m"the end of the character range is less than its start"

        case Backreference        => m"backreferences are not part of RE2 syntax"
        case Lookaround           => m"lookahead and lookbehind are not part of RE2 syntax"
        case PossessiveQuantifier => m"possessive quantifiers are not part of RE2 syntax"
        case AtomicGroup          => m"atomic groups are not part of RE2 syntax"
        case UnsupportedGroup     => m"the group syntax is not supported"
        case Flag                 => m"inline flags are not supported"

        case RepetitionTooLarge =>
          m"the number of repetitions exceeds the limit of ${Motif.maxRepetition}"

        case BudgetExceeded => m"the analysis exceeded its computation budget"

        case Unverifiable =>
          m"the pattern contains constructs which the analysis cannot verify"

        case InvalidPosixClass   => m"the POSIX character class name is not recognised"
        case UnknownUnicodeClass => m"the Unicode class name is not recognised"

    enum Reason(val number: Int) extends Clarification:
      case UnclosedGroup        extends Reason(1)
      case NotInGroup           extends Reason(2)
      case UnexpectedChar       extends Reason(3)
      case BadRepetition        extends Reason(4)
      case IncompleteRepetition extends Reason(5)
      case EmptyCharClass       extends Reason(6)
      case UnclosedClass        extends Reason(7)
      case UnclosedEscape       extends Reason(8)
      case InvalidEscape        extends Reason(9)
      case InvertedRange        extends Reason(10)
      case Backreference        extends Reason(11)
      case Lookaround           extends Reason(12)
      case PossessiveQuantifier extends Reason(13)
      case AtomicGroup          extends Reason(14)
      case UnsupportedGroup     extends Reason(15)
      case Flag                 extends Reason(16)
      case RepetitionTooLarge   extends Reason(17)
      case BudgetExceeded       extends Reason(18)
      case Unverifiable         extends Reason(19)
      case InvalidPosixClass    extends Reason(20)
      case UnknownUnicodeClass  extends Reason(21)

  case class Error(index: Int, reason: Motif.Error.Reason)(using Diagnostics)
  extends fulminate.Error(500, reason.number)
    ( m"the regular expression could not be compiled because $reason at $index" )

// A parsed RE2 regular expression: the source `pattern`, its structural `node` form, the number
// of capturing groups it contains, and its compiled `program`.
case class Motif(pattern: Text, node: Node, captures: Int, program: Program):
  def matches[input: Symbolizer](input: input): Boolean =
    Pike.run(program, input, 0, true, true).present

  def seek[input: Symbolizer](input: input, start: Ordinal = Prim): Optional[Interval] =
    Pike.run(program, input, start.n0, false, false).let: slots =>
      Interval.zerary(slots(0), slots(1))

  def search[input: Symbolizer as symbolizer](input: input, start: Ordinal = Prim)
  :   Chain[Interval] =

    def recur(from: Int): Chain[Interval] =
      if from > symbolizer.length(input) then Chain() else
        Pike.run(program, input, from, false, false).let: slots =>
          val interval = Interval.zerary(slots(0), slots(1))
          interval #:: recur(if slots(1) > slots(0) then slots(1) else slots(1) + 1)

        . or(Chain())

    recur(start.n0)

  // True if this expression matches a superset of the inputs `that` matches, i.e.
  // L(`that`) ⊆ L(`this`). Word boundaries are unsupported (`Unverifiable`), and pathological
  // pairs can exhaust the analysis budget (`BudgetExceeded`).
  def subsumes(that: Motif): Boolean raises Motif.Error =
    Subsumption.subsumes(program, that.program)

  // True if this expression matches every input matched by *all* of `those` at once, i.e.
  // L(⋂`those`) ⊆ L(`this`). Since RE2 has no intersection operator, this is the only way to
  // ask the containment question about a set of expressions applied in conjunction.
  def subsumes(those: List[Motif]): Boolean raises Motif.Error =
    Subsumption.subsumes(program, those.map(_.program))

  def intersects(that: Motif): Boolean raises Motif.Error =
    Subsumption.intersects(program, that.program)

  // The spans of each capturing group for a whole-input match, in group order, or `Unset` if
  // the input does not match. A group which participated in no iteration of the match is
  // `Unset` within the list.
  def groups[input: Symbolizer](input: input): Optional[List[Optional[Interval]]] =
    spans(input).let: all => all.skip(1)

  // The winning thread's raw capture-slot array: the bounds of group `n` at indices `2n` and
  // `2n + 1` (the whole match at 0 and 1), with `-1` for a slot no thread reached. This exists
  // alongside `spans` because an `Interval` cannot represent the *position* of an empty match,
  // which callers stepping through repeated matches need.
  def slots[input: Symbolizer](input: input, start: Ordinal = Prim, anchored: Boolean = true)
  :   Optional[scala.IArray[Int]] =

    Pike.run(program, input, start.n0, anchored, anchored)

  // Like `groups`, but index 0 is the whole match, and when `anchored` is unset the leftmost
  // match from `start` is reported rather than requiring the whole input to match.
  def spans[input: Symbolizer](input: input, start: Ordinal = Prim, anchored: Boolean = true)
  :   Optional[List[Optional[Interval]]] =

    Pike.run(program, input, start.n0, anchored, anchored).let: slots =>
      List.tabulate(captures + 1): group =>
        val first = slots(2*group)
        val last = slots(2*group + 1)
        if first < 0 || last < 0 then Unset else Interval.zerary(first, last)
