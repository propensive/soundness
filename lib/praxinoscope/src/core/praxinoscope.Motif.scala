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
import scala.collection.mutable.ArrayBuffer

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
          else
            abort(Motif.Error(start, Flag))

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

  object Program:
    import anticipation.*
    import contingency.*
    import rudiments.*
    import Motif.Error.Reason.*
    import Motif.Node

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

      def bounds(ranges: Ranges): Array[Int] = ranges.spans.to[Array]

      // Emits the code for one node. Fragments are contiguous and fall through to whatever is
      // emitted next, so a consuming op's `next` is always its own pc + 1; only loops and
      // alternations jump.
      def walk(node: Node): Unit = node match
        case Node.Empty =>
          ()

        case Node.Literal(codepoint) =>
          emit(Program.Op.Symbol(Array(codepoint, codepoint), ops.size + 1))

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

      Program(Array.from(ops), 2*(captures + 1), captures)

    // One instruction of a compiled program. Only `Symbol` consumes input; every other op is
    // followed at no cost during the Pike VM's ε-closure. `bounds` is the interleaved
    // `lo, hi, lo, hi, …` form of a `Ranges`, flattened for allocation-free scanning; `Split`
    // lists its `preferred` target first, which is how greed and reluctance are encoded.
    enum Op:
      case Symbol(bounds: Array[Int], next: Int)
      case Split(preferred: Int, alternate: Int)
      case Jump(target: Int)
      case Save(slot: Int, next: Int)
      case Test(anchor: Node.Anchor, next: Int)
      case Accept

  // A regular expression compiled to a flat, array-indexed instruction sequence, executable by the
  // Pike VM and analyzable by the containment checker. Capture group `n` records its bounds in
  // slots `2n` and `2n + 1`; slots `0` and `1` are the whole match.
  case class Program(ops: Array[Program.Op], slots: Int, captures: Int):
    import anticipation.*
    import contingency.*
    import rudiments.*
    import Motif.Error.Reason.*
    import Motif.Node

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

            ops.readUnchecked(pc) match
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
    lazy val firstBounds: Array[Int] =
      var spans: List[Int] = Nil

      opening(0).each: pc =>
        ops.readUnchecked(pc).absolve match
          case Program.Op.Symbol(bounds, _) =>
            var index = 0

            while index < bounds.length do
              val range = Ranges(bounds.readUnchecked(index), bounds.readUnchecked(index + 1))

              spans = range.spans.absolve match
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

      merged.spans.to[Array]

    // The longest literal every match must begin with (UTF-16-encoded, capped at 32 chars), or
    // empty if the first consuming op is not unique and single-valued. Seeking jumps between
    // occurrences of this literal with `Symbolizer.find` rather than stepping positions.
    lazy val prefix: Text =
      val builder = StringBuilder()

      def recur(pcs: List[Int], accepts: Boolean): Unit =
        if !accepts && builder.length < 32 then pcs match
          case pc :: Nil => ops.readUnchecked(pc).absolve match
            case Program.Op.Symbol(bounds, next) =>
              if bounds.length == 2 && bounds.readUnchecked(0) == bounds.readUnchecked(1) then
                builder.append(new String(Character.toChars(bounds.readUnchecked(0))))
                val (symbols, accepts2) = reach(next :: Nil)
                recur(symbols, accepts2)

          case _ =>
            ()

      recur(opening(0), nullable)
      builder.toString.tt

  object Ranges:
    import rudiments.*
    import vacuous.*

    // The greatest symbol in the default (Unicode codepoint) domain. Other symbol domains (UTF-8
    // bytes, say) are strictly smaller, so this is a safe universal bound for `negate`.
    val maxSymbol: Int = 0x10ffff

    val empty: Ranges = new Ranges(Nil)
    def apply(lo: Int, hi: Int): Ranges = new Ranges(lo :: hi :: Nil)
    def point(symbol: Int): Ranges = new Ranges(symbol :: symbol :: Nil)

    // The RE2 perl classes are ASCII-only.
    val digit: Ranges = Ranges('0'.toInt, '9'.toInt)

    val word: Ranges =
      new Ranges
        ( '0'.toInt :: '9'.toInt :: 'A'.toInt :: 'Z'.toInt :: '_'.toInt :: '_'.toInt ::
          'a'.toInt :: 'z'.toInt :: Nil )

    val space: Ranges = new Ranges('\t'.toInt :: '\r'.toInt :: ' '.toInt :: ' '.toInt :: Nil)
    val any: Ranges = new Ranges(0 :: '\n'.toInt - 1 :: '\n'.toInt + 1 :: maxSymbol :: Nil)
    val anySymbol: Ranges = new Ranges(0 :: maxSymbol :: Nil)

    // Every symbol the predicate admits, as coalesced runs. Scanning the whole domain is what
    // makes the Unicode classes exact — the JDK's own tables answer the predicate, so there is no
    // second copy of the Unicode database here to drift out of date — and it costs one pass per
    // distinct class, which `unicode` memoises.
    def where(predicate: Int => Boolean): Ranges =
      val spans = scala.collection.mutable.ListBuffer.empty[Int]
      var symbol = 0
      var start = -1

      while symbol <= maxSymbol do
        if predicate(symbol) then (if start < 0 then start = symbol)
        else if start >= 0 then
          spans += start
          spans += symbol - 1
          start = -1

        symbol += 1

      if start >= 0 then
        spans += start
        spans += maxSymbol

      new Ranges(spans.to(List))

    // The POSIX classes of RE2's `[[:name:]]` form, all ASCII-only.
    val posix: Map[String, Ranges] =
      Map
        ( "alnum"  -> Ranges('0', '9').union(Ranges('A', 'Z')).union(Ranges('a', 'z')),
          "alpha"  -> Ranges('A', 'Z').union(Ranges('a', 'z')),
          "ascii"  -> Ranges(0x00, 0x7f),
          "blank"  -> Ranges.point('\t').union(Ranges.point(' ')),
          "cntrl"  -> Ranges(0x00, 0x1f).union(Ranges.point(0x7f)),
          "digit"  -> digit,
          "graph"  -> Ranges('!', '~'),
          "lower"  -> Ranges('a', 'z'),
          "print"  -> Ranges(' ', '~'),
          "punct"  -> Ranges('!', '/').union(Ranges(':', '@')).union(Ranges('[', '`'))
                      . union(Ranges('{', '~')),
          "space"  -> Ranges('\t', '\r').union(Ranges.point(' ')),
          "upper"  -> Ranges('A', 'Z'),
          "word"   -> word,
          "xdigit" -> Ranges('0', '9').union(Ranges('A', 'F')).union(Ranges('a', 'f')) )

    private val unicodeClasses: scala.collection.concurrent.TrieMap[String, Optional[Ranges]] =
      scala.collection.concurrent.TrieMap()

    // A Unicode general category (`L`, `Lu`, `Nd`, …) or script (`Greek`, `Han`, …) by the name
    // RE2's `\p{…}` uses, or `Unset` if no such class exists. `Any` is RE2's name for the whole
    // domain. General categories are matched by prefix, so the one-letter `L` is the union of
    // `Lu`, `Ll`, `Lt`, `Lm` and `Lo`, exactly as RE2 defines it.
    def unicode(name: String): Optional[Ranges] =
      unicodeClasses.getOrElseUpdate(name, compute(name))

    private def compute(name: String): Optional[Ranges] =
      if name == "Any" then anySymbol
      else if categories.has(name) then where: symbol => categoryName(symbol) == name
      else if categoryGroups.has(name)
      then where: symbol => categoryName(symbol).startsWith(name)
      else
        try
          val script = Character.UnicodeScript.forName(name).nn
          where: symbol => Character.UnicodeScript.of(symbol) == script
        catch case _: IllegalArgumentException => Unset

    // The two-letter general-category abbreviation for a codepoint, as Unicode names them.
    private def categoryName(symbol: Int): String = Character.getType(symbol) match
      case Character.UPPERCASE_LETTER          => "Lu"
      case Character.LOWERCASE_LETTER          => "Ll"
      case Character.TITLECASE_LETTER          => "Lt"
      case Character.MODIFIER_LETTER           => "Lm"
      case Character.OTHER_LETTER              => "Lo"
      case Character.NON_SPACING_MARK          => "Mn"
      case Character.COMBINING_SPACING_MARK    => "Mc"
      case Character.ENCLOSING_MARK            => "Me"
      case Character.DECIMAL_DIGIT_NUMBER      => "Nd"
      case Character.LETTER_NUMBER             => "Nl"
      case Character.OTHER_NUMBER              => "No"
      case Character.CONNECTOR_PUNCTUATION     => "Pc"
      case Character.DASH_PUNCTUATION          => "Pd"
      case Character.START_PUNCTUATION         => "Ps"
      case Character.END_PUNCTUATION           => "Pe"
      case Character.INITIAL_QUOTE_PUNCTUATION => "Pi"
      case Character.FINAL_QUOTE_PUNCTUATION   => "Pf"
      case Character.OTHER_PUNCTUATION         => "Po"
      case Character.MATH_SYMBOL               => "Sm"
      case Character.CURRENCY_SYMBOL           => "Sc"
      case Character.MODIFIER_SYMBOL           => "Sk"
      case Character.OTHER_SYMBOL              => "So"
      case Character.SPACE_SEPARATOR           => "Zs"
      case Character.LINE_SEPARATOR            => "Zl"
      case Character.PARAGRAPH_SEPARATOR       => "Zp"
      case Character.CONTROL                   => "Cc"
      case Character.FORMAT                    => "Cf"
      case Character.SURROGATE                 => "Cs"
      case Character.PRIVATE_USE               => "Co"
      case _                                   => "Cn"

    private val categories: Set[String] =
      Set
        ( "Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Mc", "Me", "Nd", "Nl", "No", "Pc", "Pd", "Ps", "Pe",
          "Pi", "Pf", "Po", "Sm", "Sc", "Sk", "So", "Zs", "Zl", "Zp", "Cc", "Cf", "Cs", "Co", "Cn" )

    private val categoryGroups: Set[String] = Set("L", "M", "N", "P", "S", "Z", "C")

    // Codepoints equivalent under simple case folding, grouped into orbits keyed by their folded
    // form. An orbit is not always a pair: `K`, `k` and the Kelvin sign all fold together, which
    // is why folding cannot be done by mapping each symbol up and down in isolation.
    private lazy val foldOrbits: scala.collection.immutable.Map[Int, scala.Array[Int]] =
      val groups = scala.collection.mutable.HashMap.empty[Int, scala.List[Int]]
      var symbol = 0

      while symbol <= maxSymbol do
        val folded = Character.toLowerCase(Character.toUpperCase(symbol))
        if folded != symbol then groups(folded) = symbol :: groups.getOrElse(folded, scala.Nil)
        symbol += 1

      groups.map{ (key, members) => (key, (key :: members).toArray) }.toMap

  // A set of symbols, represented as a sorted, disjoint, non-adjacent list of inclusive
  // `lo :: hi :: …` bounds. Symbols are `Int`-encoded members of an ordered alphabet: Unicode
  // codepoints today, though nothing here assumes more than an ordering and an upper bound. The
  // constructor is private because it admits unnormalized bounds; every public constructor and
  // operation preserves the invariant.
  case class Ranges private(spans: List[Int]):
    import rudiments.*
    import vacuous.*

    def vacant: Boolean = spans == Ranges.empty.spans

    def contains(symbol: Int): Boolean =
      def recur(todo: List[Int]): Boolean = todo match
        case lo :: hi :: tail => if symbol < lo then false else symbol <= hi || recur(tail)
        case _                => false

      recur(spans)

    def union(that: Ranges): Ranges =
      def interleave(left: List[Int], right: List[Int]): List[Int] = (left, right) match
        case (Nil, _) => right
        case (_, Nil) => left

        case (lo1 :: hi1 :: tail1, lo2 :: hi2 :: tail2) =>
          if lo1 <= lo2 then lo1 :: hi1 :: interleave(tail1, right)
          else lo2 :: hi2 :: interleave(left, tail2)

        case _ => Nil

      def coalesce(todo: List[Int]): List[Int] = todo match
        case lo1 :: hi1 :: lo2 :: hi2 :: tail =>
          if lo2 <= hi1 + 1 then coalesce(lo1 :: Math.max(hi1, hi2) :: tail)
          else lo1 :: hi1 :: coalesce(lo2 :: hi2 :: tail)

        case other => other

      new Ranges(coalesce(interleave(spans, that.spans)))

    def negate(max: Int = Ranges.maxSymbol): Ranges =
      def recur(todo: List[Int], from: Int): List[Int] = todo match
        case lo :: hi :: tail =>
          if from < lo then from :: (lo - 1) :: recur(tail, hi + 1) else recur(tail, hi + 1)

        case _ =>
          if from <= max then from :: max :: Nil else Nil

      new Ranges(recur(spans, 0))

    def intersect(that: Ranges): Ranges = negate().union(that.negate()).negate()

    // The closure of this set under simple case folding, as RE2's `i` flag defines it. Only the
    // orbits are examined, not the whole domain, so this is proportional to the number of
    // case-varying codepoints rather than to the size of the set.
    def folded: Ranges =
      var result = this

      Ranges.foldOrbits.foreach: (_, members) =>
        var index = 0
        var hit = false

        while !hit && index < members.length do
          hit = contains(members(index))
          index += 1

        if hit then
          var at = 0

          while at < members.length do
            result = result.union(Ranges.point(members(at)))
            at += 1

      result

// A parsed RE2 regular expression: the source `pattern`, its structural `node` form, the number
// of capturing groups it contains, and its compiled `program`.
case class Motif(pattern: Text, node: Node, captures: Int, program: Motif.Program):
  def matches[input: Symbolizer](input: input): Boolean =
    Pike.run(program, input, 0, true, true).present

  def seek[input: Symbolizer](input: input, start: Ordinal = Prim): Optional[Interval] =
    Pike.run(program, input, start.n0, false, false).let: slots =>
      Interval.zerary(slots.readUnchecked(0), slots.readUnchecked(1))

  def search[input: Symbolizer as symbolizer](input: input, start: Ordinal = Prim)
  :   Chain[Interval] =

    def recur(from: Int): Chain[Interval] =
      if from > symbolizer.length(input) then Chain() else
        Pike.run(program, input, from, false, false).let: slots =>
          val interval = Interval.zerary(slots.readUnchecked(0), slots.readUnchecked(1))
          val first = slots.readUnchecked(0)
          val last = slots.readUnchecked(1)
          interval #:: recur(if last > first then last else last + 1)

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
  :   Optional[Array[Int]] =

    Pike.run(program, input, start.n0, anchored, anchored)

  // Like `groups`, but index 0 is the whole match, and when `anchored` is unset the leftmost
  // match from `start` is reported rather than requiring the whole input to match.
  def spans[input: Symbolizer](input: input, start: Ordinal = Prim, anchored: Boolean = true)
  :   Optional[List[Optional[Interval]]] =

    Pike.run(program, input, start.n0, anchored, anchored).let: slots =>
      List.tabulate(captures + 1): group =>
        val first = slots.readUnchecked(2*group)
        val last = slots.readUnchecked(2*group + 1)
        if first < 0 || last < 0 then Unset else Interval.zerary(first, last)
