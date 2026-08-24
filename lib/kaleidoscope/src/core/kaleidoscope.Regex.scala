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
package kaleidoscope

import java.util.regex as jur

import scala.collection.concurrent.TrieMap
import scala.language.experimental.pureFunctions

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import praxinoscope.*
import prepositional.*
import rudiments.*
import vacuous.*

import Regex.Error.Reason.*
import denominative.dysasymptotics.linearSize

object Regex:
  private[kaleidoscope] val cache: TrieMap[String, jur.Pattern] = TrieMap()

  object Engine:
    private val motifs: TrieMap[String, Motif] = TrieMap()

    // The praxinoscope form of `regex`, cached by its rendered pattern. `r"…"` literals compiled
    // under `regexBackends.re2` were validated during macro expansion; a `Regex` retagged with
    // `to[Re2]` at runtime is validated here on first use instead, throwing `Motif.Error` if it
    // strays outside the RE2 subset.
    private[kaleidoscope] def motif(regex: Regex): Motif = compile(regex.plainPattern)

    // Pre-seeds the cache with a Motif staged at compile time, so an `r"…"` literal compiled
    // under `regexBackends.re2` never parses or compiles its pattern at runtime.
    private[kaleidoscope] def install(motif: Motif): Unit =
      if !motifs.contains(motif.pattern.s) then motifs(motif.pattern.s) = motif

    private[kaleidoscope] def compile(pattern: Text): Motif =
      motifs.getOrElseUpdate
        ( pattern.s,
          { given tactic: (ThrowTactic[Hazard, Any]^) = strategies.throwUnsafely
            Motif.parse(pattern) } )

    // `Re2`'s companion sits below this library, so per issue #1632 the instance lives in the
    // typeclass companion. It bridges to praxinoscope: the pattern is re-rendered with a plain
    // parenthesis around every group (`plainPattern`), and `captureIndices` maps kaleidoscope's
    // capture groups onto praxinoscope's paren-ordered group numbers.
    given re2: Re2 is Regex.Engine:
      def matches(regex: Regex, text: Text)(using scanner: Scanner): Boolean =
        scanner.nextStart match
          case index: Int =>
            motif(regex).slots(text, Ordinal.zerary(index), false).lay(false): slots =>
              scanner.nextStart = slots(0) + 1
              scanner.matchEnd = slots(1)
              true

          case _ =>
            motif(regex).matches(text)

      def seek(regex: Regex, input: Text, start: Ordinal): Optional[Interval] =
        motif(regex).slots(input, start, false).let: slots => Interval.zerary(slots(0), slots(1))

      def search(regex: Regex, input: Text, start: Ordinal, overlap: Boolean): Chain[Interval] =
        val compiled = motif(regex)

        // Restarting from `end + 1` (not `end`) after a non-overlapping match reproduces the
        // `Jur` engine's behaviour exactly, including its skipping of immediately-adjacent
        // matches. Positions advance over the raw slot bounds, since an empty match's
        // `Interval` does not record where it occurred.
        def recur(from: Int): Chain[Interval] =
          if from > input.s.length then Chain() else
            compiled.slots(input, Ordinal.zerary(from), false).lay(Chain()): slots =>
              val next = if overlap then slots(0) + 1 else slots(1) + 1
              Interval.zerary(slots(0), slots(1)) #:: recur(next)

        recur(start.n0)


      private[kaleidoscope] def matchGroups(regex: Regex, text: Text)(using scanner: Scanner)
      :   Option[Array[List[Text | Char] | Optional[Text | Char]]^{}] =

        val compiled = motif(regex)

        val found = scanner.nextStart match
          case index: Int => compiled.slots(text, Ordinal.zerary(index), false)
          case _          => compiled.slots(text)

        found.let: slots =>
          scanner.nextStart match
            case started: Int =>
              scanner.nextStart = slots(0) + 1
              scanner.matchEnd = slots(1)

            case _ =>
              ()

          // The praxinoscope analogue of the `Jur` engine's sub-scan: a repeating group's span
          // covers all its iterations, so the group's body is matched repeatedly over the span
          // to recover each iteration.
          def rescan(body: Text, region: Text): List[Text] =
            val sub = compile(body)

            def recur(from: Int, results: List[Text]): List[Text] =
              if from > region.s.length then results.reverse else
                sub.slots(region, Ordinal.zerary(from), false).lay(results.reverse): bounds =>
                  val skip = if bounds(1) > bounds(0) then bounds(1) else bounds(1) + 1
                  recur(skip, region.s.substring(bounds(0), bounds(1)).nn.tt :: results)

            recur(0, Nil)


          def recur
            ( todo:    List[Regex.Group],
              indices: List[Int],
              values:  List[Optional[Text | Char] | List[Text | Char]] )
          :   List[Optional[Text | Char] | List[Text | Char]] =

            (todo, indices) match
              case (group :: groups, index :: rest) =>
                val first = slots(2*index)
                val last = slots(2*index + 1)
                val matched = if first < 0 then "".tt else text.s.substring(first, last).nn.tt
                val optional = group.quantifier == Regex.Quantifier.Between(0, 1)

                val value: Optional[Text | Char] | List[Text | Char] =
                  if group.charMatcher then
                    if group.quantifier.unitary then
                      matched.s.charAt(0)
                    else if optional then
                      if matched.s.length > 0 then matched.s.charAt(0) else Unset
                    else
                      matched.s.toCharArray.nn.iterator.to(List)
                  else if group.quantifier.unitary then
                    matched
                  else
                    val body = regex.pattern.s.substring(group.start, group.end).nn.tt
                    if optional then rescan(body, matched).prim else rescan(body, matched)

                recur(groups, rest, value :: values)

              case _ =>
                values

          val values = recur(regex.captureGroups, regex.captureIndices, Nil)
          Some(Array.frozen(scala.IArray.from(values.stdlib.reverse)))

        . or(None)

  // A matching backend, selected by the `Form` refinement of the `Regex` it operates on:
  // `Regex in Jur` dispatches to `java.util.regex` and `Regex in Re2` to praxinoscope. The
  // `Jur` instance lives in `Jur`'s companion; `Re2`'s companion is below this library, so its
  // instance lives in this trait's companion instead (issue #1632).
  trait Engine:
    type Self

    def matches(regex: Regex, text: Text)(using Scanner): Boolean
    def seek(regex: Regex, input: Text, start: Ordinal): Optional[Interval]
    def search(regex: Regex, input: Text, start: Ordinal, overlap: Boolean): Chain[Interval]

    private[kaleidoscope] def matchGroups(regex: Regex, text: Text)(using Scanner)
    :   Option[Array[List[Text | Char] | Optional[Text | Char]]^{}]

  enum Greed:
    case Greedy, Reluctant, Possessive

    def serialize: Text = this match
      case Greedy     => "".tt
      case Reluctant  => "?".tt
      case Possessive => "+".tt

  enum Quantifier:
    case Exactly(start: Int)
    case AtLeast(start: Int)
    case Between(start: Int, end: Int)

    def serialize: Text = this match
      case Exactly(1)          => "".tt
      case Exactly(start)      => s"{$start}".tt
      case AtLeast(1)          => "+".tt
      case AtLeast(0)          => "*".tt
      case AtLeast(start)      => s"{$start,}".tt
      case Between(0, 1)       => "?".tt
      case Between(start, end) => s"{$start,$end}".tt

    def unitary: Boolean = this == Exactly(1)

  case class Group
    ( start:      Int,
      end:        Int,
      outerEnd:   Int,
      groups:     List[Group] = Nil,
      quantifier: Quantifier  = Quantifier.Exactly(1),
      greed:      Greed       = Greed.Greedy,
      capture:    Boolean     = false,
      charClass:  Boolean     = false,
      singleChar: Boolean     = false ):

    def outerStart: Int = if singleChar && !charClass then start else (start - 1).max(0)

    def allGroups: List[Regex.Group] = groups.bind: group =>
      (group :: group.allGroups): List[Regex.Group]

    def captureGroups: List[Regex.Group] = allGroups.filter(_.capture)
    def charMatcher: Boolean = charClass || singleChar

    def serialize(pattern: Text, index: Int, named: Boolean): (Int, Text) =
      if charClass then
        val groupName = (if capture && named then s"?<g$index>" else "").tt

        if quantifier.unitary then (index, s"($groupName[${pattern.s.substring(start, end)}])")
        else
          val chars = pattern.s.substring(start, end)
          (index, s"($groupName[$chars]${quantifier.serialize}${greed.serialize})")
      else if singleChar then
        val groupName = (if capture && named then s"?<g$index>" else "").tt
        val token = pattern.s.substring(start, end)

        if quantifier.unitary then (index, s"($groupName$token)")
        else (index, s"($groupName$token${quantifier.serialize}${greed.serialize})")
      else
        val (index2, subpattern) =
          Regex.makePattern(pattern, groups, start, "".tt, end, index, named)

        val groupName = (if capture && named then s"?<g$index>" else "").tt

        if quantifier.unitary then (index2, s"($groupName$subpattern)".tt)
        else (index2, s"($groupName($subpattern)${quantifier.serialize}${greed.serialize})".tt)

  def apply(parts: List[String])(using erased unsafe: Unsafe): Regex =
    given tactic: (ThrowTactic[Hazard, Any]^) = strategies.throwUnsafely
    parse(parts.map(_.tt))

  def apply(text: Text): Regex in Jur raises Regex.Error = parse(List(text))

  def parse(parts: List[Text]): Regex in Jur raises Regex.Error =
    def validStart(part: Text): Boolean =
      val str = part.s
      str.startsWith("(") || str.startsWith("[") || str.startsWith(".") ||
        (str.length >= 2 && str.charAt(0) == '\\' && "dDwWsS".indexOf(str.charAt(1)) >= 0)

    parts.absolve match
      case head :: tail =>
        if !tail.all(validStart) then abort(Regex.Error(0, ExpectedGroup))

    def captures(todo: List[Text], last: Int, done: Set[Int]): Set[Int] = todo.absolve match
      case Nil          => done
      case head :: tail => captures(tail, last + head.s.length, done :+ last)

    val captured: Set[Int] =
      if parts.size > 1
      then captures(parts.stdlib.tail.to(List), parts.stdlib.head.s.size, Set())
      else Set()

    val text: Text = parts.stdlib.mkString.tt

    var index: Int = 0

    def current(): Char = if index >= text.s.length then '\u0000' else text.s.charAt(index)

    extension [value](value: value) def adv(): value = value.also { index += 1 }

    def greed(): Greed = current() match
      case '?' => Greed.Reluctant.adv()
      case '+' => Greed.Possessive.adv()
      case _   => Greed.Greedy

    def quantifier(): Quantifier = current() match
      case '\u0000' => Quantifier.Exactly(1)
      case '*'      => Quantifier.AtLeast(0).adv()
      case '+'      => Quantifier.AtLeast(1).adv()
      case '?'      => Quantifier.Between(0, 1).adv()

      case '{' =>
        index += 1
        val n = number(true)

        val quantifier = current() match
          case '}' =>
            Quantifier.Exactly(n)

          case ',' =>
            index += 1

            if current() == '}' then Quantifier.AtLeast(n)
            else number(false) match
              case 0 =>
                abort(Regex.Error(index - 1, ZeroMaximum))

              case m =>
                if m < n then abort(Regex.Error(index - 1, BadRepetition))
                else Quantifier.Between(n, m)

          case _ =>
            abort(Regex.Error(index, UnexpectedChar))

        if current() != '}' then abort(Regex.Error(index, UnexpectedChar)) else quantifier.adv()

      case _ =>
        Quantifier.Exactly(1)

    @tailrec
    def number(required: Boolean, count: Int = 0, first: Boolean = true): Int = current() match
      case '\u0000' => abort(Regex.Error(index, IncompleteRepetition))

      case ch if ch.isDigit =>
        index += 1
        number(required, count*10 + (ch - '0').toInt, false)

      case ',' =>
        if !required then abort(Regex.Error(index, UnexpectedChar)) else count

      case '}' =>
        if first && required then abort(Regex.Error(index, UnexpectedChar)) else count

      case other =>
        abort(Regex.Error(index, UnexpectedChar))


    def group(start: Int, children: List[Group], top: Boolean, escape: Boolean, charClass: Boolean)
    :   Group =

      current() match
        case '\u0000' =>
          if !top then abort(Regex.Error(index, UnclosedGroup))

          Group(start, index, (index + 1).min(text.s.length), children.reverse,
              Quantifier.Exactly(1), Greed.Greedy, captured.has(start - 1), false)

        case '.' if !escape && !charClass && captured.has(index) =>
          val groupStart = index
          index += 1
          val tokenEnd = index
          val q = quantifier()
          val g = greed()

          val newGroup = Group(groupStart, tokenEnd, index, Nil, q, g, true, false, true)
          group(start, newGroup :: children, top, false, false)

        case '\\' if !escape && !charClass && captured.has(index) &&
          index + 1 < text.s.length &&
          "dDwWsS".indexOf(text.s.charAt(index + 1)) >= 0 =>

          val groupStart = index
          index += 2
          val tokenEnd = index
          val q = quantifier()
          val g = greed()

          val newGroup = Group(groupStart, tokenEnd, index, Nil, q, g, true, false, true)
          group(start, newGroup :: children, top, false, false)

        case '\\' =>
          index += 1
          group(start, children, top, !escape, charClass)

        case char if escape =>
          index += 1
          group(start, children, top, false, charClass)

        case '[' if !charClass =>
          index += 1
          group(start, group(index, Nil, false, false, true) :: children, top, false, false)

        case ']' if charClass =>
          if index - 1 == start then abort(Regex.Error(index, EmptyCharClass))
          index += 1
          if top then abort(Regex.Error(index - 1, NotInGroup))
          val end = index - 1
          val quantifier2 = quantifier()
          val greed2 = greed()

          Group(start, end, index, Nil, quantifier2, greed2, captured.has(start - 1), true)

        case char if charClass =>
          index += 1
          group(start, children, top, false, charClass)

        case '(' =>
          index += 1
          group(start, group(index, Nil, false, false, false) :: children, top, false, false)

        case ')' =>
          index += 1
          if top then abort(Regex.Error(index - 1, NotInGroup))
          val end = index - 1
          val quantifier2 = quantifier()
          val greed2 = greed()

          Group
            ( start,
              end,
              index,
              children.reverse,
              quantifier2,
              greed2,
              captured.has(start - 1),
              false )

        case _ =>
          index += 1
          group(start, children, top, false, charClass)


    val mainGroup = group(0, Nil, true, false, false)

    def check(groups: List[Group], canCapture: Boolean): Unit =
      groups.each: group =>
        if !canCapture && group.capture then abort(Regex.Error(group.start - 1, Uncapturable))
        check(group.groups, canCapture && group.quantifier.unitary)

    check(mainGroup.groups, true)

    Regex(text, mainGroup.groups).to[Jur]


  def makePattern
    ( pattern: Text,
      todo:    List[Group],
      last:    Int,
      text:    Text,
      end:     Int,
      index:   Int,
      named:   Boolean = true )
  :   (Int, Text) =

    todo.absolve match
      case Nil => (index, (text.s+pattern.s.substring(last, end).nn).tt)

      case head :: tail =>
        val (index2, subpattern) = head.serialize(pattern, index, named)
        val partial = text.s+pattern.s.substring(last, head.outerStart)+subpattern.nn
        val index3 = if head.capture then index2 + 1 else index2

        makePattern(pattern, tail, head.outerEnd, partial.tt, end, index3, named)

  // RegexError → Regex.Error
  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case UnclosedGroup => m"a capturing group was not closed"

        case ExpectedGroup =>
          m"a capturing group was expected immediately following an extractor"

        case BadRepetition =>
          m"the maximum number of repetitions is less than the minimum"

        case Uncapturable =>
          m"a capturing group inside a repeating group can not be extracted"

        case UnexpectedChar =>
          m"the repetition range contained an unexpected character"

        case NotInGroup =>
          m"a closing parenthesis was found without a corresponding opening parenthesis"

        case IncompleteRepetition =>
          m"the repetition range was not closed"

        case InvalidPattern =>
          m"the pattern was invalid"

        case UnclosedEscape =>
          m"nothing followed the escape character `\\`"

        case EmptyCharClass =>
          m"the character class is empty"

        case ZeroMaximum =>
          m"the maximum number of repetitions must be greater than zero"

    enum Reason(val number: Int) extends Clarification:
      case UnclosedGroup       extends Reason(1)
      case ExpectedGroup       extends Reason(2)
      case BadRepetition       extends Reason(3)
      case Uncapturable        extends Reason(4)
      case UnexpectedChar      extends Reason(5)
      case NotInGroup          extends Reason(6)
      case IncompleteRepetition extends Reason(7)
      case InvalidPattern      extends Reason(8)
      case UnclosedEscape      extends Reason(9)
      case EmptyCharClass      extends Reason(10)
      case ZeroMaximum         extends Reason(11)

  case class Error(index: Int, reason: Regex.Error.Reason)(using Diagnostics)
  extends fulminate.Error(397, reason.number)
    ( m"the regular expression could not be parsed because $reason at $index" )

case class Regex(pattern: Text, groups: List[Regex.Group]) extends Formal:
  def to[form: Regex.Engine]: Regex in form = asInstanceOf[Regex in form]

  def unapply(text: Text)(using engine: Form is Regex.Engine): Boolean =
    engine.matches(this, text)(using Scanner(Unset))

  // Matching operations are methods rather than extensions so they cannot collide with the
  // generic collection extensions of the same names in the `soundness` package; the engine
  // resolves from the `Form` refinement, so a `Regex in Re2` can never silently fall back to
  // `java.util.regex`.
  def matches(text: Text)(using scanner: Scanner, engine: Form is Regex.Engine): Boolean =
    engine.matches(this, text)

  def seek(input: Text, start: Ordinal = Prim)(using engine: Form is Regex.Engine)
  :   Optional[Interval] =

    engine.seek(this, input, start)

  def search(input: Text, start: Ordinal = Prim, overlap: Boolean = false)
    ( using engine: Form is Regex.Engine )
  :   Chain[Interval] =

    engine.search(this, input, start, overlap)

  lazy val capturePattern: Text =
    Regex.makePattern(pattern, groups, 0, "".tt, pattern.s.length, 0)(1)

  // The pattern re-rendered with a plain `(…)` around every group, which is how it is handed to
  // praxinoscope: RE2 syntax has no named groups, so capture groups are identified by paren
  // order via `captureIndices` instead.
  private[kaleidoscope] lazy val plainPattern: Text =
    Regex.makePattern(pattern, groups, 0, "".tt, pattern.s.length, 0, false)(1)

  // For each capture group (in `captureGroups` order), the 1-based index of its outer
  // parenthesis in `plainPattern`, mirroring the paren-emission order of `Group.serialize`: one
  // paren per group, plus an inner one for a quantified non-character group.
  private[kaleidoscope] lazy val captureIndices: List[Int] =
    var indices: List[Int] = Nil
    var parens: Int = 0

    def walk(groups: List[Regex.Group]): Unit =
      groups.each: group =>
        parens += 1
        if group.capture then indices = parens :: indices
        if !group.charMatcher && !group.quantifier.unitary then parens += 1
        if !group.charMatcher then walk(group.groups)

    walk(groups)
    indices.reverse

  // Containment analyses are available only on RE2-tagged values, whose lack of backreferences
  // and lookaround is what makes the question decidable.
  def subsumes(that: Regex in Re2)(using Form =:= Re2): Boolean raises Motif.Error =
    Regex.Engine.motif(this).subsumes(Regex.Engine.motif(that))

  def intersects(that: Regex in Re2)(using Form =:= Re2): Boolean raises Motif.Error =
    Regex.Engine.motif(this).intersects(Regex.Engine.motif(that))

  def allGroups: List[Regex.Group] = groups.bind: group =>
    (group :: group.allGroups): List[Regex.Group]

  def captureGroups: List[Regex.Group] = allGroups.filter(_.capture)

  private[kaleidoscope] lazy val javaPattern: jur.Pattern =
    Regex.cache.getOrElseUpdate(capturePattern.s, jur.Pattern.compile(capturePattern.s).nn)
