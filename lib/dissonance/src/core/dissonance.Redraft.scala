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
package dissonance

import anticipation.*
import contingency.*
import fulminate.*
import rudiments.*
import turbulence.*
import vacuous.*

import RedraftError.Reason


object Redraft:

  /* A single line of a `Redraft`. `Keep` is unambiguous context; `Add`/`Cut` are edits forced with
     the alternate `>`/`<` markers; `Mark` is a primary `+`/`-` line, whose meaning (an edit of its
     payload, or context matching the literal marker line) is resolved against the original. */
  enum Directive:
    case Keep(line: Text)
    case Add(line: Text)
    case Cut(line: Text)
    case Mark(line: Text, insert: Boolean)

  /* A place where a `Redraft` cannot be unambiguously applied to a particular original; `line` is
     the zero-based directive index in the redraft. */
  case class Anomaly(line: Int, text: Text, reason: Reason)

  /* How much unchanged context to include when rendering a `Diff` as a `Redraft`. */
  enum Context:
    case Minimal, Full
    case Fixed(lines: Int)

  private def marker(text: Text): Boolean =
    val s = text.s
    s == "+" || s == "-" || s == "<" || s == ">" || s.startsWith("+ ") || s.startsWith("- ")
    || s.startsWith("< ") || s.startsWith("> ")

  private def needsEscape(text: Text): Boolean = marker(text) || text.s.startsWith("\\")

  private def payload(text: Text): Text =
    (if text.s.length >= 2 then text.s.substring(2).nn else "").tt

  def parse(lines: Stream[Text]): Redraft =
    val directives = lines.map: line =>
      val s = line.s
      if s == "+" || s.startsWith("+ ") then Directive.Mark(payload(line), insert = true)
      else if s == "-" || s.startsWith("- ") then Directive.Mark(payload(line), insert = false)
      else if s == ">" || s.startsWith("> ") then Directive.Add(payload(line))
      else if s == "<" || s.startsWith("< ") then Directive.Cut(payload(line))
      else if s.startsWith("\\") then Directive.Keep(s.substring(1).nn.tt)
      else Directive.Keep(line)

    Redraft(directives.to(List)*)

  private def render1(directive: Directive): Text = directive match
    case Directive.Keep(line)        => if needsEscape(line) then ("\\"+line.s).tt else line
    case Directive.Add(line)         => ("> "+line.s).tt
    case Directive.Cut(line)         => ("< "+line.s).tt
    case Directive.Mark(line, true)  => ("+ "+line.s).tt
    case Directive.Mark(line, false) => ("- "+line.s).tt

  /* The result of aligning the matcher lines (`Keep`/`Cut` and resolved `Mark`s) against the
     original, recording for each matcher its text, the leftmost index it matched, and the directive
     index that produced it. */
  private case class Matcher(text: Text, index: Int, line: Int)

  /* Apply a redraft's directives to an original by leftmost subsequence matching, producing the full
     `Edit` list together with any anomalies (no-match, ambiguity, or insufficient anchoring). This
     never raises; `resolve` and `verify` interpret the anomalies. */
  private def analyze
              (directives: List[Directive],
               original:   IndexedSeq[Text],
               compare:    (Text, Text) => Boolean)
  :   (List[Edit[Text]], List[Anomaly]) =

    val n = original.length

    def seek(from: Int, text: Text): Int =
      var j = from
      while j < n && !compare(original(j), text) do j += 1
      if j < n then j else -1

    var cursor = 0
    var right = 0
    var edits: List[Edit[Text]] = Nil
    var matchers: List[Matcher] = Nil
    var anomalies: List[Anomaly] = Nil

    def keepUpTo(target: Int): Unit =
      var k = cursor
      while k < target do
        edits = Par(k, right, original(k)) :: edits
        right += 1
        k += 1
      cursor = target

    def keep(index: Int, text: Text, line: Int): Unit =
      keepUpTo(index)
      edits = Par(index, right, original(index)) :: edits
      matchers = Matcher(text, index, line) :: matchers
      right += 1
      cursor = index + 1

    def cut(index: Int, text: Text, line: Int): Unit =
      keepUpTo(index)
      edits = Del(index, original(index)) :: edits
      matchers = Matcher(text, index, line) :: matchers
      cursor = index + 1

    def add(text: Text): Unit =
      edits = Ins(right, text) :: edits
      right += 1

    directives.zipWithIndex.each: (directive, line) =>
      directive match
        case Directive.Keep(text) =>
          val index = seek(cursor, text)
          if index < 0 then anomalies = Anomaly(line, text, Reason.NoMatch) :: anomalies
          else keep(index, text, line)

        case Directive.Add(text) =>
          add(text)

        case Directive.Cut(text) =>
          val index = seek(cursor, text)
          if index < 0 then anomalies = Anomaly(line, text, Reason.NoMatch) :: anomalies
          else cut(index, text, line)

        case Directive.Mark(load, insert) =>
          val literal = ((if insert then "+ " else "- ")+load.s).tt
          val contextIndex = seek(cursor, literal)
          val editIndex = if insert then cursor else seek(cursor, load)
          val contextViable = contextIndex >= 0
          val editViable = insert || editIndex >= 0

          if contextViable && editViable
          then anomalies = Anomaly(line, literal, Reason.Ambiguous) :: anomalies
          else if contextViable then keep(contextIndex, literal, line)
          else if editViable then (if insert then add(load) else cut(editIndex, load, line))
          else anomalies = Anomaly(line, literal, Reason.NoMatch) :: anomalies

    keepUpTo(n)

    val ordered = matchers.reverse

    // Rightmost greedy alignment of the same matcher texts; where it disagrees with the leftmost
    // alignment, the matcher is under-anchored.
    @tailrec
    def alignRight(todo: List[Matcher], limit: Int, acc: List[Int]): Optional[List[Int]] =
      todo match
        case Nil => acc
        case matcher :: rest =>
          var j = limit
          while j >= 0 && !compare(original(j), matcher.text) do j -= 1
          if j < 0 then Unset else alignRight(rest, j - 1, j :: acc)

    alignRight(ordered.reverse, n - 1, Nil).let: rightmost =>
      ordered.zip(rightmost).each: (matcher, rightIndex) =>
        if matcher.index != rightIndex
        then anomalies = Anomaly(matcher.line, matcher.text, Reason.Unanchored) :: anomalies

    (edits.reverse, anomalies.sortBy(_.line))

  def render(diff: Diff[Text], context: Context): Redraft =
    val original: IndexedSeq[Text] = diff.edits.collect:
      case Par(_, _, value) => value.vouch
      case Del(_, value)    => value.vouch
    . to(IndexedSeq)

    val full: List[Directive] = diff.edits.to(List).flatMap:
      case Par(_, _, value) => List(Directive.Keep(value.vouch))
      case Del(_, value)    => List(Directive.Mark(value.vouch, insert = false))
      case Ins(_, value)    => List(Directive.Mark(value, insert = true))

    // Promote any ambiguous primary `+`/`-` marker to the forced `>`/`<` spelling.
    def deambiguate(directives: List[Directive]): List[Directive] =
      val ambiguous =
        analyze(directives, original, _ == _)(1).collect:
          case Anomaly(line, _, Reason.Ambiguous) => line

      . to(Set)

      if ambiguous.isEmpty then directives
      else deambiguate:
        directives.zipWithIndex.map: (directive, index) =>
          if !ambiguous.contains(index) then directive else directive match
            case Directive.Mark(load, true)  => Directive.Add(load)
            case Directive.Mark(load, false) => Directive.Cut(load)
            case other                       => other

    val resolved = deambiguate(full)

    context match
      case Context.Full     => Redraft(resolved*)
      case Context.Fixed(k) => Redraft(trim(resolved, k)*)
      case Context.Minimal  => Redraft(minimize(resolved, original, diff.patch(original).to(List))*)

  /* Keep only unchanged lines within `k` directives of a change. */
  private def trim(directives: List[Directive], k: Int): List[Directive] =
    val keep = directives.map { case Directive.Keep(_) => false; case _ => true }.to(Array)
    val n = keep.length

    def near(index: Int): Boolean =
      (1 to k).exists: d =>
        (index - d >= 0 && keep(index - d)) || (index + d < n && keep(index + d))

    directives.zipWithIndex.collect:
      case (directive, index) if !directive.isInstanceOf[Directive.Keep] || near(index) => directive

  /* Greedily drop unchanged context, preferring lines furthest from any change, retaining a drop
     only while the redraft still reproduces the target and stays unambiguous. */
  private def minimize
              (directives: List[Directive], original: IndexedSeq[Text], target: List[Text])
  :   List[Directive] =

    val array = directives.to(Array)
    val dropped = scala.collection.mutable.Set[Int]()

    def remaining: List[Directive] =
      array.indices.to(List).filter(!dropped.contains(_)).map(array(_))

    def valid(candidate: List[Directive]): Boolean =
      val (edits, anomalies) = analyze(candidate, original, _ == _)
      anomalies.isEmpty && Diff(edits*).patch(original).to(List) == target

    val changes = array.indices.filter(i => !array(i).isInstanceOf[Directive.Keep])

    def distance(index: Int): Int =
      if changes.isEmpty then 0 else changes.map(c => (c - index).abs).min

    val order = array.indices
                . filter(array(_).isInstanceOf[Directive.Keep])
                . sortBy(index => -distance(index))

    order.each: index =>
      dropped += index
      if !valid(remaining) then dropped -= index

    remaining


case class Redraft(directives: Redraft.Directive*):
  def serialize: Stream[Text] = directives.map(Redraft.render1).to(Stream)

  def resolve(original: IndexedSeq[Text], compare: (Text, Text) => Boolean = _ == _)
  :   Diff[Text] raises RedraftError =

    val (edits, anomalies) = Redraft.analyze(directives.to(List), original, compare)

    anomalies match
      case anomaly :: _ => abort(RedraftError(anomaly.line, anomaly.text, anomaly.reason))
      case Nil          => Diff(edits*)

  def verify(original: IndexedSeq[Text], compare: (Text, Text) => Boolean = _ == _)
  :   List[Redraft.Anomaly] =

    Redraft.analyze(directives.to(List), original, compare)(1)

  def patch(original: IndexedSeq[Text], compare: (Text, Text) => Boolean = _ == _)
  :   Stream[Text] raises RedraftError =

    resolve(original, compare).patch(original)
