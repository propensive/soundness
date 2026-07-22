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
package prophesy

import anticipation.*
import vacuous.*

object KeywordPattern:
  // A pattern element matches a single lexeme of the reversed context. `Exact` matches one
  // lexeme; the class elements are the wildcard mechanism, letting a curated tree generalize
  // without enumerating every literal context. Branch order in a `KeywordPattern` is
  // significance order, so `Exact` branches should precede class branches.
  enum Element:
    case Exact(lexeme: Lexeme)
    case AnyKeyword
    case ValueEnd
    case TypeEnd
    case AnyOf(lexemes: Set[Lexeme])
    case Any

    def matches(lexeme: Lexeme): Boolean = this match
      case Exact(expected) => lexeme == expected

      case AnyKeyword => lexeme match
        case Lexeme.Keyword(_) => true
        case _                 => false

      // "An expression just ended": a closing bracket, a term identifier or a literal.
      case ValueEnd => lexeme match
        case Lexeme.Close(_) | Lexeme.Term | Lexeme.Literal => true
        case _                                              => false

      // "A type just ended": a type identifier or a closing square bracket.
      case TypeEnd => lexeme match
        case Lexeme.Typal | Lexeme.Close(Lexeme.Bracket.Square) => true
        case _                                                  => false

      case AnyOf(lexemes) => lexemes.contains(lexeme)
      case Any            => true

  // What the grammar expects at the caret beyond (or instead of) keywords: a fresh term or
  // type name (suppressing member completions), a type identifier, or nothing at all (e.g.
  // after `.`, where keywords are impossible but member completions remain valid).
  enum Expectation:
    case TermBinding, TypeBinding, TypeIdentifier, Nothing

object Keywords:
  val empty: Keywords = Keywords(Set())

// The result of consulting the pattern tree: the set of keywords plausible at the caret, and
// optionally what non-keyword content the grammar expects there.
case class Keywords
  ( keywords:    Set[Text],
    expectation: Optional[KeywordPattern.Expectation] = Unset )

// A node of the pattern tree: a trie over reversed lexeme sequences. `result`, when present,
// is the keyword set determined by the context consumed so far; `branches` refine it by
// looking one lexeme further back. Lookup descends the first branch matching the next lexeme
// and returns the deepest `result` encountered — so an interior `result` acts as the default
// when no deeper pattern matches, and the tree looks back exactly as far as remains relevant.
case class KeywordPattern
  ( result:   Optional[Keywords],
    branches: List[(KeywordPattern.Element, KeywordPattern)] = Nil ):

  def apply(context: List[Lexeme]): Keywords = lookup(context, Keywords.empty)

  private def lookup(context: List[Lexeme], enclosing: Keywords): Keywords =
    val current = result.or(enclosing)

    context match
      case Nil => current

      case lexeme :: more =>
        branches.find(_(0).matches(lexeme)) match
          case Some((_, deeper)) => deeper.lookup(more, current)
          case scala.None        => current
