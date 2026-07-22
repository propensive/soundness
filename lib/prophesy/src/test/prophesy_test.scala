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

import soundness.*

import prophesy.KeywordPattern.{Element, Expectation}
import prophesy.Lexeme.Bracket

object Tests extends Suite(m"Prophesy tests"):
  def run(): Unit =
    // A miniature tree exercising the lookup semantics; the real Scala tree is exercised
    // end-to-end from harlequin's tests. Reversed contexts read caret-outwards: the head is
    // the lexeme immediately before the caret.
    val statement = Keywords(Set(t"val", t"var", t"def", t"if"))
    val definition = Keywords(Set(t"def", t"given"))
    val parameter = Keywords(Set(t"using"), Expectation.TermBinding)

    val tree = KeywordPattern
      ( Unset,
        List
         ( Element.Exact(Lexeme.Break) -> KeywordPattern(statement),
           Element.Exact(Lexeme.Start) -> KeywordPattern(statement),
           Element.Exact(Lexeme.Symbol(t".")) -> KeywordPattern
             ( Keywords(Set(), Expectation.Nothing) ),
           Element.Exact(Lexeme.Keyword(t"inline")) -> KeywordPattern
             ( definition,
               List
                ( Element.Exact(Lexeme.Keyword(t"transparent")) -> KeywordPattern(definition),
                  Element.Exact(Lexeme.Open(Bracket.Round)) -> KeywordPattern(parameter) ) ),
           Element.ValueEnd -> KeywordPattern(Keywords(Set(t"match"))) ) )

    suite(m"Pattern-tree lookup"):
      test(m"an empty context yields the empty result"):
        tree(Nil)
      . assert(_ == Keywords.empty)

      test(m"a statement boundary offers statement keywords"):
        tree(List(Lexeme.Break))
      . assert(_ == statement)

      test(m"start of input offers statement keywords"):
        tree(List(Lexeme.Start))
      . assert(_ == statement)

      test(m"after a dot, keywords are impossible but members remain valid"):
        tree(List(Lexeme.Symbol(t".")))
      . assert(_ == Keywords(Set(), Expectation.Nothing))

      test(m"the issue's example: transparent inline unambiguously offers definitions"):
        tree(List(Lexeme.Keyword(t"inline"), Lexeme.Keyword(t"transparent")))
      . assert(_ == definition)

      test(m"the same last token in a parameter list resolves differently"):
        tree
          ( List
             ( Lexeme.Keyword(t"inline"),
               Lexeme.Open(Bracket.Round),
               Lexeme.Term,
               Lexeme.Keyword(t"def") ) )
      . assert(_ == parameter)

      test(m"an unmatched deeper context falls back to the enclosing result"):
        tree(List(Lexeme.Keyword(t"inline"), Lexeme.Literal))
      . assert(_ == definition)

      test(m"a class element matches any of its members"):
        (tree(List(Lexeme.Term)), tree(List(Lexeme.Close(Bracket.Brace))))
      . assert(_ == (Keywords(Set(t"match")), Keywords(Set(t"match"))))

      test(m"an exact branch takes precedence over a later class branch"):
        // `Term` also matches `ValueEnd`, but the keyword branch is listed first for the
        // `inline` lexeme, which is not a `Term`; conversely a `Term` never reaches the
        // keyword branches.
        tree(List(Lexeme.Keyword(t"inline")))
      . assert(_ == definition)

      test(m"lookup consumes no more context than the tree's depth"):
        val deep = List.fill(10)(Lexeme.Term) :+ Lexeme.Break
        tree(Lexeme.Symbol(t".") :: deep)
      . assert(_ == Keywords(Set(), Expectation.Nothing))
