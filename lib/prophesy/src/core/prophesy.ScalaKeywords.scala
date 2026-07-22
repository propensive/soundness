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
import gossamer.*
import vacuous.*

import KeywordPattern.{Element, Expectation}
import Lexeme.Bracket

// The curated pattern tree for Scala keyword completion: hand-written source, derived from —
// and checked against — the corpus analysis produced by the `keywords` tool (`make
// keywords`), which reports the observed keyword set and settling depth of every reversed
// caret context across the Soundness and proscala corpora (~445,000 keyword occurrences).
//
// Curation notes. Corpus sets are pruned of soft-modifier noise: a word like `update` or
// `open` after `def` is a method *name*, not a keyword, so definition heads carry a binding
// expectation instead. Grammar knowledge the corpus under-attests is added back: `class` and
// `object` after `case` (the scanner fuses fully-typed `case class` into one token, so the
// pair never appears in corpus contexts), and expression keywords at the start of input,
// where a REPL line may begin with an expression though a source file rarely does.
// Deliberate exclusions, as in Flame: `implicit` (superseded by `given`/`using`) and
// Scala-2-style `with` after `:` are never offered. Where a context merges two grammatical
// positions the lexeme stream cannot separate — an indented template body after `:` and a
// same-line type ascription, say — the union is offered: a keyword offered where it is not
// strictly valid is harmless, whereas a missing one is a failure.
object ScalaKeywords:
  private val expression: Set[Text] =
    Set(t"new", t"if", t"for", t"while", t"try", t"throw", t"return", t"super", t"this",
        t"true", t"false", t"null")

  private val modifiers: Set[Text] =
    Set(t"final", t"sealed", t"abstract", t"private", t"protected", t"override", t"lazy",
        t"inline", t"transparent", t"opaque", t"open", t"infix")

  private val definition: Set[Text] =
    Set(t"val", t"var", t"def", t"given", t"type", t"class", t"object", t"trait", t"enum",
        t"case", t"case class", t"case object", t"extension", t"import", t"export",
        t"package") ++
      modifiers

  private val statement: Set[Text] = definition ++ expression

  // Keywords continuing a completed expression or definition on the same line.
  private val continuation: Set[Text] =
    Set(t"match", t"with", t"then", t"else", t"do", t"yield", t"if", t"catch", t"finally",
        t"extends")

  // Modifiers valid at the head of a parameter (a `(` whose preceding tokens name a
  // definition), including `case class` member visibility.
  private val parameter: Keywords =
    Keywords
      ( Set(t"using", t"erased", t"inline", t"tracked", t"val", t"var", t"private",
            t"protected", t"final", t"override"),
        Expectation.TermBinding )

  private def leaf(keywords: Set[Text]): KeywordPattern = KeywordPattern(Keywords(keywords))

  private def leaf(keywords: Set[Text], expectation: Expectation): KeywordPattern =
    KeywordPattern(Keywords(keywords, expectation))

  private def word(text: Text): Element = Element.Exact(Lexeme.Keyword(text))
  private def glyph(text: Text): Element = Element.Exact(Lexeme.Symbol(text))

  // A definition head introducing a parameter list: `def f(`, `given (`, `extension (`,
  // `class C(` and friends. Reversed order: the `(`'s branch on `Term` then the keyword.
  private val parameterHeads: List[(Element, KeywordPattern)] =
    List(t"def", t"given", t"extension", t"class", t"trait", t"enum", t"case class")
    . map: keyword =>
        word(keyword) -> KeywordPattern(parameter)

  val pattern: KeywordPattern =
    KeywordPattern
      ( Unset,
        List
         ( // Statement boundaries: everything that can begin a statement, plus the
           // continuations that may lawfully start a fresh line (`else`, `catch`, `end`…).
           Element.Exact(Lexeme.Break) ->
             leaf(statement ++ continuation ++ Set(t"end", t"case")),

           Element.Exact(Lexeme.Start) -> leaf(statement),
           glyph(t";") -> leaf(statement),
           Element.Exact(Lexeme.Open(Bracket.Brace)) -> leaf(statement ++ Set(t"case")),

           // A definition or lambda right-hand side; an indented block after `=`/`=>` also
           // reaches here, since a more deeply indented line is not a `Break`.
           glyph(t"=") -> leaf(statement),
           glyph(t"=>") -> leaf(statement ++ Set(t"case")),
           glyph(t"?=>") -> leaf(statement),
           glyph(t"<-") -> leaf(expression),

           // A member selection: keywords are impossible apart from postfix `match`,
           // singleton `.type` and import/export `.given`; member completions remain valid.
           glyph(t".") -> leaf(Set(t"match", t"type", t"given"), Expectation.Nothing),

           // Type ascriptions, annotations and bounds expect a type; after `:` the union
           // with an indented template body's definition keywords is offered.
           glyph(t":") -> leaf(definition, Expectation.TypeIdentifier),
           glyph(t"@") -> leaf(Set(t"inline"), Expectation.TypeIdentifier),
           glyph(t"<:") -> leaf(Set(), Expectation.TypeIdentifier),
           glyph(t">:") -> leaf(Set(), Expectation.TypeIdentifier),

           // In an argument list, an expression (or a `(using …)` given argument) may
           // begin; in a parameter list — recognized one lexeme deeper by the definition
           // keyword before the name, or by the `]`/`)` of an earlier clause — a parameter
           // modifier or fresh binding is expected instead.
           Element.Exact(Lexeme.Open(Bracket.Round)) ->
             KeywordPattern
               ( Keywords(expression ++ Set(t"using", t"erased", t"inline")),
                 List
                  ( Element.Exact(Lexeme.Term) ->
                    KeywordPattern(Keywords(expression ++ Set(t"using")), parameterHeads),
                    // A class/trait name lexes as a *type* identifier, so `class Foo(` finds
                    // its parameter position through a `Typal` branch.
                    Element.Exact(Lexeme.Typal) ->
                      KeywordPattern(Keywords(expression), parameterHeads),
                    Element.Exact(Lexeme.Close(Bracket.Square)) -> KeywordPattern(parameter),
                    Element.Exact(Lexeme.Close(Bracket.Round)) ->
                      KeywordPattern(parameter) ) ),

           Element.Exact(Lexeme.Open(Bracket.Square)) ->
             leaf(Set(), Expectation.TypeIdentifier),

           glyph(t",") ->
             leaf
               ( expression ++
                 Set(t"using", t"erased", t"inline", t"val", t"var", t"final", t"private",
                     t"protected", t"override", t"given") ),

           // A completed expression, type or bracketed group: same-line continuations, plus
           // the definition keywords an indented body (after `extension (…)`, `package x`)
           // admits — the indented line is not a `Break`, so they surface here.
           Element.Exact(Lexeme.Term) ->
             KeywordPattern
               ( Keywords(continuation),
                 List
                  ( word(t"package") -> leaf(statement),
                    // An annotation: `@foo` is followed by the annotated definition.
                    glyph(t"@") -> leaf(statement) ) ),

           Element.Exact(Lexeme.Typal) ->
             leaf(continuation ++ Set(t"val", t"def", t"type", t"case", t"class", t"private",
                 t"protected")),

           Element.Exact(Lexeme.Literal) -> leaf(continuation),
           Element.Exact(Lexeme.Close(Bracket.Round)) -> leaf(continuation ++ definition),
           Element.Exact(Lexeme.Close(Bracket.Square)) -> leaf(continuation ++ definition),
           Element.Exact(Lexeme.Close(Bracket.Brace)) -> leaf(continuation ++ definition),

           // Modifier follow-sets, from the corpus with identifier noise pruned.
           word(t"private") ->
             leaf(Set(t"val", t"var", t"def", t"given", t"type", t"class", t"object",
                 t"trait", t"enum", t"case class", t"case object", t"final", t"sealed",
                 t"abstract", t"lazy", t"inline", t"transparent", t"opaque")),

           word(t"protected") ->
             leaf(Set(t"val", t"var", t"def", t"given", t"type", t"class", t"object",
                 t"trait", t"final", t"abstract", t"lazy", t"inline", t"override")),

           word(t"final") ->
             leaf(Set(t"val", t"var", t"def", t"given", t"type", t"class", t"object",
                 t"case class", t"abstract", t"lazy", t"inline", t"override", t"private",
                 t"protected")),

           word(t"override") ->
             leaf(Set(t"val", t"var", t"def", t"given", t"type", t"lazy", t"inline",
                 t"transparent", t"opaque", t"final", t"private", t"protected",
                 t"abstract")),

           word(t"sealed") -> leaf(Set(t"trait", t"class", t"abstract", t"case class")),
           word(t"abstract") -> leaf(Set(t"class", t"trait", t"override")),
           word(t"lazy") -> leaf(Set(t"val")),
           word(t"open") -> leaf(Set(t"class")),
           word(t"opaque") -> leaf(Set(t"type", t"infix")),
           word(t"infix") -> leaf(Set(t"def", t"type", t"class", t"trait", t"enum",
               t"abstract")),

           // After bare `inline`, a definition or an inline conditional may follow; after
           // `transparent inline`, only a definition can.
           word(t"inline") ->
             KeywordPattern
               ( Keywords(Set(t"def", t"given", t"val", t"if", t"match", t"infix")),
                 List(word(t"transparent") -> leaf(Set(t"def", t"given"))) ),
           word(t"transparent") -> leaf(Set(t"inline", t"trait", t"def", t"class")),
           word(t"implicit") -> leaf(Set(t"val", t"def", t"class", t"object", t"lazy",
               t"final")),

           word(t"using") -> leaf(Set(t"erased", t"inline", t"val", t"var")),
           word(t"erased") -> leaf(Set(t"given", t"val", t"inline")),
           word(t"tracked") -> leaf(Set(t"val")),
           word(t"update") -> leaf(Set(t"def", t"inline", t"private", t"protected")),

           // Definition heads: a fresh name is expected, so member completions are
           // suppressed and no keywords offered.
           word(t"def") -> leaf(Set(), Expectation.TermBinding),
           word(t"val") -> leaf(Set(), Expectation.TermBinding),
           word(t"var") -> leaf(Set(), Expectation.TermBinding),
           word(t"object") -> leaf(Set(), Expectation.TermBinding),
           word(t"type") -> leaf(Set(), Expectation.TypeBinding),
           word(t"class") -> leaf(Set(), Expectation.TypeBinding),
           word(t"trait") -> leaf(Set(), Expectation.TypeBinding),
           word(t"enum") -> leaf(Set(), Expectation.TypeBinding),
           word(t"given") -> leaf(Set()),
           word(t"package") -> leaf(Set(t"object")),

           // Selections and references.
           word(t"import") -> leaf(Set()),
           word(t"export") -> leaf(Set()),
           word(t"new") -> leaf(Set(), Expectation.TypeIdentifier),
           word(t"extends") -> leaf(Set(), Expectation.TypeIdentifier),

           // Expression positions after control keywords.
           word(t"if") -> leaf(expression),
           word(t"else") -> leaf(statement),
           word(t"then") -> leaf(statement),
           word(t"do") -> leaf(statement),
           word(t"try") -> leaf(statement),
           word(t"finally") -> leaf(expression),
           word(t"yield") -> leaf(expression),
           word(t"return") -> leaf(expression),
           word(t"throw") -> leaf(expression),
           word(t"while") -> leaf(expression),

           word(t"match") -> leaf(Set(t"case")),
           word(t"catch") -> leaf(Set(t"case")),
           word(t"for") -> leaf(Set(t"case", t"given")),
           word(t"case") -> leaf(Set(t"class", t"object", t"given")),
           word(t"with") -> leaf(Set(t"def", t"val", t"type", t"override", t"given")),
           word(t"end") -> leaf(Set(t"if", t"match", t"for", t"while", t"try", t"given",
               t"extension")) ) )
