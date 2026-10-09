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
package cataclysm

// Deliberate stdlib opt-out: internal keyword tables used as predicates.
import scala.collection.immutable.Set

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

// Checks a CSS property value against its `Css.Syntax` grammar. Composite `<type>`s
// are resolved lazily from the bundled `syntaxes.json` (which expand to keywords,
// functions and a handful of leaf primitives); leaf primitives (`<length>`,
// `<color>` parts, …) are matched at the token level. A value containing an
// arbitrary-substitution function (`var()`, `env()`) is always valid (it is a
// pending-substitution value). `calc()` and friends are accepted wherever a
// numeric primitive is expected. Any `<type>` that is neither resolvable nor an
// implemented primitive yields `Outcome.Unsupported`.
object SyntaxMatcher:
  // The composite syntaxes, from the string tables compiled from the bundled dataset at build
  // time (`CssData`).
  private lazy val rawComposites: Dictionary[Text] =
    val pairs = List.tabulate(CssData.syntaxNames.length): index =>
      (CssData.syntaxNames(index).tt, CssData.syntaxSyntaxes(index).tt)

    Dictionary(pairs*)

  private val cache: scala.collection.mutable.HashMap[Text, Optional[Css.Syntax]] =
    scala.collection.mutable.HashMap()

  private def composite(name: Text): Optional[Css.Syntax] =
    cache.getOrElseUpdate(name, rawComposites(name).let(parsed))

  private def parsed(raw: Text): Optional[Css.Syntax] = safely(SyntaxParser.parse(raw))

  private val lengthUnits: Set[Text] =
    Set
      ( t"px", t"em", t"rem", t"ex", t"ch", t"cap", t"ic", t"lh", t"rlh", t"vw", t"vh", t"vi",
        t"vb", t"vmin", t"vmax", t"svw", t"svh", t"lvw", t"lvh", t"dvw", t"dvh", t"cm", t"mm", t"q",
        t"in", t"pt", t"pc" )

  private val angleUnits: Set[Text] = Set(t"deg", t"grad", t"rad", t"turn")
  private val timeUnits: Set[Text] = Set(t"s", t"ms")
  private val resolutionUnits: Set[Text] = Set(t"dpi", t"dpcm", t"dppx", t"x")
  private val frequencyUnits: Set[Text] = Set(t"hz", t"khz")
  private val flexUnits: Set[Text] = Set(t"fr")
  private val substitutions: Set[Text] = Set(t"var", t"env")

  // The CSS-wide keywords are valid as the sole value of every property, but appear
  // in no property's grammar, so they are accepted before grammar matching.
  private val globalKeywords: Set[Text] = Set(t"inherit", t"initial", t"unset", t"revert",
      t"revert-layer")

  private val mathFunctions: Set[Text] =
    Set
      ( t"calc", t"min", t"max", t"clamp", t"sin", t"cos", t"tan", t"asin", t"acos", t"atan",
        t"atan2", t"pow", t"sqrt", t"hypot", t"log", t"exp", t"abs", t"sign", t"mod", t"rem",
        t"round" )

  private def substitution(token: ValueToken): Boolean = token match
    case ValueToken.Function(name) => substitutions(name.lower)
    case _                         => false

  private def globalKeyword(tokens: List[ValueToken]): Boolean = tokens match
    case List(ValueToken.Ident(value)) => globalKeywords(value.lower)
    case _                             => false

  def check(property: PropertyDef, value: Text)(using Tactic[Css.Error]): Outcome =
    check(property.grammar, value)

  def check(grammar: Css.Syntax, value: Text)(using Tactic[Css.Error]): Outcome =
    val tokens = ValueTokenizer.tokens(value).filter(_ != ValueToken.Whitespace)

    if tokens.exists(substitution) then Outcome.Valid
    else if globalKeyword(tokens) then Outcome.Valid
    else
      val matcher = Matcher()

      if matcher.consume(grammar, tokens).exists(_.nil) then Outcome.Valid
      else if matcher.unsupported.nonEmpty then Outcome.Unsupported(matcher.unsupported.to(List))
      else Outcome.Invalid

  // ── the backtracking matcher ─────────────────────────────────────────────

  // `consume` returns every possible list of tokens remaining after matching
  // `syntax` against a prefix of `tokens`. A full match exists when some
  // remainder is empty.
  private class Matcher:
    val unsupported: scala.collection.mutable.LinkedHashSet[Text] =
      scala.collection.mutable.LinkedHashSet()

    private val resolving: scala.collection.mutable.HashSet[Text] =
      scala.collection.mutable.HashSet()

    def consume(syntax: Css.Syntax, tokens: List[ValueToken]): List[List[ValueToken]] = syntax match
      case Css.Syntax.Keyword(name) =>
        keyword(name, tokens)

      case Css.Syntax.Literal(token) =>
        literal(token, tokens)

      case Css.Syntax.Type(name, _) =>
        typeMatch(name, tokens)

      case Css.Syntax.Property(name) =>
        propertyMatch(name, tokens)

      case Css.Syntax.Function(name, body) =>
        functionMatch(name, body, tokens)

      case Css.Syntax.Sequence(terms) =>
        terms.fold(List(tokens): List[List[ValueToken]]): (states, term) =>
          states.bind(consume(term, _))

      case Css.Syntax.OneOf(options) =>
        options.bind(consume(_, tokens))

      case Css.Syntax.AllOf(terms) =>
        allOf(terms, tokens)

      case Css.Syntax.AnyOf(terms) =>
        anyOf(terms, tokens)

      case Css.Syntax.Repeated(term, min, max, separated) =>
        repeat(term, min, max, separated, tokens)

      case Css.Syntax.Mandatory(term) =>
        consume(term, tokens)

    private def pickEach(terms: List[Css.Syntax], tokens: List[ValueToken])
    :   List[(List[Css.Syntax], List[ValueToken])] =

      // A single documented stdlib view: picking each term in turn needs the list's length,
      // indexed access, and a positional removal, none of which the native `List` offers.
      val terms0 = terms.stdlib

      List.range(0, terms0.length).bind: index =>
        consume(terms0(index), tokens).map: rem =>
          (terms0.patch(index, Nil.stdlib, 1).to(List), rem)

    private def allOf(terms: List[Css.Syntax], tokens: List[ValueToken]): List[List[ValueToken]] =
      if terms.nil then List(tokens)
      else
        pickEach(terms, tokens).bind: (rest, rem) => allOf(rest, rem)

    private def anyOf(terms: List[Css.Syntax], tokens: List[ValueToken]): List[List[ValueToken]] =
      pickEach(terms, tokens).bind: (rest, rem) => (rem :: anyOf(rest, rem)): List[List[ValueToken]]

    private def repeat
      ( term: Css.Syntax, min: Int, max: Optional[Int], separated: Boolean, tokens: List[ValueToken] )
    :   List[List[ValueToken]] =

      def go(count: Int, toks: List[ValueToken]): List[List[ValueToken]] =
        val stop = if count >= min then List(toks) else Nil
        val more = max.lay(true)(count < _)

        if !more then stop
        else
          val starts = if count > 0 && separated then comma(toks) else List(toks)
          stop + starts.bind(consume(term, _)).bind(go(count + 1, _))

      go(0, tokens)

    private def comma(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Comma :: tail => List(tail)
      case _                        => Nil

    private def keyword(name: Text, tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Ident(value) :: tail if value.lower == name.lower =>
        List(tail)

      case _ =>
        Nil

    private def literal(token: Text, tokens: List[ValueToken]): List[List[ValueToken]] =
      if token == t"," then comma(tokens)
      else tokens match
        case ValueToken.Delim(char) :: tail if token == char.show =>
          List(tail)

        case _ =>
          Nil

    private def typeMatch(name: Text, tokens: List[ValueToken]): List[List[ValueToken]] =
      composite(name) match
        case syntax: Css.Syntax => guarded(name, consume(syntax, tokens))
        case _                  => primitive(name, tokens)

    private def propertyMatch(name: Text, tokens: List[ValueToken]): List[List[ValueToken]] =
      PropertyDef.of(name) match
        case property: PropertyDef =>
          guarded(t"<$name>", consume(property.grammar, tokens))

        case _ =>
          unsupported += name
          Nil

    // Resolve a named reference, guarding against cycles in recursive grammars.
    private inline def guarded(name: Text, inline block: => List[List[ValueToken]])
    :   List[List[ValueToken]] =

      if resolving.contains(name) then
        unsupported += name
        Nil
      else
        resolving += name
        val result = block
        resolving -= name
        result

    private def functionMatch(name: Text, body: Css.Syntax, tokens: List[ValueToken])
    :   List[List[ValueToken]] =

      tokens match
        case ValueToken.Function(fname) :: tail if fname.lower == name.lower =>
          val (inner, after) = split(tail)

          after.lay(Nil): rest => if consume(body, inner).exists(_.nil) then List(rest) else Nil

        case _ =>
          Nil

    // Split `tokens` at the `)` that closes the current function, returning the
    // tokens inside and (if balanced) those after the close.
    private def split(tokens: List[ValueToken]): (List[ValueToken], Optional[List[ValueToken]]) =
      def loop(depth: Int, acc: List[ValueToken], rest: List[ValueToken])
      :   (List[ValueToken], Optional[List[ValueToken]]) =

        rest match
          case Nil =>
            (acc.reverse, Unset)

          case ValueToken.Close :: tail if depth == 0 =>
            (acc.reverse, tail)

          case (token @ ValueToken.Close) :: tail =>
            loop(depth - 1, token :: acc, tail)

          case (token @ (ValueToken.Function(_) | ValueToken.Open)) :: tail =>
            loop(depth + 1, token :: acc, tail)

          case token :: tail =>
            loop(depth, token :: acc, tail)

      loop(0, Nil, tokens)

    private def afterFunction(tokens: List[ValueToken]): List[List[ValueToken]] =
      split(tokens)._2.lay(Nil)(List(_))

    // ── leaf primitives ──────────────────────────────────────────────────────

    private def primitive(name: Text, tokens: List[ValueToken]): List[List[ValueToken]] =
      name.lower match
        case t"length" =>
          numeric(tokens)(lengthLeaf)

        case t"percentage" =>
          numeric(tokens)(percentageLeaf)

        case t"number" | t"number-token" | t"integer" =>
          numeric(tokens)(numberLeaf)

        case t"angle" =>
          numeric(tokens)(unitLeaf(angleUnits))

        case t"time" =>
          numeric(tokens)(unitLeaf(timeUnits))

        case t"resolution" =>
          numeric(tokens)(unitLeaf(resolutionUnits))

        case t"frequency" =>
          numeric(tokens)(unitLeaf(frequencyUnits))

        case t"flex" =>
          unitLeaf(flexUnits)(tokens)

        case t"ratio" =>
          ratioLeaf(tokens)

        case t"declaration-value" | t"any-value" =>
          List(Nil)

        case t"dimension" | t"dimension-token" =>
          dimensionLeaf(tokens)

        case t"string" | t"string-token" =>
          stringLeaf(tokens)

        case t"url" | t"url-token" =>
          urlLeaf(tokens)

        case t"hex-color" | t"hash-token" =>
          hashLeaf(tokens)

        case t"custom-ident" | t"ident" | t"ident-token" | t"dashed-ident"
        | t"custom-property-name" =>
          identLeaf(tokens)

        case _ =>
          unsupported += name
          Nil

    // Accept a `calc()`/`min()`/… wherever a numeric primitive is expected;
    // otherwise fall back to the leaf matcher.
    private def numeric(tokens: List[ValueToken])(leaf: List[ValueToken] => List[List[ValueToken]])
    :   List[List[ValueToken]] =

      tokens match
        case ValueToken.Function(name) :: tail if mathFunctions(name.lower) =>
          afterFunction(tail)

        case _ =>
          leaf(tokens)

    private def lengthLeaf(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Dimension(_, unit, _) :: tail if lengthUnits(unit.lower) => List(tail)
      case ValueToken.Number(value, _, _) :: tail if value == 0.0              => List(tail)
      case _                                                                   => Nil

    private def percentageLeaf(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Percentage(_, _) :: tail => List(tail)
      case _                                   => Nil

    private def numberLeaf(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Number(_, _, _) :: tail => List(tail)
      case _                                  => Nil

    // A dimension whose unit is one of `units`: an angle, a time, a resolution, and so on.
    private def unitLeaf(units: Set[Text])(tokens: List[ValueToken]): List[List[ValueToken]] =
      tokens match
        case ValueToken.Dimension(_, unit, _) :: tail if units(unit.lower) => List(tail)
        case _                                                             => Nil

    // `<ratio> = <number> [ / <number> ]?`
    private def ratioLeaf(tokens: List[ValueToken]): List[List[ValueToken]] =
      numberLeaf(tokens).bind: rest =>
        rest match
          case ValueToken.Delim('/') :: tail => numberLeaf(tail)
          case _                             => List(rest)

    private def dimensionLeaf(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Dimension(_, _, _) :: tail => List(tail)
      case _                                     => Nil

    private def stringLeaf(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Quoted(_) :: tail => List(tail)
      case _                            => Nil

    private def identLeaf(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Ident(_) :: tail => List(tail)
      case _                           => Nil

    private def hashLeaf(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Hash(_) :: tail => List(tail)
      case _                          => Nil

    private def urlLeaf(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Url(_) :: tail =>
        List(tail)

      case ValueToken.Function(name) :: tail if name.lower == t"url".lower =>
        afterFunction(tail)

      case _ =>
        Nil
