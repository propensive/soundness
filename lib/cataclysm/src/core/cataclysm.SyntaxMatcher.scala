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

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import hellenism.*
import jacinta.*
import turbulence.*
import vacuous.*

import hellenism.classloaders.threadContextClassloader

// Checks a CSS property value against its `Syntax` grammar. Composite `<type>`s
// are resolved lazily from the bundled `syntaxes.json` (which expand to keywords,
// functions and a handful of leaf primitives); leaf primitives (`<length>`,
// `<color>` parts, …) are matched at the token level. A value containing an
// arbitrary-substitution function (`var()`, `env()`) is always valid (it is a
// pending-substitution value). `calc()` and friends are accepted wherever a
// numeric primitive is expected. Any `<type>` that is neither resolvable nor an
// implemented primitive yields `Outcome.Unsupported`.
object SyntaxMatcher:
  private object Entry:
    given decodable: Tactic[JsonError] => Entry is Json.Decodable = Json.DecodableDerivation.derived

  private case class Entry(syntax: Text)

  private lazy val rawComposites: Map[Text, Text] =
    import contingency.strategies.throwUnsafely

    val entries = cp"/cataclysm/syntaxes.json".read[Json].as[Map[Text, Entry]]
    entries.view.mapValues(_.syntax).toMap

  private val cache: scala.collection.mutable.HashMap[Text, Optional[Syntax]] =
    scala.collection.mutable.HashMap()

  private def composite(name: Text): Optional[Syntax] =
    cache.getOrElseUpdate(name, rawComposites.get(name).map(parsed).getOrElse(Unset))

  private def parsed(raw: Text): Optional[Syntax] = safely(SyntaxParser.parse(raw))

  private val lengthUnits: Set[String] =
    Set("px", "em", "rem", "ex", "ch", "cap", "ic", "lh", "rlh", "vw", "vh", "vi", "vb", "vmin",
        "vmax", "svw", "svh", "lvw", "lvh", "dvw", "dvh", "cm", "mm", "q", "in", "pt", "pc")

  private val angleUnits: Set[String] = Set("deg", "grad", "rad", "turn")
  private val timeUnits: Set[String] = Set("s", "ms")
  private val resolutionUnits: Set[String] = Set("dpi", "dpcm", "dppx", "x")
  private val frequencyUnits: Set[String] = Set("hz", "khz")
  private val flexUnits: Set[String] = Set("fr")
  private val substitutions: Set[String] = Set("var", "env")

  // The CSS-wide keywords are valid as the sole value of every property, but appear
  // in no property's grammar, so they are accepted before grammar matching.
  private val globalKeywords: Set[String] = Set("inherit", "initial", "unset", "revert",
      "revert-layer")

  private val mathFunctions: Set[String] =
    Set("calc", "min", "max", "clamp", "sin", "cos", "tan", "asin", "acos", "atan", "atan2", "pow",
        "sqrt", "hypot", "log", "exp", "abs", "sign", "mod", "rem", "round")

  private def lower(text: Text): String = text.s.toLowerCase.nn
  private def same(a: Text, b: Text): Boolean = lower(a) == lower(b)

  private def substitution(token: ValueToken): Boolean = token match
    case ValueToken.Function(name) => substitutions(lower(name))
    case _                         => false

  private def globalKeyword(tokens: List[ValueToken]): Boolean = tokens match
    case List(ValueToken.Ident(value)) => globalKeywords(lower(value))
    case _                             => false

  def check(property: PropertyDef, value: Text)(using Tactic[CssError]): Outcome =
    check(property.grammar, value)

  def check(grammar: Syntax, value: Text)(using Tactic[CssError]): Outcome =
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

    def consume(syntax: Syntax, tokens: List[ValueToken]): List[List[ValueToken]] = syntax match
      case Syntax.Keyword(name) =>
        keyword(name, tokens)

      case Syntax.Literal(token) =>
        literal(token, tokens)

      case Syntax.Type(name, _) =>
        typeMatch(name, tokens)

      case Syntax.Property(name) =>
        propertyMatch(name, tokens)

      case Syntax.Function(name, body) =>
        functionMatch(name, body, tokens)

      case Syntax.Sequence(terms) =>
        terms.foldLeft(List(tokens)): (states, term) =>
          states.flatMap(consume(term, _))

      case Syntax.OneOf(options) =>
        options.flatMap(consume(_, tokens))

      case Syntax.AllOf(terms) =>
        allOf(terms, tokens)

      case Syntax.AnyOf(terms) =>
        anyOf(terms, tokens)

      case Syntax.Repeated(term, min, max, separated) =>
        repeat(term, min, max, separated, tokens)

      case Syntax.Mandatory(term) =>
        consume(term, tokens)

    private def pickEach(terms: List[Syntax], tokens: List[ValueToken])
    :   List[(List[Syntax], List[ValueToken])] =

      terms.indices.to(List).flatMap: index =>
        consume(terms(index), tokens).map: rem =>
          (terms.patch(index, Nil, 1), rem)

    private def allOf(terms: List[Syntax], tokens: List[ValueToken]): List[List[ValueToken]] =
      if terms.nil then List(tokens)
      else
        pickEach(terms, tokens).flatMap: (rest, rem) =>
          allOf(rest, rem)

    private def anyOf(terms: List[Syntax], tokens: List[ValueToken]): List[List[ValueToken]] =
      pickEach(terms, tokens).flatMap: (rest, rem) =>
        rem :: anyOf(rest, rem)

    private def repeat
      ( term: Syntax, min: Int, max: Optional[Int], separated: Boolean, tokens: List[ValueToken] )
    :   List[List[ValueToken]] =

      def go(count: Int, toks: List[ValueToken]): List[List[ValueToken]] =
        val stop = if count >= min then List(toks) else Nil
        val more = max.lay(true)(count < _)

        if !more then stop
        else
          val starts = if count > 0 && separated then comma(toks) else List(toks)
          stop ::: starts.flatMap(consume(term, _)).flatMap(go(count + 1, _))

      go(0, tokens)

    private def comma(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Comma :: tail => List(tail)
      case _                        => Nil

    private def keyword(name: Text, tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Ident(value) :: tail if same(value, name) =>
        List(tail)

      case _ =>
        Nil

    private def literal(token: Text, tokens: List[ValueToken]): List[List[ValueToken]] =
      if token == t"," then comma(tokens)
      else tokens match
        case ValueToken.Delim(char) :: tail if token == char.toString.tt =>
          List(tail)

        case _ =>
          Nil

    private def typeMatch(name: Text, tokens: List[ValueToken]): List[List[ValueToken]] =
      composite(name) match
        case syntax: Syntax => guarded(name, consume(syntax, tokens))
        case _              => primitive(name, tokens)

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

    private def functionMatch(name: Text, body: Syntax, tokens: List[ValueToken])
    :   List[List[ValueToken]] =

      tokens match
        case ValueToken.Function(fname) :: tail if same(fname, name) =>
          val (inner, after) = split(tail)

          after.lay(Nil): rest =>
            if consume(body, inner).exists(_.nil) then List(rest) else Nil

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
      lower(name) match
        case "length" =>
          numeric(tokens)(lengthLeaf)

        case "percentage" =>
          numeric(tokens)(percentageLeaf)

        case "number" | "number-token" | "integer" =>
          numeric(tokens)(numberLeaf)

        case "angle" =>
          numeric(tokens)(angleLeaf)

        case "time" =>
          numeric(tokens)(timeLeaf)

        case "resolution" =>
          numeric(tokens)(resolutionLeaf)

        case "frequency" =>
          numeric(tokens)(frequencyLeaf)

        case "flex" =>
          flexLeaf(tokens)

        case "ratio" =>
          ratioLeaf(tokens)

        case "declaration-value" | "any-value" =>
          List(Nil)

        case "dimension" | "dimension-token" =>
          dimensionLeaf(tokens)

        case "string" | "string-token" =>
          stringLeaf(tokens)

        case "url" | "url-token" =>
          urlLeaf(tokens)

        case "hex-color" | "hash-token" =>
          hashLeaf(tokens)

        case "custom-ident" | "ident" | "ident-token" | "dashed-ident" | "custom-property-name" =>
          identLeaf(tokens)

        case _ =>
          unsupported += name
          Nil

    // Accept a `calc()`/`min()`/… wherever a numeric primitive is expected;
    // otherwise fall back to the leaf matcher.
    private def numeric(tokens: List[ValueToken])(leaf: List[ValueToken] => List[List[ValueToken]])
    :   List[List[ValueToken]] =

      tokens match
        case ValueToken.Function(name) :: tail if mathFunctions(lower(name)) =>
          afterFunction(tail)

        case _ =>
          leaf(tokens)

    private def lengthLeaf(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Dimension(_, unit, _) :: tail if lengthUnits(lower(unit)) => List(tail)
      case ValueToken.Number(value, _, _) :: tail if value == 0.0               => List(tail)
      case _                                                                    => Nil

    private def percentageLeaf(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Percentage(_, _) :: tail => List(tail)
      case _                                   => Nil

    private def numberLeaf(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Number(_, _, _) :: tail => List(tail)
      case _                                  => Nil

    private def angleLeaf(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Dimension(_, unit, _) :: tail if angleUnits(lower(unit)) => List(tail)
      case _                                                                   => Nil

    private def timeLeaf(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Dimension(_, unit, _) :: tail if timeUnits(lower(unit)) => List(tail)
      case _                                                                  => Nil

    private def resolutionLeaf(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Dimension(_, unit, _) :: tail if resolutionUnits(lower(unit)) =>
        List(tail)

      case _ =>
        Nil

    private def frequencyLeaf(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Dimension(_, unit, _) :: tail if frequencyUnits(lower(unit)) =>
        List(tail)

      case _ =>
        Nil

    private def flexLeaf(tokens: List[ValueToken]): List[List[ValueToken]] = tokens match
      case ValueToken.Dimension(_, unit, _) :: tail if flexUnits(lower(unit)) => List(tail)
      case _                                                                  => Nil

    // `<ratio> = <number> [ / <number> ]?`
    private def ratioLeaf(tokens: List[ValueToken]): List[List[ValueToken]] =
      numberLeaf(tokens).flatMap: rest =>
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

      case ValueToken.Function(name) :: tail if same(name, t"url") =>
        afterFunction(tail)

      case _ =>
        Nil
