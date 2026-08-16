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
package xylophone

import proscenium.compat.*

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import vacuous.*
import zephyrine.*

// A hand-written lexer and recursive-descent parser for the full XPath 1.0
// grammar (one method per production of the W3C recommendation), reporting
// every failure as a `Parse.Error` over the `XPath` format, carrying the
// character offset at which it was detected, which the `xp"…"` interpolator
// maps back onto a source-file caret. With `holes` enabled, a NUL (`\u0000`) marker (as inserted between the
// parts of an interpolated literal) lexes as a numbered hole and parses as an
// `Expression.Substitution` wherever a primary expression is permitted.
private[xylophone] object XPathReader:
  import XPath.{Axis, Expression, NodeTest, Origin, Step}

  private enum Token derives CanEqual:
    case Slash, DoubleSlash, Pipe, Plus, Minus, Equals, Unequals, Less, LessOrEqual, Greater,
         GreaterOrEqual, Star, OrKeyword, AndKeyword, DivKeyword, ModKeyword,
         At, OpenParen, CloseParen, OpenBracket, CloseBracket, Comma, Dot, DotDot
    case NameToken(prefix: Optional[Text], local: Text)
    case WildcardTest
    case PrefixWildcardTest(prefix: Text)
    case NodeTypeToken(name: Text)
    case AxisToken(axis: Axis)
    case FunctionToken(prefix: Optional[Text], local: Text)
    case LiteralToken(text: Text)
    case NumberToken(value: Double)
    case VariableToken(prefix: Optional[Text], name: Text)
    case HoleToken(index: Int)

  private case class Positioned(token: Token, offset: Int)

  // The disambiguation rule of §3.7: `*` is the multiplication operator, and
  // `and`/`or`/`div`/`mod` are operator names, exactly when the preceding
  // token could end an operand — i.e. it is not `@`, `::`, `(`, `[`, `,` or
  // an operator. (`::` never survives as a token here: it is folded into
  // `AxisToken`, which likewise cannot end an operand.)
  private def operand(token: Token): Boolean = token match
    case Token.NameToken(_, _) | Token.WildcardTest | Token.PrefixWildcardTest(_)
       | Token.LiteralToken(_) | Token.NumberToken(_) | Token.VariableToken(_, _)
       | Token.CloseParen | Token.CloseBracket | Token.Dot | Token.DotDot
       | Token.HoleToken(_) =>
      true

    case _ =>
      false

  private def nameStart(char: Char): Boolean = Character.isLetter(char) || char == '_'

  private def namePart(char: Char): Boolean =
    Character.isLetterOrDigit(char) || char == '_' || char == '-' || char == '.'

  private def axisFor(name: String): Optional[Axis] = name match
    case "ancestor"           => Axis.Ancestor
    case "ancestor-or-self"   => Axis.AncestorOrSelf
    case "attribute"          => Axis.Attribute
    case "child"              => Axis.Child
    case "descendant"         => Axis.Descendant
    case "descendant-or-self" => Axis.DescendantOrSelf
    case "following"          => Axis.Following
    case "following-sibling"  => Axis.FollowingSibling
    case "namespace"          => Axis.Namespace
    case "parent"             => Axis.Parent
    case "preceding"          => Axis.Preceding
    case "preceding-sibling"  => Axis.PrecedingSibling
    case "self"               => Axis.Self
    case _                    => Unset

  private def tokenize(string: String, holes: Boolean)(using Tactic[Parse.Error])
  :   scm.ArrayBuffer[Positioned] =

    val tokens = scm.ArrayBuffer[Positioned]()
    var offset = 0
    var holeCount = 0
    val length = string.length

    def push(token: Token, at: Int): Unit = tokens += Positioned(token, at)
    def afterOperand: Boolean = tokens.nonEmpty && operand(tokens.last.token)
    def digit(at: Int): Boolean = at < length && string.charAt(at) >= '0' && string.charAt(at) <= '9'

    def scanNumber(start: Int): Unit =
      var end = start
      while digit(end) do end += 1
      if end < length && string.charAt(end) == '.' then
        end += 1
        while digit(end) do end += 1

      push(Token.NumberToken(java.lang.Double.parseDouble(string.substring(start, end).nn)), start)
      offset = end

    def scanLiteral(start: Int): Unit =
      val quote = string.charAt(start)
      val close = string.indexOf(quote, start + 1)
      if close < 0 then abort(Parse.Error(XPath, XPath.Position(start), XPath.Issue.UnterminatedLiteral))
      push(Token.LiteralToken(string.substring(start + 1, close).nn.tt), start)
      offset = close + 1

    def scanNcname(start: Int): Int =
      var end = start + 1
      while end < length && namePart(string.charAt(end)) do end += 1
      end

    def skipSpace(at: Int): Int =
      var index = at
      while index < length
            && (string.charAt(index) == ' ' || string.charAt(index) == '\t'
                || string.charAt(index) == '\r' || string.charAt(index) == '\n')
      do index += 1
      index

    def scanName(start: Int): Unit =
      var end = scanNcname(start)
      val first = string.substring(start, end).nn

      if afterOperand then
        first match
          case "and" => push(Token.AndKeyword, start)
          case "or"  => push(Token.OrKeyword, start)
          case "div" => push(Token.DivKeyword, start)
          case "mod" => push(Token.ModKeyword, start)
          case _     => abort(Parse.Error(XPath, XPath.Position(start), XPath.Issue.UnexpectedToken))

        offset = end
      else
        var prefix: Optional[Text] = Unset
        var local = first

        if end < length && string.charAt(end) == ':' && end + 1 < length
           && string.charAt(end + 1) != ':'
        then
          if string.charAt(end + 1) == '*' then
            push(Token.PrefixWildcardTest(first.tt), start)
            offset = end + 2
            return
          else if nameStart(string.charAt(end + 1)) then
            val localEnd = scanNcname(end + 1)
            prefix = first.tt
            local = string.substring(end + 1, localEnd).nn
            end = localEnd
          else
            abort(Parse.Error(XPath, XPath.Position(end), XPath.Issue.UnexpectedCharacter))

        val ahead = skipSpace(end)

        if ahead + 1 < length && string.charAt(ahead) == ':' && string.charAt(ahead + 1) == ':'
        then
          if prefix != Unset then abort(Parse.Error(XPath, XPath.Position(start), XPath.Issue.UnknownAxis))

          axisFor(local) match
            case axis: Axis => push(Token.AxisToken(axis), start)
            case _          => abort(Parse.Error(XPath, XPath.Position(start), XPath.Issue.UnknownAxis))

          offset = ahead + 2
        else if ahead < length && string.charAt(ahead) == '(' then
          if prefix == Unset
             && (local == "node" || local == "text" || local == "comment"
                 || local == "processing-instruction")
          then push(Token.NodeTypeToken(local.tt), start)
          else push(Token.FunctionToken(prefix, local.tt), start)

          offset = end
        else
          push(Token.NameToken(prefix, local.tt), start)
          offset = end

    while offset < length do
      val start = offset
      val char = string.charAt(offset)

      char match
        case ' ' | '\t' | '\r' | '\n' =>
          offset += 1

        case '/' =>
          if offset + 1 < length && string.charAt(offset + 1) == '/' then
            push(Token.DoubleSlash, start)
            offset += 2
          else
            push(Token.Slash, start)
            offset += 1

        case '|' => push(Token.Pipe, start); offset += 1
        case '+' => push(Token.Plus, start); offset += 1
        case '-' => push(Token.Minus, start); offset += 1
        case '=' => push(Token.Equals, start); offset += 1
        case '(' => push(Token.OpenParen, start); offset += 1
        case ')' => push(Token.CloseParen, start); offset += 1
        case '[' => push(Token.OpenBracket, start); offset += 1
        case ']' => push(Token.CloseBracket, start); offset += 1
        case ',' => push(Token.Comma, start); offset += 1
        case '@' => push(Token.At, start); offset += 1

        case '!' =>
          if offset + 1 < length && string.charAt(offset + 1) == '=' then
            push(Token.Unequals, start)
            offset += 2
          else
            abort(Parse.Error(XPath, XPath.Position(start), XPath.Issue.UnexpectedCharacter))

        case '<' =>
          if offset + 1 < length && string.charAt(offset + 1) == '=' then
            push(Token.LessOrEqual, start)
            offset += 2
          else
            push(Token.Less, start)
            offset += 1

        case '>' =>
          if offset + 1 < length && string.charAt(offset + 1) == '=' then
            push(Token.GreaterOrEqual, start)
            offset += 2
          else
            push(Token.Greater, start)
            offset += 1

        case '*' =>
          push(if afterOperand then Token.Star else Token.WildcardTest, start)
          offset += 1

        case '.' =>
          if digit(offset + 1) then scanNumber(start)
          else if offset + 1 < length && string.charAt(offset + 1) == '.' then
            push(Token.DotDot, start)
            offset += 2
          else
            push(Token.Dot, start)
            offset += 1

        case '\'' | '"' =>
          scanLiteral(start)

        case '$' =>
          if offset + 1 < length && nameStart(string.charAt(offset + 1)) then
            var end = scanNcname(offset + 1)
            val first = string.substring(offset + 1, end).nn

            if end + 1 < length && string.charAt(end) == ':' && nameStart(string.charAt(end + 1))
            then
              val localEnd = scanNcname(end + 1)
              push
                ( Token.VariableToken(first.tt, string.substring(end + 1, localEnd).nn.tt),
                  start )
              offset = localEnd
            else
              push(Token.VariableToken(Unset, first.tt), start)
              offset = end
          else
            abort(Parse.Error(XPath, XPath.Position(start), XPath.Issue.UnexpectedCharacter))

        case '\u0000' if holes =>
          push(Token.HoleToken(holeCount), start)
          holeCount += 1
          offset += 1

        case other =>
          if other >= '0' && other <= '9' then scanNumber(start)
          else if nameStart(other) then scanName(start)
          else abort(Parse.Error(XPath, XPath.Position(start), XPath.Issue.UnexpectedCharacter))

    tokens

  private val descendantStep: Step = Step(Axis.DescendantOrSelf, NodeTest.Node, Nil)

  def parse(text: Text, holes: Boolean)(using Tactic[Parse.Error]): Expression =
    val tokens = tokenize(text.s, holes)
    val end = text.s.length
    var index = 0

    def more: Boolean = index < tokens.length
    def current: Token = tokens(index).token
    def here: Int = if more then tokens(index).offset else end
    def advance(): Unit = index += 1

    def expect(token: Token, issue: XPath.Issue): Unit =
      if more && current == token then advance()
      else abort(Parse.Error(XPath, XPath.Position(here), issue))

    def parseAll(): Expression =
      val result = parseOr()
      if more then abort(Parse.Error(XPath, XPath.Position(here), XPath.Issue.UnexpectedToken))
      result

    def parseOr(): Expression =
      var left = parseAnd()

      while more && current == Token.OrKeyword do
        advance()
        left = Expression.Or(left, parseAnd())

      left

    def parseAnd(): Expression =
      var left = parseEquality()

      while more && current == Token.AndKeyword do
        advance()
        left = Expression.And(left, parseEquality())

      left

    def parseEquality(): Expression =
      var left = parseRelational()

      while more && (current == Token.Equals || current == Token.Unequals) do
        val equal = current == Token.Equals
        advance()
        val right = parseRelational()
        left = if equal then Expression.Equal(left, right) else Expression.Unequal(left, right)

      left

    def parseRelational(): Expression =
      var left = parseAdditive()

      while more
            && (current == Token.Less || current == Token.LessOrEqual
                || current == Token.Greater || current == Token.GreaterOrEqual)
      do
        val operator = current
        advance()
        val right = parseAdditive()

        left = operator match
          case Token.Less           => Expression.Less(left, right)
          case Token.LessOrEqual    => Expression.LessOrEqual(left, right)
          case Token.Greater        => Expression.Greater(left, right)
          case _                    => Expression.GreaterOrEqual(left, right)

      left

    def parseAdditive(): Expression =
      var left = parseMultiplicative()

      while more && (current == Token.Plus || current == Token.Minus) do
        val plus = current == Token.Plus
        advance()
        val right = parseMultiplicative()
        left = if plus then Expression.Add(left, right) else Expression.Subtract(left, right)

      left

    def parseMultiplicative(): Expression =
      var left = parseUnary()

      while more
            && (current == Token.Star || current == Token.DivKeyword
                || current == Token.ModKeyword)
      do
        val operator = current
        advance()
        val right = parseUnary()

        left = operator match
          case Token.Star       => Expression.Multiply(left, right)
          case Token.DivKeyword => Expression.Divide(left, right)
          case _                => Expression.Modulo(left, right)

      left

    def parseUnary(): Expression =
      if more && current == Token.Minus then
        advance()
        Expression.Negate(parseUnary())
      else
        parseUnion()

    def parseUnion(): Expression =
      var left = parsePath()

      while more && current == Token.Pipe do
        advance()
        left = Expression.Union(left, parsePath())

      left

    def startsStep(token: Token): Boolean = token match
      case Token.Dot | Token.DotDot | Token.At | Token.WildcardTest
         | Token.NameToken(_, _) | Token.PrefixWildcardTest(_) | Token.NodeTypeToken(_)
         | Token.AxisToken(_) =>
        true

      case _ =>
        false

    def parsePath(): Expression =
      if !more then abort(Parse.Error(XPath, XPath.Position(here), XPath.Issue.ExpectedExpression))

      current match
        case Token.Slash =>
          advance()

          if more && startsStep(current)
          then Expression.Route(Origin.Root, parseRelative())
          else Expression.Route(Origin.Root, Nil)

        case Token.DoubleSlash =>
          advance()
          Expression.Route(Origin.Root, List.of(descendantStep :: parseRelative().stdlib))

        case token if startsStep(token) =>
          Expression.Route(Origin.Here, parseRelative())

        case _ =>
          parseFilter()

    def parseRelative(): List[Step] =
      val steps = scm.ListBuffer[Step]()
      steps += parseStep()

      while more && (current == Token.Slash || current == Token.DoubleSlash) do
        if current == Token.DoubleSlash then steps += descendantStep
        advance()
        steps += parseStep()

      List.of(steps.toList)

    def parseStep(): Step =
      if !more then abort(Parse.Error(XPath, XPath.Position(here), XPath.Issue.ExpectedNodeTest))

      current match
        case Token.Dot =>
          advance()
          Step(Axis.Self, NodeTest.Node, Nil)

        case Token.DotDot =>
          advance()
          Step(Axis.Parent, NodeTest.Node, Nil)

        case Token.At =>
          advance()
          finishStep(Axis.Attribute)

        case Token.AxisToken(axis) =>
          advance()
          finishStep(axis)

        case _ =>
          finishStep(Axis.Child)

    def finishStep(axis: Axis): Step =
      val test = parseNodeTest()
      Step(axis, test, parsePredicates())

    def parseNodeTest(): NodeTest =
      if !more then abort(Parse.Error(XPath, XPath.Position(here), XPath.Issue.ExpectedNodeTest))

      current match
        case Token.NameToken(prefix, local) =>
          advance()
          NodeTest.Name(prefix, local)

        case Token.WildcardTest =>
          advance()
          NodeTest.Wildcard

        case Token.PrefixWildcardTest(prefix) =>
          advance()
          NodeTest.PrefixWildcard(prefix)

        case Token.NodeTypeToken(name) =>
          advance()
          expect(Token.OpenParen, XPath.Issue.UnexpectedToken)

          name.s match
            case "processing-instruction" =>
              val target: Optional[Text] =
                if more then current match
                  case Token.LiteralToken(text) =>
                    advance()
                    text

                  case _ =>
                    Unset
                else Unset

              expect(Token.CloseParen, XPath.Issue.ExpectedCloseParen)
              NodeTest.Instruction(target)

            case "node" =>
              expect(Token.CloseParen, XPath.Issue.ExpectedCloseParen)
              NodeTest.Node

            case "text" =>
              expect(Token.CloseParen, XPath.Issue.ExpectedCloseParen)
              NodeTest.Textual

            case _ =>
              expect(Token.CloseParen, XPath.Issue.ExpectedCloseParen)
              NodeTest.Comment

        case _ =>
          abort(Parse.Error(XPath, XPath.Position(here), XPath.Issue.ExpectedNodeTest))

    def parsePredicates(): List[Expression] =
      val predicates = scm.ListBuffer[Expression]()

      while more && current == Token.OpenBracket do
        advance()
        predicates += parseOr()
        expect(Token.CloseBracket, XPath.Issue.ExpectedCloseBracket)

      List.of(predicates.toList)

    def parseFilter(): Expression =
      val primary = parsePrimary()
      val predicates = parsePredicates()

      if more && (current == Token.Slash || current == Token.DoubleSlash) then
        val descend = current == Token.DoubleSlash
        advance()
        val rest = parseRelative()
        val steps = if descend then List.of(descendantStep :: rest.stdlib) else rest
        Expression.Route(Origin.Filter(primary, predicates), steps)
      else if predicates.isEmpty then primary
      else Expression.Route(Origin.Filter(primary, predicates), Nil)

    def parsePrimary(): Expression =
      if !more then abort(Parse.Error(XPath, XPath.Position(here), XPath.Issue.ExpectedExpression))

      current match
        case Token.VariableToken(prefix, name) =>
          advance()
          Expression.Variable(prefix, name)

        case Token.LiteralToken(text) =>
          advance()
          Expression.Literal(text)

        case Token.NumberToken(value) =>
          advance()
          Expression.Number(value)

        case Token.HoleToken(index) =>
          advance()
          Expression.Substitution(index)

        case Token.OpenParen =>
          advance()
          val expression = parseOr()
          expect(Token.CloseParen, XPath.Issue.ExpectedCloseParen)
          expression

        case Token.FunctionToken(prefix, local) =>
          advance()
          expect(Token.OpenParen, XPath.Issue.ExpectedExpression)

          if more && current == Token.CloseParen then
            advance()
            Expression.Call(prefix, local, Nil)
          else
            val arguments = scm.ListBuffer[Expression]()
            arguments += parseOr()

            while more && current == Token.Comma do
              advance()
              arguments += parseOr()

            expect(Token.CloseParen, XPath.Issue.ExpectedCloseParen)
            Expression.Call(prefix, local, List.of(arguments.toList))

        case _ =>
          abort(Parse.Error(XPath, XPath.Position(here), XPath.Issue.ExpectedExpression))

    parseAll()
