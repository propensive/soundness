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

import anticipation.*
import contextual.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import murmuration.*
import prepositional.*
import vacuous.*

object XPath:
  // The thirteen XPath 1.0 axes (§2.2). `keyword` is the spelling used in the
  // unabbreviated `axis::test` syntax.
  enum Axis(val keyword: Text) derives CanEqual:
    case Ancestor          extends Axis(t"ancestor")
    case AncestorOrSelf    extends Axis(t"ancestor-or-self")
    case Attribute         extends Axis(t"attribute")
    case Child             extends Axis(t"child")
    case Descendant        extends Axis(t"descendant")
    case DescendantOrSelf  extends Axis(t"descendant-or-self")
    case Following         extends Axis(t"following")
    case FollowingSibling  extends Axis(t"following-sibling")
    case Namespace         extends Axis(t"namespace")
    case Parent            extends Axis(t"parent")
    case Preceding         extends Axis(t"preceding")
    case PrecedingSibling  extends Axis(t"preceding-sibling")
    case Self              extends Axis(t"self")

  // A node test (§2.3). `Textual` rather than `Text`, which would shadow
  // gossamer's `Text` throughout this file.
  enum NodeTest derives CanEqual:
    case Name(prefix: Optional[Text], local: Text)
    case Wildcard                                    // *
    case PrefixWildcard(prefix: Text)                // prefix:*
    case Node                                        // node()
    case Textual                                     // text()
    case Comment                                     // comment()
    case Instruction(target: Optional[Text])         // processing-instruction('target')

  // One location step. Steps are stored root-first, in source order. The
  // abbreviations `//`, `.`, `..` and `@` are desugared by the parser
  // (§2.5): `//` becomes a `descendant-or-self::node()` step, `.` is
  // `self::node()`, `..` is `parent::node()` and `@name` is
  // `attribute::name`; the encoder re-sugars them.
  case class Step(axis: Axis, test: NodeTest, predicates: List[Expression] = Nil)
  derives CanEqual

  // Where a path starts: the document root (absolute paths), the context node
  // (relative paths), or a filter expression (`(//a)[1]/b`).
  enum Origin derives CanEqual:
    case Root
    case Here
    case Filter(expression: Expression, predicates: List[Expression])

  // The XPath 1.0 expression language (§3). `Route` is any location path or
  // path expression; `Substitution` marks an `xp"…"` interpolator insertion
  // and never appears in a decoded or evaluated expression.
  enum Expression derives CanEqual:
    case Or(left: Expression, right: Expression)
    case And(left: Expression, right: Expression)
    case Equal(left: Expression, right: Expression)
    case Unequal(left: Expression, right: Expression)
    case Less(left: Expression, right: Expression)
    case LessOrEqual(left: Expression, right: Expression)
    case Greater(left: Expression, right: Expression)
    case GreaterOrEqual(left: Expression, right: Expression)
    case Add(left: Expression, right: Expression)
    case Subtract(left: Expression, right: Expression)
    case Multiply(left: Expression, right: Expression)
    case Divide(left: Expression, right: Expression)
    case Modulo(left: Expression, right: Expression)
    case Negate(operand: Expression)
    case Union(left: Expression, right: Expression)
    case Literal(text: Text)
    case Number(value: Double)
    case Variable(prefix: Optional[Text], name: Text)
    case Call(prefix: Optional[Text], name: Text, arguments: List[Expression])
    case Route(origin: Origin, steps: List[Step])
    case Substitution(index: Int)

  // The simple positional view of a path: what `Xml`'s position `Locator` can
  // resolve against a `PositionIndex`, and what the Wisteria-derived decoders
  // produce for diagnostics.
  enum Location derives CanEqual:
    case Element(name: Text, rank: Int)
    case Attribute(name: Text)

  private def qualify(prefix: Optional[Text], local: Text): Text =
    prefix.let { prefix => t"$prefix:$local" }.or(local)

  // Operator precedence levels, loosest-first, following the grammar's
  // production nesting (§3.1-§3.5): or=1, and=2, equality=3, relational=4,
  // additive=5, multiplicative=6, unary=7, union=8; paths and primaries are
  // atomic.
  private def render(expression: Expression, minimum: Int): Text =
    def binary(operator: Text, left: Expression, right: Expression, level: Int): Text =
      val text = t"${render(left, level)}$operator${render(right, level + 1)}"
      if level < minimum then t"($text)" else text

    expression match
      case Expression.Or(left, right)             => binary(t" or ", left, right, 1)
      case Expression.And(left, right)            => binary(t" and ", left, right, 2)
      case Expression.Equal(left, right)          => binary(t"=", left, right, 3)
      case Expression.Unequal(left, right)        => binary(t"!=", left, right, 3)
      case Expression.Less(left, right)           => binary(t"<", left, right, 4)
      case Expression.LessOrEqual(left, right)    => binary(t"<=", left, right, 4)
      case Expression.Greater(left, right)        => binary(t">", left, right, 4)
      case Expression.GreaterOrEqual(left, right) => binary(t">=", left, right, 4)
      case Expression.Add(left, right)            => binary(t" + ", left, right, 5)
      case Expression.Subtract(left, right)       => binary(t" - ", left, right, 5)
      case Expression.Multiply(left, right)       => binary(t" * ", left, right, 6)
      case Expression.Divide(left, right)         => binary(t" div ", left, right, 6)
      case Expression.Modulo(left, right)         => binary(t" mod ", left, right, 6)

      case Expression.Negate(operand) =>
        val text = t"-${render(operand, 7)}"
        if minimum > 7 then t"($text)" else text

      case Expression.Union(left, right)          => binary(t"|", left, right, 8)
      case Expression.Literal(text)               => renderLiteral(text)
      case Expression.Number(value)               => renderNumber(value)
      case Expression.Variable(prefix, name)      => t"$$${qualify(prefix, name)}"

      case Expression.Call(prefix, name, arguments) =>
        t"${qualify(prefix, name)}(${arguments.map(render(_, 1)).join(t",")})"

      case Expression.Route(origin, steps)        => renderRoute(origin, steps)
      case Expression.Substitution(index)         => t"«$index»"

  // XPath 1.0 number syntax has no exponent, and the canonical form of an
  // integral value has no decimal point (`string(1.0)` is `1`).
  private def renderNumber(value: Double): Text =
    if value != value then t"NaN"
    else if java.lang.Double.isInfinite(value) then (if value > 0 then t"Infinity" else t"-Infinity")
    else if value == Math.floor(value) && Math.abs(value) < 1e15 then value.toLong.toString.tt
    else value.toString.tt

  // XPath 1.0 literals have no escape mechanism: a literal containing one
  // quote kind is rendered with the other, and a literal containing both is
  // unrepresentable directly, so it decomposes into a `concat(…)` call over
  // single-quoted pieces joined by double-quoted apostrophes.
  private def renderLiteral(text: Text): Text =
    if text.s.indexOf('\'') < 0 then t"'$text'"
    else if text.s.indexOf('"') < 0 then t"\"$text\""
    else
      val pieces = text.cut(t"'").map { (piece: Text) => t"'$piece'" }
      t"concat(${pieces.join(t",\"'\",")})"

  private def renderTest(test: NodeTest): Text = test match
    case NodeTest.Name(prefix, local)    => qualify(prefix, local)
    case NodeTest.Wildcard               => t"*"
    case NodeTest.PrefixWildcard(prefix) => t"$prefix:*"
    case NodeTest.Node                   => t"node()"
    case NodeTest.Textual                => t"text()"
    case NodeTest.Comment                => t"comment()"

    case NodeTest.Instruction(target) =>
      target.let { target => t"processing-instruction('$target')" }
      . or(t"processing-instruction()")

  private def renderPredicates(predicates: List[Expression]): Text =
    predicates.map { predicate => t"[${render(predicate, 1)}]" }.join

  // A `descendant-or-self::node()` step with no predicates is the desugaring
  // of `//`: it renders as an empty segment, so that joining segments with
  // `/` produces the abbreviated form. As the final step of a path the
  // abbreviation is unavailable (`a//` is not valid syntax), so it renders in
  // full.
  private def renderStep(step: Step, last: Boolean): Text = step match
    case Step(Axis.DescendantOrSelf, NodeTest.Node, Nil) =>
      if last then t"descendant-or-self::node()" else t""

    case Step(Axis.Self, NodeTest.Node, Nil)   => t"."
    case Step(Axis.Parent, NodeTest.Node, Nil) => t".."

    case Step(Axis.Child, test, predicates) =>
      t"${renderTest(test)}${renderPredicates(predicates)}"

    case Step(Axis.Attribute, test, predicates) =>
      t"@${renderTest(test)}${renderPredicates(predicates)}"

    case Step(axis, test, predicates) =>
      t"${axis.keyword}::${renderTest(test)}${renderPredicates(predicates)}"

  private def renderSteps(steps: List[Step]): Text =
    val length = steps.length
    steps.zipWithIndex.map { (step, index) => renderStep(step, index == length - 1) }.join(t"/")

  private def renderRoute(origin: Origin, steps: List[Step]): Text = origin match
    case Origin.Root => steps match
      case Nil => t"/"
      case _   => t"/${renderSteps(steps)}"

    case Origin.Here => steps match
      case Nil => t"."
      case _   => renderSteps(steps)

    case Origin.Filter(expression, predicates) =>
      // A filter expression's head must be a `PrimaryExpr`: variable
      // references, literals, numbers and function calls stand alone;
      // anything else is parenthesised.
      val head = expression match
        case Expression.Variable(_, _) | Expression.Literal(_) | Expression.Call(_, _, _) =>
          render(expression, 1)

        case Expression.Number(value) if value >= 0 => render(expression, 1)
        case other                                  => t"(${render(other, 1)})"

      val trail = steps match
        case Nil => t""
        case _   => t"/${renderSteps(steps)}"

      t"$head${renderPredicates(predicates)}$trail"

  given XPath is Encodable in Text = xpath => render(xpath.expression, 1)

  inline given interpolable: XPath is Interpolable:
    transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
      ( inline insertions: Any* )
    :   XPath =

      ${xylophone.internal.xpath[parts, origins]('insertions)}

  // Parses any XPath 1.0 expression — location paths, absolute or relative,
  // and the full expression language — reporting the offset of any error.
  given decodable: (tactic: Tactic[XPath.Error]) => ((XPath is Decodable in Text)^{tactic}) = text =>
    XPath(XPathReader.parse(text, holes = false))

  // XPathError → XPath.Error
  object Error:
    // Reasons are append-only: `number` values are stable identifiers within
    // the error 562 envelope and must never be reassigned. `ExpectedSlash`
    // and `BadStep` were raised by the pre-AST positional parser and are
    // retained only for numbering.
    enum Reason(val number: Int) extends Clarification:
      case ExpectedSlash        extends Reason(1)
      case BadStep              extends Reason(2)
      case UnexpectedCharacter  extends Reason(3)
      case UnterminatedLiteral  extends Reason(4)
      case UnknownAxis          extends Reason(5)
      case ExpectedNodeTest     extends Reason(6)
      case ExpectedExpression   extends Reason(7)
      case ExpectedCloseParen   extends Reason(8)
      case ExpectedCloseBracket extends Reason(9)
      case UnexpectedEnd        extends Reason(10)
      case UnexpectedToken      extends Reason(11)

    given communicable: Reason is Communicable =
      case Reason.ExpectedSlash        => m"an XPath must begin with '/'"
      case Reason.BadStep              => m"the path step is not a valid XPath step"
      case Reason.UnexpectedCharacter  => m"the character is not valid in an XPath"
      case Reason.UnterminatedLiteral  => m"the string literal is not terminated"
      case Reason.UnknownAxis          => m"the axis name is not one of the thirteen XPath axes"
      case Reason.ExpectedNodeTest     => m"a node test was expected"
      case Reason.ExpectedExpression   => m"an expression was expected"
      case Reason.ExpectedCloseParen   => m"a closing parenthesis was expected"
      case Reason.ExpectedCloseBracket => m"a closing square bracket was expected"
      case Reason.UnexpectedEnd        => m"the XPath ends prematurely"
      case Reason.UnexpectedToken      => m"the token was not expected at this position"

  // `offset` is the character index, within the path text, where the error was
  // detected; consumers (e.g. the `xp"…"` interpolator) use it to position a
  // compile-time error precisely.
  case class Error(reason: XPath.Error.Reason, offset: Int)(using Diagnostics)
  extends fulminate.Error(562, reason.number)(m"the XPath was not valid because $reason")

case class XPath(expression: XPath.Expression = XPath.Expression.Route(XPath.Origin.Root, Nil))
derives CanEqual:

  // Appends a step to the path. On a non-path expression, the expression
  // becomes the head of a filter path, per the `FilterExpr '/'
  // RelativeLocationPath` production.
  private def append(step: XPath.Step): XPath = expression match
    case XPath.Expression.Route(origin, steps) =>
      XPath(XPath.Expression.Route(origin, steps :+ step))

    case other =>
      XPath(XPath.Expression.Route(XPath.Origin.Filter(other, Nil), List(step)))

  def element(name: Text, ordinal: Int = 1): XPath =
    append:
      XPath.Step
        ( XPath.Axis.Child,
          XPath.NodeTest.Name(Unset, name),
          List(XPath.Expression.Number(ordinal)) )

  def attribute(name: Text): XPath =
    append(XPath.Step(XPath.Axis.Attribute, XPath.NodeTest.Name(Unset, name), Nil))

  // Prepend `name[ordinal]` at the root end of the path, leaving the rest of
  // the steps intact. Used by `Xml`'s Wisteria derivation: each outer `focus`
  // block runs *after* the inner one, and needs to push its label to the
  // front of the accumulated XPath (so `/parent[1]/child[1]` lands
  // root-first), not append at the leaf end like `element` does.
  private[xylophone] def prepend(name: Text, ordinal: Int = 1): XPath =
    val step =
      XPath.Step
        ( XPath.Axis.Child,
          XPath.NodeTest.Name(Unset, name),
          List(XPath.Expression.Number(ordinal)) )

    expression match
      case XPath.Expression.Route(origin, steps) =>
        XPath(XPath.Expression.Route(origin, List.of(step :: steps.stdlib)))

      case _ =>
        this

  // The simple positional view: `Unset` unless this is an absolute path of
  // `child::name` steps whose only predicate is an integer ordinal (a missing
  // predicate counts as ordinal 1), optionally ending with a single
  // `attribute::name` step — the subset that `Xml`'s position `Locator` can
  // resolve against a `PositionIndex`.
  private[xylophone] def locations: Optional[List[XPath.Location]] =
    import XPath.{Axis, Expression, Location, NodeTest, Origin, Step}

    def recur(steps: List[Step], done: List[Location]): Optional[List[Location]] = steps match
      case Nil =>
        done.reverse

      case Step(Axis.Child, NodeTest.Name(Unset, local), predicates) :: rest =>
        predicates match
          case Nil =>
            recur(rest, Location.Element(local, 1) :: done)

          case List(Expression.Number(value)) =>
            if value == Math.floor(value) && value >= 1
            then recur(rest, Location.Element(local, value.toInt) :: done)
            else Unset

          case _ =>
            Unset

      case Step(Axis.Attribute, NodeTest.Name(Unset, local), Nil) :: Nil =>
        recur(Nil, Location.Attribute(local) :: done)

      case _ =>
        Unset

    expression match
      case Expression.Route(Origin.Root, steps) => recur(steps, Nil)
      case _                                    => Unset
