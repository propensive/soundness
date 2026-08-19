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
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import murmuration.*
import prepositional.*
import vacuous.*
import zephyrine.*
import rudiments.`:+`
import denominative.asymptotics.linearSizeComplexity

// `XPath` is a `Format`, so a malformed *expression* is a `Parse.Error` like any other
// parse failure, carrying the offset at which it was detected. `XPath.Error` is reserved
// for the distinct failure of *evaluating* an expression that parsed.
object XPath extends Format:
  def name: Text = t"XPath"

  // An XPath is a line-less source, so the span is `Offset`-mode: a character index into
  // the expression text, which the `xp"…"` interpolator maps back onto a source-file caret.
  // `Location` is taken here for a node's place within a *document*, so this is `Position`,
  // the name every other `Format` uses for the same role.
  case class Position(override val span: Span) extends Format.Position:
    def describe: Text = span.offset.lay(t"an unknown position"): offset =>
      t"character ${offset.n1}"

  object Position:
    // The parser detects a fault at a point, not over a range, so spans are zero-length.
    def at(offset: Int): Position = Position(Span.offset(offset.z, 0))

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

    // Operator members, not extensions: `soundness.*` exports generic
    // extensions of the same names (`contains`, `===`, `<`, …) whose context
    // bounds commit before their givens resolve, so an extension here would
    // be unreachable under the umbrella import. Members always win.
    infix def or(right: into[Expression]): Expression = Expression.Or(this, right)
    infix def and(right: into[Expression]): Expression = Expression.And(this, right)
    def ===(right: into[Expression]): Expression = Expression.Equal(this, right)
    def !==(right: into[Expression]): Expression = Expression.Unequal(this, right)
    def <(right: into[Expression]): Expression = Expression.Less(this, right)
    def <=(right: into[Expression]): Expression = Expression.LessOrEqual(this, right)
    def >(right: into[Expression]): Expression = Expression.Greater(this, right)
    def >=(right: into[Expression]): Expression = Expression.GreaterOrEqual(this, right)
    def +(right: into[Expression]): Expression = Expression.Add(this, right)
    def -(right: into[Expression]): Expression = Expression.Subtract(this, right)
    def *(right: into[Expression]): Expression = Expression.Multiply(this, right)
    def |(right: into[Expression]): Expression = Expression.Union(this, right)

    infix def contains(right: into[Expression]): Expression =
      Expression.Call(Unset, t"contains", List(this, right))

    infix def startsWith(right: into[Expression]): Expression =
      Expression.Call(Unset, t"starts-with", List(this, right))

  // The simple positional view of a path: what `Xml`'s position `Locator` can
  // resolve against a `PositionIndex`, and what the Wisteria-derived decoders
  // produce for diagnostics.
  enum Location derives CanEqual:
    case Element(name: Text, rank: Int)
    case Attribute(name: Text)

  private[xylophone] def qualify(prefix: Optional[Text], local: Text): Text = prefix match
    case prefix: Text => t"$prefix:$local"
    case _            => local

  private val descendantStep: Step = Step(Axis.DescendantOrSelf, NodeTest.Node, Nil)

  // Typed construction. A bare `Text` converts to a `child::name` step or to
  // a string literal (by target type), so paths and predicates read close to
  // their XPath forms:
  //
  //     XPath.deep(t"div").where(XPath.attribute(t"class").contains(t"button")
  //         and XPath.textual === t"Submit")
  //
  // encodes as `//div[contains(@class,'button') and text()='Submit']`.
  given stepConversion: Conversion[Text, Step] =
    name => Step(Axis.Child, NodeTest.Name(Unset, name))

  given literalConversion: Conversion[Text, Expression] = Expression.Literal(_)
  given integerConversion: Conversion[Int, Expression] = int => Expression.Number(int.toDouble)
  given decimalConversion: Conversion[Double, Expression] = Expression.Number(_)
  given pathConversion: Conversion[XPath, Expression] = _.expression

  // Absolute path start: `XPath / t"html" / t"body"` is `/html/body`.
  def /(step: into[Step]): XPath = XPath(Expression.Route(Origin.Root, List(step)))

  // Descendant-or-self start: `XPath.deep(t"div")` is `//div`.
  def deep(step: into[Step]): XPath =
    XPath(Expression.Route(Origin.Root, List(descendantStep, step)))

  // Relative path start: `XPath.relative(t"div")` is `div`.
  def relative(step: into[Step]): XPath = XPath(Expression.Route(Origin.Here, List(step)))

  // The context node, `.` — usable as a path or, via `pathConversion`, in a
  // predicate.
  val here: XPath = XPath(Expression.Route(Origin.Here, List(Step(Axis.Self, NodeTest.Node))))

  // Predicate constructors.
  def attribute(name: Text): Expression =
    Expression.Route(Origin.Here, List(Step(Axis.Attribute, NodeTest.Name(Unset, name))))

  val textual: Expression =
    Expression.Route(Origin.Here, List(Step(Axis.Child, NodeTest.Textual)))

  val position: Expression = Expression.Call(Unset, t"position", Nil)
  val last: Expression = Expression.Call(Unset, t"last", Nil)

  def function(name: Text)(arguments: into[Expression]*): Expression =
    Expression.Call(Unset, name, List(arguments*))

  def variable(name: Text): Expression = Expression.Variable(Unset, name)

  object Locus:
    private[xylophone] def root(document: Xml): Locus = Locus(document, Nil, Unset, Unset)

    // The string-value (§5) of a single node: elements concatenate every
    // descendant text and CDATA node; the character-carrying kinds are their
    // own content.
    private[xylophone] def textOf(node: Node): Text = node match
      case TextNode(text)                 => text
      case Cdata(text)                    => text
      case Comment(text)                  => text
      case ProcessingInstruction(_, data) => data

      case element: Element =>
        val builder = StringBuilder()
        accumulate(element, builder)
        builder.toString.nn.tt

      case _ =>
        t""

    private def accumulate(element: Element, builder: StringBuilder): Unit =
      val children = element.children
      var i = 0

      while i < children.length do
        children.readUnchecked(i) match
          case TextNode(text)   => builder.append(text.s)
          case Cdata(text)      => builder.append(text.s)
          case child: Element   => accumulate(child, builder)
          case _                => ()

        i += 1

  // A located node: the evaluation subject plus the child-index path from the
  // virtual root (XPath's `/`, the node *above* the root element) down to the
  // node. Index paths provide parentage, document order and identity in one
  // stroke — none of which the node tree itself can offer, since nodes carry
  // no parent pointers and their `equals` deliberately conflates a node with
  // a singleton `Fragment` of it. `subject` is `Unset` for the virtual root.
  // An attribute pseudo-node shares its owner element's `path`, holds the
  // owner as `subject`, and sets `attributeIndex`.
  case class Locus
    ( document:       Xml,
      path:           List[Int],
      subject:        Optional[Node],
      attributeIndex: Optional[Int] ):

    private[xylophone] def attributeName: Optional[Text] = attributeIndex match
      case index: Int => subject match
        case element: Element =>
          val keys = element.attributes.keys.drop(index)
          if keys.hasNext then keys.next() else Unset

        case _ =>
          Unset

      case _ =>
        Unset

    def stringValue: Text = attributeIndex match
      case index: Int => subject match
        case element: Element =>
          val values = element.attributes.values.drop(index)
          if values.hasNext then values.next() else t""

        case _ =>
          t""

      case _ => subject match
        case node: Node => Locus.textOf(node)

        case _ => document match
          case Fragment(nodes*) =>
            val builder = StringBuilder()
            nodes.foreach { node => builder.append(Locus.textOf(node).s) }
            builder.toString.nn.tt

          case node: Node =>
            Locus.textOf(node)

  // The four XPath 1.0 value types (§1), with the conversion rules of §3.2,
  // §4.2 and §4.3 as members. A node-set is always in document order without
  // duplicates.
  enum Value derives CanEqual:
    case NodeSet(loci: List[Locus])
    case Truth(value: Boolean)
    case Numeric(value: Double)
    case Textual(value: Text)

    def truth: Boolean = this match
      case Truth(value)   => value
      case Numeric(value) => value == value && value != 0.0
      case Textual(value) => value.s.length > 0
      case NodeSet(loci)  => loci.stdlib.nonEmpty

    def number: Double = this match
      case Numeric(value) => value
      case Truth(value)   => if value then 1.0 else 0.0
      case Textual(value) => XPath.parseNumber(value)
      case NodeSet(_)     => XPath.parseNumber(text)

    def text: Text = this match
      case Textual(value) => value
      case Truth(value)   => if value then t"true" else t"false"
      case Numeric(value) => XPath.renderNumber(value)

      case NodeSet(loci) => loci.stdlib.headOption match
        case Some(locus) => locus.stringValue
        case None        => t""

  // The `number()` conversion of a string (§4.4): optional whitespace, an
  // optional minus sign, then the Number production — no exponents, no other
  // sign forms; anything else is NaN.
  private[xylophone] def parseNumber(text: Text): Double =
    val trimmed = text.s.trim.nn

    if trimmed.isEmpty then Double.NaN else
      var index = if trimmed.charAt(0) == '-' then 1 else 0
      var digits = false
      var dot = false
      var valid = index < trimmed.length

      while index < trimmed.length && valid do
        val char = trimmed.charAt(index)

        if char >= '0' && char <= '9' then digits = true
        else if char == '.' && !dot then dot = true
        else valid = false

        index += 1

      if valid && digits then java.lang.Double.parseDouble(trimmed) else Double.NaN

  object Error:
    enum Reason(val number: Int) extends Clarification:
      case UnknownFunction(name: Text) extends Reason(1)
      case BadArity(name: Text)        extends Reason(2)
      case UnboundVariable(name: Text) extends Reason(3)
      case NotNodeSet                  extends Reason(4)
      case Unsupported(feature: Text)  extends Reason(5)
      case Unresolved                  extends Reason(6)

    given communicable: Reason is Communicable =
      case Reason.UnknownFunction(name) => m"the function $name is not an XPath 1.0 core function"
      case Reason.BadArity(name)        => m"the function $name was applied to the wrong number of arguments"
      case Reason.UnboundVariable(name) => m"the variable $$$name has no binding"
      case Reason.NotNodeSet            => m"a node-set was expected"
      case Reason.Unsupported(feature)  => m"$feature is not supported"
      case Reason.Unresolved            => m"the expression contains an unresolved substitution"

  case class Error(reason: XPath.Error.Reason)(using Diagnostics)
  extends fulminate.Error(563, reason.number)(m"the XPath could not be evaluated because $reason")

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
  private[xylophone] def renderNumber(value: Double): Text =
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

    case NodeTest.Instruction(target) => target match
      case target: Text => t"processing-instruction('$target')"
      case _            => t"processing-instruction()"

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
    val length = steps.size
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
  given decodable: (tactic: Tactic[Parse.Error]) => ((XPath is Decodable in Text)^{tactic}) = text =>
    XPath(XPathReader.parse(text, holes = false))

  // Was `XPath.Error.Reason`, whose numbering was append-only within the 562 envelope;
  // as `Issue`s they need no numbers, so `ExpectedSlash` and `BadStep` — retained only
  // for numbering, and raised by the pre-AST positional parser — are gone.
  enum Issue extends Format.Issue:
    case UnexpectedCharacter
    case UnterminatedLiteral
    case UnknownAxis
    case ExpectedNodeTest
    case ExpectedExpression
    case ExpectedCloseParen
    case ExpectedCloseBracket
    case UnexpectedEnd
    case UnexpectedToken

    def describe: Message = this match
      case UnexpectedCharacter  => m"the character is not valid in an XPath"
      case UnterminatedLiteral  => m"the string literal is not terminated"
      case UnknownAxis          => m"the axis name is not one of the thirteen XPath axes"
      case ExpectedNodeTest     => m"a node test was expected"
      case ExpectedExpression   => m"an expression was expected"
      case ExpectedCloseParen   => m"a closing parenthesis was expected"
      case ExpectedCloseBracket => m"a closing square bracket was expected"
      case UnexpectedEnd        => m"the XPath ends prematurely"
      case UnexpectedToken      => m"the token was not expected at this position"

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

  // Appends a `child::` step: `XPath / t"html" / t"body"` is `/html/body`.
  def /(step: into[XPath.Step]): XPath = append(step)

  // Appends a descendant-or-self step: `XPath.deep(t"div").deep(t"a")` is
  // `//div//a`.
  def deep(step: into[XPath.Step]): XPath = append(XPath.descendantStep).append(step)

  // Appends a fully-general step on any axis.
  def on(axis: XPath.Axis, test: XPath.NodeTest): XPath = append(XPath.Step(axis, test))

  // Adds a predicate to the final step (or, for a stepless filter path, to
  // the filter expression): `XPath.deep(t"div").where(...)` is `//div[...]`.
  def where(predicate: into[XPath.Expression]): XPath = expression match
    case XPath.Expression.Route(origin, steps) =>
      val stdlibSteps = steps.stdlib

      if stdlibSteps.isEmpty then origin match
        case XPath.Origin.Filter(filtered, predicates) =>
          val amended = XPath.Origin.Filter(filtered, List.of(predicates.stdlib :+ predicate))
          XPath(XPath.Expression.Route(amended, Nil))

        case _ =>
          XPath(XPath.Expression.Route(XPath.Origin.Filter(expression, List(predicate)), Nil))
      else
        val lastStep = stdlibSteps.last
        val amended = lastStep.copy(predicates = List.of(lastStep.predicates.stdlib :+ predicate))
        XPath(XPath.Expression.Route(origin, List.of(stdlibSteps.init :+ amended)))

    case other =>
      XPath(XPath.Expression.Route(XPath.Origin.Filter(other, List(predicate)), Nil))

  // Positional predicate: `(XPath / t"li")(2)` is `/li[2]`.
  def apply(ordinal: Int): XPath = where(XPath.Expression.Number(ordinal))

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
