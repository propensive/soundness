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

import scala.collection.immutable as sci
import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import gossamer.*
import vacuous.*

private[xylophone] object XPathEngine:
  import XPath.{Axis, EvaluationError, Expression, Locus, NodeTest, Origin, Step, Value}
  import EvaluationError.Reason

  private case class Context
    ( locus: Locus, position: Int, size: Int, variables: Map[Text, Value] )

  def evaluate(xml: Xml, expression: Expression, variables: Map[Text, Value])
    ( using Tactic[EvaluationError] )
  :   Value =

    evaluate(expression, Context(Locus.root(xml), 1, 1, variables))

  private def evaluate(expression: Expression, context: Context)(using Tactic[EvaluationError])
  :   Value =

    expression match
      case Expression.Or(left, right) =>
        Value.Truth(evaluate(left, context).truth || evaluate(right, context).truth)

      case Expression.And(left, right) =>
        Value.Truth(evaluate(left, context).truth && evaluate(right, context).truth)

      case Expression.Equal(left, right) =>
        Value.Truth(equalTest(evaluate(left, context), evaluate(right, context), true))

      case Expression.Unequal(left, right) =>
        Value.Truth(equalTest(evaluate(left, context), evaluate(right, context), false))

      case Expression.Less(left, right) =>
        Value.Truth(relationalTest(evaluate(left, context), evaluate(right, context), _ < _))

      case Expression.LessOrEqual(left, right) =>
        Value.Truth(relationalTest(evaluate(left, context), evaluate(right, context), _ <= _))

      case Expression.Greater(left, right) =>
        Value.Truth(relationalTest(evaluate(left, context), evaluate(right, context), _ > _))

      case Expression.GreaterOrEqual(left, right) =>
        Value.Truth(relationalTest(evaluate(left, context), evaluate(right, context), _ >= _))

      case Expression.Add(left, right) =>
        Value.Numeric(evaluate(left, context).number + evaluate(right, context).number)

      case Expression.Subtract(left, right) =>
        Value.Numeric(evaluate(left, context).number - evaluate(right, context).number)

      case Expression.Multiply(left, right) =>
        Value.Numeric(evaluate(left, context).number * evaluate(right, context).number)

      case Expression.Divide(left, right) =>
        Value.Numeric(evaluate(left, context).number / evaluate(right, context).number)

      case Expression.Modulo(left, right) =>
        Value.Numeric(evaluate(left, context).number % evaluate(right, context).number)

      case Expression.Negate(operand) =>
        Value.Numeric(-evaluate(operand, context).number)

      case Expression.Union(left, right) =>
        (evaluate(left, context), evaluate(right, context)) match
          case (Value.NodeSet(left), Value.NodeSet(right)) =>
            Value.NodeSet(List.of(sortDedup(left.stdlib ++ right.stdlib)))

          case _ =>
            abort(EvaluationError(Reason.NotNodeSet))

      case Expression.Literal(text)  => Value.Textual(text)
      case Expression.Number(value)  => Value.Numeric(value)

      case Expression.Variable(prefix, name) =>
        val key = XPath.qualify(prefix, name)

        context.variables.get(key) match
          case Some(value) => value
          case None        => abort(EvaluationError(Reason.UnboundVariable(key)))

      case Expression.Call(prefix, name, arguments) =>
        prefix match
          case prefix: Text =>
            abort(EvaluationError(Reason.UnknownFunction(XPath.qualify(prefix, name))))

          case _ =>
            call(name, arguments.stdlib.map(evaluate(_, context)), context)

      case Expression.Route(origin, steps) =>
        Value.NodeSet(List.of(route(origin, steps.stdlib, context)))

      case Expression.Substitution(_) =>
        abort(EvaluationError(Reason.Unresolved))

  // Comparison semantics (§3.4): node-sets compare existentially over their
  // members' string-values (or numbers, for relational operators); mixed
  // plain comparisons promote to boolean, then number, then string.
  private def equalTest(left: Value, right: Value, equal: Boolean): Boolean = (left, right) match
    case (Value.NodeSet(left), Value.NodeSet(right)) =>
      val rights = right.stdlib.map(_.stringValue.s)
      left.stdlib.exists { locus =>
        val value = locus.stringValue.s
        rights.exists { other => (value == other) == equal }
      }

    case (Value.NodeSet(loci), other) => nodeSetTest(loci.stdlib, other, equal)
    case (other, Value.NodeSet(loci)) => nodeSetTest(loci.stdlib, other, equal)

    case (left, right) => (left, right) match
      case (Value.Truth(_), _) | (_, Value.Truth(_))     => (left.truth == right.truth) == equal
      case (Value.Numeric(_), _) | (_, Value.Numeric(_)) => (left.number == right.number) == equal
      case _                                             => (left.text.s == right.text.s) == equal

  private def nodeSetTest(loci: sci.List[Locus], other: Value, equal: Boolean): Boolean =
    other match
      case Value.Truth(value) => (loci.nonEmpty == value) == equal

      case Value.Numeric(value) =>
        loci.exists { locus => (XPath.parseNumber(locus.stringValue) == value) == equal }

      case Value.Textual(value) =>
        loci.exists { locus => (locus.stringValue.s == value.s) == equal }

      case _ =>
        false

  private def relationalTest(left: Value, right: Value, test: (Double, Double) => Boolean)
  :   Boolean =

    def numberOf(locus: Locus): Double = XPath.parseNumber(locus.stringValue)

    (left, right) match
      case (Value.NodeSet(left), Value.NodeSet(right)) =>
        left.stdlib.exists { a => right.stdlib.exists { b => test(numberOf(a), numberOf(b)) } }

      case (Value.NodeSet(loci), other) =>
        val number = other.number
        loci.stdlib.exists { locus => test(numberOf(locus), number) }

      case (other, Value.NodeSet(loci)) =>
        val number = other.number
        loci.stdlib.exists { locus => test(number, numberOf(locus)) }

      case (left, right) =>
        test(left.number, right.number)

  // Document order and identity, both from the index path: lexicographic
  // comparison, ancestors before descendants, and an element before its
  // attributes.
  private def attributeIndexOf(locus: Locus): Int = locus.attributeIndex match
    case index: Int => index
    case _          => -1

  private def compareLoci(a: Locus, b: Locus): Int =
    var pa = a.path.stdlib
    var pb = b.path.stdlib

    while pa.nonEmpty && pb.nonEmpty do
      val difference = pa.head - pb.head
      if difference != 0 then return difference
      pa = pa.tail
      pb = pb.tail

    if pa.nonEmpty then 1
    else if pb.nonEmpty then -1
    else attributeIndexOf(a) - attributeIndexOf(b)

  private def sortDedup(loci: sci.List[Locus]): sci.List[Locus] =
    val sorted = loci.sortWith { (a, b) => compareLoci(a, b) < 0 }
    val buffer = scm.ListBuffer[Locus]()

    sorted.foreach: locus =>
      if buffer.isEmpty || compareLoci(buffer.last, locus) != 0 then buffer += locus

    buffer.toList

  private def treeNode(node: Node): Boolean = node match
    case _: Header | _: Doctype => false
    case _                      => true

  private def appendIndex(path: List[Int], index: Int): List[Int] =
    List.of(path.stdlib :+ index)

  private def childLoci(locus: Locus): sci.List[Locus] =
    if attributeIndexOf(locus) >= 0 then sci.Nil else locus.subject match
      case element: Element =>
        val children = element.children
        val buffer = scm.ListBuffer[Locus]()
        var i = 0

        while i < children.length do
          val child = children.readUnchecked(i)
          if treeNode(child) then
            buffer += Locus(locus.document, appendIndex(locus.path, i), child, Unset)
          i += 1

        buffer.toList

      case _: Node =>
        sci.Nil

      case _ => locus.document match
        case Fragment(nodes*) =>
          nodes.zipWithIndex.collect:
            case (node, index) if treeNode(node) =>
              Locus(locus.document, appendIndex(locus.path, index), node, Unset)
          . to(sci.List)

        case node: Node =>
          if treeNode(node)
          then sci.List(Locus(locus.document, appendIndex(locus.path, 0), node, Unset))
          else sci.Nil

  private def descendantLoci(locus: Locus): sci.List[Locus] =
    childLoci(locus).flatMap { child => child :: descendantLoci(child) }

  private def nodeAt(locus: Locus, index: Int): Node = locus.subject match
    case element: Element => element.children.readUnchecked(index)

    case _ => locus.document match
      case Fragment(nodes*) => nodes(index)
      case node: Node       => node

  private def resolve(document: Xml, path: sci.List[Int]): Locus =
    path.foldLeft(Locus.root(document)): (locus, index) =>
      Locus(document, appendIndex(locus.path, index), nodeAt(locus, index), Unset)

  private def parentLocus(locus: Locus): sci.List[Locus] =
    if attributeIndexOf(locus) >= 0
    then sci.List(Locus(locus.document, locus.path, locus.subject, Unset))
    else
      val path = locus.path.stdlib
      if path.isEmpty then sci.Nil else sci.List(resolve(locus.document, path.init))

  // Nearest-first, as a reverse axis requires for proximity positions.
  private def ancestorLoci(locus: Locus): sci.List[Locus] = parentLocus(locus).headOption match
    case Some(parent) => parent :: ancestorLoci(parent)
    case None         => sci.Nil

  private def siblingLoci(locus: Locus, following: Boolean): sci.List[Locus] =
    if attributeIndexOf(locus) >= 0 || locus.path.stdlib.isEmpty then sci.Nil else
      val mine = locus.path.stdlib.last

      parentLocus(locus).headOption match
        case Some(parent) =>
          val all = childLoci(parent)
          if following then all.filter(_.path.stdlib.last > mine)
          else all.filter(_.path.stdlib.last < mine).reverse

        case None =>
          sci.Nil

  private def isPrefix(prefix: sci.List[Int], path: sci.List[Int]): Boolean =
    prefix.length < path.length && path.take(prefix.length) == prefix

  private def followingLoci(locus: Locus): sci.List[Locus] =
    val root = Locus.root(locus.document)

    descendantLoci(root).filter: candidate =>
      compareLoci(locus, candidate) < 0
      && !isPrefix(locus.path.stdlib, candidate.path.stdlib)

  // Nearest-first (reverse document order), as a reverse axis requires.
  private def precedingLoci(locus: Locus): sci.List[Locus] =
    val root = Locus.root(locus.document)

    descendantLoci(root).filter: candidate =>
      compareLoci(candidate, locus) < 0
      && !isPrefix(candidate.path.stdlib, locus.path.stdlib)

    . reverse

  private def attributeLoci(locus: Locus): sci.List[Locus] =
    if attributeIndexOf(locus) >= 0 then sci.Nil else locus.subject match
      case element: Element =>
        val buffer = scm.ListBuffer[Locus]()
        var i = 0

        while i < element.attributes.size do
          buffer += Locus(locus.document, locus.path, element, i)
          i += 1

        buffer.toList

      case _ =>
        sci.Nil

  private def axisLoci(axis: Axis, locus: Locus)(using Tactic[EvaluationError])
  :   sci.List[Locus] =

    axis match
      case Axis.Child            => childLoci(locus)
      case Axis.Descendant       => descendantLoci(locus)
      case Axis.DescendantOrSelf => locus :: descendantLoci(locus)
      case Axis.Self             => sci.List(locus)
      case Axis.Parent           => parentLocus(locus)
      case Axis.Ancestor         => ancestorLoci(locus)
      case Axis.AncestorOrSelf   => locus :: ancestorLoci(locus)
      case Axis.FollowingSibling => siblingLoci(locus, true)
      case Axis.PrecedingSibling => siblingLoci(locus, false)
      case Axis.Following        => followingLoci(locus)
      case Axis.Preceding        => precedingLoci(locus)
      case Axis.Attribute        => attributeLoci(locus)

      case Axis.Namespace =>
        abort(EvaluationError(Reason.Unsupported(t"the namespace axis")))

  // Node tests, with the principal node type of the axis (§2.3): a name or
  // wildcard on the attribute axis matches attributes; on every other axis,
  // elements. Names match the raw label — this model performs no namespace
  // processing, so `svg:rect` matches the literal label `svg:rect`.
  private def testLocus(test: NodeTest, axis: Axis, locus: Locus): Boolean =
    val attributeAxis = axis == Axis.Attribute
    val isAttribute = attributeIndexOf(locus) >= 0

    test match
      case NodeTest.Name(prefix, local) =>
        val qname = XPath.qualify(prefix, local)

        if attributeAxis then locus.attributeName match
          case name: Text => name.s == qname.s
          case _          => false
        else if isAttribute then false
        else locus.subject match
          case element: Element => element.label.s == qname.s
          case _                => false

      case NodeTest.Wildcard =>
        if attributeAxis then isAttribute
        else
          !isAttribute && (locus.subject match
            case _: Element => true
            case _          => false)

      case NodeTest.PrefixWildcard(prefix) =>
        val start = t"$prefix:"

        if attributeAxis then locus.attributeName match
          case name: Text => name.s.startsWith(start.s)
          case _          => false
        else if isAttribute then false
        else locus.subject match
          case element: Element => element.label.s.startsWith(start.s)
          case _                => false

      case NodeTest.Node =>
        true

      case NodeTest.Textual =>
        !isAttribute && (locus.subject match
          case _: TextNode | _: Cdata => true
          case _                      => false)

      case NodeTest.Comment =>
        !isAttribute && (locus.subject match
          case _: Comment => true
          case _          => false)

      case NodeTest.Instruction(target) =>
        !isAttribute && (locus.subject match
          case ProcessingInstruction(target0, _) => target match
            case target: Text => target0.s == target.s
            case _            => true

          case _ =>
            false)

  // Predicates evaluate over the candidate list in axis order, so that
  // `position()` is the proximity position (§2.4): a numeric result keeps
  // the candidate at that position; anything else coerces to boolean.
  private def filterPredicates
    ( candidates: sci.List[Locus],
      predicates: sci.List[Expression],
      variables:  Map[Text, Value] )
    ( using Tactic[EvaluationError] )
  :   sci.List[Locus] =

    predicates.foldLeft(candidates): (current, predicate) =>
      val size = current.length

      current.zipWithIndex.filter: (locus, index) =>
        evaluate(predicate, Context(locus, index + 1, size, variables)) match
          case Value.Numeric(value) => value == index + 1
          case value                => value.truth

      . map(_(0))

  private def evaluateStep
    ( step: Step, inputs: sci.List[Locus], variables: Map[Text, Value] )
    ( using Tactic[EvaluationError] )
  :   sci.List[Locus] =

    val collected = inputs.flatMap: input =>
      val candidates = axisLoci(step.axis, input).filter(testLocus(step.test, step.axis, _))
      filterPredicates(candidates, step.predicates.stdlib, variables)

    sortDedup(collected)

  private def route(origin: Origin, steps: sci.List[Step], context: Context)
    ( using Tactic[EvaluationError] )
  :   sci.List[Locus] =

    val start: sci.List[Locus] = origin match
      case Origin.Root => sci.List(Locus.root(context.locus.document))
      case Origin.Here => sci.List(context.locus)

      case Origin.Filter(expression, predicates) =>
        evaluate(expression, context) match
          case Value.NodeSet(loci) =>
            filterPredicates(sortDedup(loci.stdlib), predicates.stdlib, context.variables)

          case _ =>
            abort(EvaluationError(Reason.NotNodeSet))

    steps.foldLeft(start) { (loci, step) => evaluateStep(step, loci, context.variables) }

  // The name of a node, as `name()` reports it: an element's label, an
  // attribute's key, a processing instruction's target; empty otherwise.
  private def nodeNameOf(locus: Locus): Text = locus.attributeName match
    case name: Text => name

    case _ => locus.subject match
      case element: Element                     => element.label
      case ProcessingInstruction(target, _)     => target
      case _                                    => t""

  // The rounding used by `round()` and `substring()` (§4.2, §4.4):
  // floor(x + 0.5), with NaN and the infinities passing through.
  private def xpathRound(value: Double): Double =
    if value != value || java.lang.Double.isInfinite(value) then value
    else Math.floor(value + 0.5)

  // The core function library (§4). Zero-argument forms of `string`,
  // `number`, `string-length`, `normalize-space`, `name` and friends default
  // to the context node.
  private def call(name: Text, arguments: sci.List[Value], context: Context)
    ( using Tactic[EvaluationError] )
  :   Value =

    def arity(minimum: Int, maximum: Int): Unit =
      if arguments.length < minimum || arguments.length > maximum
      then abort(EvaluationError(Reason.BadArity(name)))

    def nodeSetArgument(value: Value): sci.List[Locus] = value match
      case Value.NodeSet(loci) => sortDedup(loci.stdlib)
      case _                   => abort(EvaluationError(Reason.NotNodeSet))

    def defaulted: Value =
      if arguments.isEmpty then Value.NodeSet(List(context.locus)) else arguments.head

    name.s match
      case "last" =>
        arity(0, 0)
        Value.Numeric(context.size)

      case "position" =>
        arity(0, 0)
        Value.Numeric(context.position)

      case "count" =>
        arity(1, 1)
        Value.Numeric(nodeSetArgument(arguments.head).length)

      case "id" =>
        abort(EvaluationError(Reason.Unsupported(t"the id() function")))

      case "local-name" | "name" | "namespace-uri" =>
        arity(0, 1)

        if name.s == "namespace-uri" then Value.Textual(t"") else
          val loci =
            if arguments.isEmpty then sci.List(context.locus)
            else nodeSetArgument(arguments.head)

          val qualified = loci.headOption match
            case Some(locus) => nodeNameOf(locus)
            case None        => t""

          if name.s == "name" then Value.Textual(qualified) else
            val colon = qualified.s.indexOf(':')
            if colon < 0 then Value.Textual(qualified)
            else Value.Textual(qualified.s.substring(colon + 1).nn.tt)

      case "string" =>
        arity(0, 1)
        Value.Textual(defaulted.text)

      case "concat" =>
        if arguments.length < 2 then abort(EvaluationError(Reason.BadArity(name)))
        val builder = StringBuilder()
        arguments.foreach { argument => builder.append(argument.text.s) }
        Value.Textual(builder.toString.nn.tt)

      case "starts-with" =>
        arity(2, 2)
        Value.Truth(arguments(0).text.s.startsWith(arguments(1).text.s))

      case "contains" =>
        arity(2, 2)
        Value.Truth(arguments(0).text.s.contains(arguments(1).text.s))

      case "substring-before" =>
        arity(2, 2)
        val whole = arguments(0).text.s
        val index = whole.indexOf(arguments(1).text.s)
        Value.Textual(if index < 0 then t"" else whole.substring(0, index).nn.tt)

      case "substring-after" =>
        arity(2, 2)
        val whole = arguments(0).text.s
        val part = arguments(1).text.s
        val index = whole.indexOf(part)
        Value.Textual(if index < 0 then t"" else whole.substring(index + part.length).nn.tt)

      case "substring" =>
        arity(2, 3)
        val whole = arguments(0).text.s
        val start = xpathRound(arguments(1).number)

        val limit =
          if arguments.length == 3 then start + xpathRound(arguments(2).number)
          else Double.PositiveInfinity

        val builder = StringBuilder()
        var position = 1

        while position <= whole.length do
          if position >= start && position < limit then builder.append(whole.charAt(position - 1))
          position += 1

        Value.Textual(builder.toString.nn.tt)

      case "string-length" =>
        arity(0, 1)
        Value.Numeric(defaulted.text.s.length)

      case "normalize-space" =>
        arity(0, 1)
        val whole = defaulted.text.s
        val builder = StringBuilder()
        var pending = false
        var index = 0

        while index < whole.length do
          val char = whole.charAt(index)

          if char == ' ' || char == '\t' || char == '\r' || char == '\n' then
            if builder.length > 0 then pending = true
          else
            if pending then builder.append(' ')
            pending = false
            builder.append(char)

          index += 1

        Value.Textual(builder.toString.nn.tt)

      case "translate" =>
        arity(3, 3)
        val whole = arguments(0).text.s
        val from = arguments(1).text.s
        val to = arguments(2).text.s
        val builder = StringBuilder()
        var index = 0

        while index < whole.length do
          val char = whole.charAt(index)
          val mapping = from.indexOf(char)

          if mapping < 0 then builder.append(char)
          else if mapping < to.length then builder.append(to.charAt(mapping))

          index += 1

        Value.Textual(builder.toString.nn.tt)

      case "boolean" =>
        arity(1, 1)
        Value.Truth(arguments.head.truth)

      case "not" =>
        arity(1, 1)
        Value.Truth(!arguments.head.truth)

      case "true" =>
        arity(0, 0)
        Value.Truth(true)

      case "false" =>
        arity(0, 0)
        Value.Truth(false)

      case "lang" =>
        arity(1, 1)
        val wanted = arguments.head.text.s.toLowerCase.nn

        val declared = (context.locus :: ancestorLoci(context.locus)).flatMap: locus =>
          locus.subject match
            case element: Element if attributeIndexOf(locus) < 0 =>
              element.attributes.fetch(t"xml:lang") match
                case value: Text => sci.List(value.s.toLowerCase.nn)
                case _           => sci.Nil

            case _ =>
              sci.Nil

        Value.Truth:
          declared.headOption match
            case Some(language) =>
              language == wanted || language.startsWith(wanted + "-")

            case None =>
              false

      case "number" =>
        arity(0, 1)
        Value.Numeric(defaulted.number)

      case "sum" =>
        arity(1, 1)
        var total = 0.0

        nodeSetArgument(arguments.head).foreach: locus =>
          total += XPath.parseNumber(locus.stringValue)

        Value.Numeric(total)

      case "floor" =>
        arity(1, 1)
        Value.Numeric(Math.floor(arguments.head.number))

      case "ceiling" =>
        arity(1, 1)
        Value.Numeric(Math.ceil(arguments.head.number))

      case "round" =>
        arity(1, 1)
        Value.Numeric(xpathRound(arguments.head.number))

      case _ =>
        abort(EvaluationError(Reason.UnknownFunction(name)))
