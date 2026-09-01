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


import scala.annotation.tailrec
import scala.math.Ordering

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import gossamer.*
import vacuous.*
import denominative.{nil, size}

// Index paths are tree-depth-short, so the linear `last` and `lead` they need are
// acknowledged rather than avoided.
import denominative.dysasymptotics.linearSize
import rudiments.*
import symbolism.*

private[xylophone] object XPathEngine:
  import XPath.{Axis, Error, Expression, Locus, NodeTest, Origin, Step, Value}
  import Error.Reason

  private case class Context
    ( locus: Locus, position: Int, size: Int, variables: Map[Text, Value] )

  def evaluate(xml: Xml, expression: Expression, variables: Map[Text, Value])
    ( using Tactic[Error] )
  :   Value =

    evaluate(expression, Context(Locus.root(xml), 1, 1, variables))

  private def evaluate(expression: Expression, context: Context)(using Tactic[Error])
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
            Value.NodeSet(sortDedup(left + right))

          case _ =>
            abort(Error(Reason.NotNodeSet))

      case Expression.Literal(text)  => Value.Textual(text)
      case Expression.Number(value)  => Value.Numeric(value)

      case Expression.Variable(prefix, name) =>
        val key = XPath.qualify(prefix, name)

        context.variables.at(key).lay(abort(Error(Reason.UnboundVariable(key))))(identity(_))

      case Expression.Call(prefix, name, arguments) =>
        prefix match
          case prefix: Text =>
            abort(Error(Reason.UnknownFunction(XPath.qualify(prefix, name))))

          case _ =>
            call(name, arguments.map(evaluate(_, context)), context)

      case Expression.Route(origin, steps) =>
        Value.NodeSet(route(origin, steps, context))

      case Expression.Substitution(_) =>
        abort(Error(Reason.Unresolved))

  // Comparison semantics (§3.4): node-sets compare existentially over their
  // members' string-values (or numbers, for relational operators); mixed
  // plain comparisons promote to boolean, then number, then string.
  private def equalTest(left: Value, right: Value, equal: Boolean): Boolean = (left, right) match
    case (Value.NodeSet(left), Value.NodeSet(right)) =>
      val rights = right.map(_.stringValue.s)
      left.exists { locus =>
        val value = locus.stringValue.s
        rights.exists { other => (value == other) == equal }
      }

    case (Value.NodeSet(loci), other) => nodeSetTest(loci, other, equal)
    case (other, Value.NodeSet(loci)) => nodeSetTest(loci, other, equal)

    case (left, right) => (left, right) match
      case (Value.Truth(_), _) | (_, Value.Truth(_))     => (left.truth == right.truth) == equal
      case (Value.Numeric(_), _) | (_, Value.Numeric(_)) => (left.number == right.number) == equal
      case _                                             => (left.text.s == right.text.s) == equal

  private def nodeSetTest(loci: List[Locus], other: Value, equal: Boolean): Boolean =
    other match
      case Value.Truth(value) => (!loci.nil == value) == equal

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
        left.exists { a => right.exists { b => test(numberOf(a), numberOf(b)) } }

      case (Value.NodeSet(loci), other) =>
        val number = other.number
        loci.exists { locus => test(numberOf(locus), number) }

      case (other, Value.NodeSet(loci)) =>
        val number = other.number
        loci.exists { locus => test(number, numberOf(locus)) }

      case (left, right) =>
        test(left.number, right.number)

  // Document order and identity, both from the index path: lexicographic
  // comparison, ancestors before descendants, and an element before its
  // attributes.
  private def attributeIndexOf(locus: Locus): Int = locus.attributeIndex match
    case index: Int => index
    case _          => -1

  private def compareLoci(a: Locus, b: Locus): Int =
    @tailrec
    def recur(left: List[Int], right: List[Int]): Int = (left, right) match
      case (x :: xs, y :: ys) => if x != y then x - y else recur(xs, ys)
      case (_ :: _, Nil)      => 1
      case (Nil, _ :: _)      => -1
      case _                  => attributeIndexOf(a) - attributeIndexOf(b)

    recur(a.path, b.path)

  // Document order is a total order on loci, so it is expressed as an `Ordering`
  // and the sort is the native one.
  private given locusOrder: Ordering[Locus] = Ordering.fromLessThan(compareLoci(_, _) < 0)

  private def sortDedup(loci: List[Locus]): List[Locus] =
    val buffer = scm.ListBuffer[Locus]()

    loci.sort.each: locus =>
      if buffer.isEmpty || compareLoci(buffer.last, locus) != 0 then buffer += locus

    buffer.to(List)

  private def treeNode(node: Node): Boolean = node match
    case _: Header | _: Doctype => false
    case _                      => true

  private def appendIndex(path: List[Int], index: Int): List[Int] = path + List(index)

  private def childLoci(locus: Locus): List[Locus] =
    if attributeIndexOf(locus) >= 0 then Nil else locus.subject match
      case element: Element =>
        val children = element.children
        val buffer = scm.ListBuffer[Locus]()
        var i = 0

        while i < children.length do
          val child = children.readUnchecked(i)
          if treeNode(child) then
            buffer += Locus(locus.document, appendIndex(locus.path, i), child, Unset)
          i += 1

        buffer.to(List)

      case _: Node =>
        Nil

      case _ => locus.document match
        case Fragment(nodes*) =>
          nodes.zipWithIndex.collect:
            case (node, index) if treeNode(node) =>
              Locus(locus.document, appendIndex(locus.path, index), node, Unset)
          . to(List)

        case node: Node =>
          if treeNode(node)
          then List(Locus(locus.document, appendIndex(locus.path, 0), node, Unset))
          else Nil

  private def descendantLoci(locus: Locus): List[Locus] =
    childLoci(locus).flatMap { child => child :: descendantLoci(child) }

  private def nodeAt(locus: Locus, index: Int): Node = locus.subject match
    case element: Element => element.children.readUnchecked(index)

    case _ => locus.document match
      case Fragment(nodes*) => nodes(index)
      case node: Node       => node

  private def resolve(document: Xml, path: List[Int]): Locus =
    path.fold(Locus.root(document)): (locus, index) =>
      Locus(document, appendIndex(locus.path, index), nodeAt(locus, index), Unset)

  private def parentLocus(locus: Locus): List[Locus] =
    if attributeIndexOf(locus) >= 0
    then List(Locus(locus.document, locus.path, locus.subject, Unset))
    else
      locus.path.occupied.lay(Nil): path =>
        List(resolve(locus.document, path.lead))

  // Nearest-first, as a reverse axis requires for proximity positions.
  private def ancestorLoci(locus: Locus): List[Locus] = parentLocus(locus) match
    case parent :: _ => parent :: ancestorLoci(parent)
    case _           => Nil

  private def siblingLoci(locus: Locus, following: Boolean): List[Locus] =
    if attributeIndexOf(locus) >= 0 then Nil else locus.path.last.lay(Nil): mine =>
      parentLocus(locus).prim.lay(Nil): parent =>
        // Every child's path ends in its own index, so `last` is present; the
        // absent case cannot arise and excludes the candidate.
        val all = childLoci(parent)
        if following then all.filter(_.path.last.let(_ > mine).or(false))
        else all.filter(_.path.last.let(_ < mine).or(false)).reverse

  // A strict prefix: `prefix` matches a leading run of `path` and is shorter,
  // so a locus is never its own ancestor. Recursive rather than length-based,
  // since both are `List`s and neither length is needed in full.
  @tailrec
  private def isPrefix(prefix: List[Int], path: List[Int]): Boolean = (prefix, path) match
    case (Nil, _ :: _)      => true
    case (x :: xs, y :: ys) => x == y && isPrefix(xs, ys)
    case _                  => false

  private def followingLoci(locus: Locus): List[Locus] =
    val root = Locus.root(locus.document)

    descendantLoci(root).filter: candidate =>
      compareLoci(locus, candidate) < 0
      && !isPrefix(locus.path, candidate.path)

  // Nearest-first (reverse document order), as a reverse axis requires.
  private def precedingLoci(locus: Locus): List[Locus] =
    val root = Locus.root(locus.document)

    descendantLoci(root).filter: candidate =>
      compareLoci(candidate, locus) < 0
      && !isPrefix(candidate.path, locus.path)

    . reverse

  private def attributeLoci(locus: Locus): List[Locus] =
    if attributeIndexOf(locus) >= 0 then Nil else locus.subject match
      case element: Element =>
        val buffer = scm.ListBuffer[Locus]()
        var i = 0

        while i < element.attributes.size do
          buffer += Locus(locus.document, locus.path, element, i)
          i += 1

        buffer.to(List)

      case _ =>
        Nil

  private def axisLoci(axis: Axis, locus: Locus)(using Tactic[Error])
  :   List[Locus] =

    axis match
      case Axis.Child            => childLoci(locus)
      case Axis.Descendant       => descendantLoci(locus)
      case Axis.DescendantOrSelf => locus :: descendantLoci(locus)
      case Axis.Self             => List(locus)
      case Axis.Parent           => parentLocus(locus)
      case Axis.Ancestor         => ancestorLoci(locus)
      case Axis.AncestorOrSelf   => locus :: ancestorLoci(locus)
      case Axis.FollowingSibling => siblingLoci(locus, true)
      case Axis.PrecedingSibling => siblingLoci(locus, false)
      case Axis.Following        => followingLoci(locus)
      case Axis.Preceding        => precedingLoci(locus)
      case Axis.Attribute        => attributeLoci(locus)

      case Axis.Namespace =>
        abort(Error(Reason.Unsupported(t"the namespace axis")))

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
    ( candidates: List[Locus],
      predicates: List[Expression],
      variables:  Map[Text, Value] )
    ( using Tactic[Error] )
  :   List[Locus] =

    predicates.fold(candidates): (current, predicate) =>
      // `last()` reports the candidate count, so the size is needed in full.
      val size = current.size

      current.indexed.filter: (locus, ordinal) =>
        evaluate(predicate, Context(locus, ordinal.n1, size, variables)) match
          case Value.Numeric(value) => value == ordinal.n1
          case value                => value.truth

      . map(_(0))

  private def evaluateStep
    ( step: Step, inputs: List[Locus], variables: Map[Text, Value] )
    ( using Tactic[Error] )
  :   List[Locus] =

    val collected = inputs.flatMap: input =>
      val candidates = axisLoci(step.axis, input).filter(testLocus(step.test, step.axis, _))
      filterPredicates(candidates, step.predicates, variables)

    sortDedup(collected)

  private def route(origin: Origin, steps: List[Step], context: Context)
    ( using Tactic[Error] )
  :   List[Locus] =

    val start: List[Locus] = origin match
      case Origin.Root => List(Locus.root(context.locus.document))
      case Origin.Here => List(context.locus)

      case Origin.Filter(expression, predicates) =>
        evaluate(expression, context) match
          case Value.NodeSet(loci) =>
            filterPredicates(sortDedup(loci), predicates, context.variables)

          case _ =>
            abort(Error(Reason.NotNodeSet))

    steps.fold(start) { (loci, step) => evaluateStep(step, loci, context.variables) }

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
  private def call(name: Text, arguments: List[Value], context: Context)
    ( using Tactic[Error] )
  :   Value =

    def arity(minimum: Int, maximum: Int): Unit =
      if arguments.size < minimum || arguments.size > maximum
      then abort(Error(Reason.BadArity(name)))

    // Every positional read below is preceded by the `arity` check its function
    // requires, so one stdlib view serves them all rather than threading a
    // presence proof through each of XPath's twenty-odd core functions.
    val args = arguments.stdlib

    def nodeSetArgument(value: Value): List[Locus] = value match
      case Value.NodeSet(loci) => sortDedup(loci)
      case _                   => abort(Error(Reason.NotNodeSet))

    def defaulted: Value =
      arguments.prim.lay(Value.NodeSet(List(context.locus)))(identity(_))

    name.s match
      case "last" =>
        arity(0, 0)
        Value.Numeric(context.size)

      case "position" =>
        arity(0, 0)
        Value.Numeric(context.position)

      case "count" =>
        arity(1, 1)
        Value.Numeric(nodeSetArgument(args.head).size)

      case "id" =>
        abort(Error(Reason.Unsupported(t"the id() function")))

      case "local-name" | "name" | "namespace-uri" =>
        arity(0, 1)

        if name.s == "namespace-uri" then Value.Textual(t"") else
          val loci =
            if arguments.nil then List(context.locus) else nodeSetArgument(args.head)

          val qualified = loci.prim.let(nodeNameOf(_)).or(t"")

          if name.s == "name" then Value.Textual(qualified) else
            val colon = qualified.s.indexOf(':')
            if colon < 0 then Value.Textual(qualified)
            else Value.Textual(qualified.s.substring(colon + 1).nn.tt)

      case "string" =>
        arity(0, 1)
        Value.Textual(defaulted.text)

      case "concat" =>
        if args.length < 2 then abort(Error(Reason.BadArity(name)))
        val builder = StringBuilder()
        arguments.each { argument => builder.append(argument.text.s) }
        Value.Textual(builder.toString.nn.tt)

      case "starts-with" =>
        arity(2, 2)
        Value.Truth(args(0).text.s.startsWith(args(1).text.s))

      case "contains" =>
        arity(2, 2)
        Value.Truth(args(0).text.s.contains(args(1).text.s))

      case "substring-before" =>
        arity(2, 2)
        val whole = args(0).text.s
        val index = whole.indexOf(args(1).text.s)
        Value.Textual(if index < 0 then t"" else whole.substring(0, index).nn.tt)

      case "substring-after" =>
        arity(2, 2)
        val whole = args(0).text.s
        val part = args(1).text.s
        val index = whole.indexOf(part)
        Value.Textual(if index < 0 then t"" else whole.substring(index + part.length).nn.tt)

      case "substring" =>
        arity(2, 3)
        val whole = args(0).text.s
        val start = xpathRound(args(1).number)

        val limit =
          if args.length == 3 then start + xpathRound(args(2).number)
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
        val whole = args(0).text.s
        val from = args(1).text.s
        val to = args(2).text.s
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
        Value.Truth(args.head.truth)

      case "not" =>
        arity(1, 1)
        Value.Truth(!args.head.truth)

      case "true" =>
        arity(0, 0)
        Value.Truth(true)

      case "false" =>
        arity(0, 0)
        Value.Truth(false)

      case "lang" =>
        arity(1, 1)
        val wanted = args.head.text.s.toLowerCase.nn

        val declared = (context.locus :: ancestorLoci(context.locus)).flatMap: locus =>
          locus.subject match
            case element: Element if attributeIndexOf(locus) < 0 =>
              element.attributes.fetch(t"xml:lang") match
                case value: Text => List(value.s.toLowerCase.nn)
                case _           => Nil

            case _ =>
              Nil

        Value.Truth:
          declared.prim.let: language =>
            language == wanted || language.startsWith(wanted + "-")

          . or(false)

      case "number" =>
        arity(0, 1)
        Value.Numeric(defaulted.number)

      case "sum" =>
        arity(1, 1)
        var total = 0.0

        nodeSetArgument(args.head).foreach: locus =>
          total += XPath.parseNumber(locus.stringValue)

        Value.Numeric(total)

      case "floor" =>
        arity(1, 1)
        Value.Numeric(Math.floor(args.head.number))

      case "ceiling" =>
        arity(1, 1)
        Value.Numeric(Math.ceil(args.head.number))

      case "round" =>
        arity(1, 1)
        Value.Numeric(xpathRound(args.head.number))

      case _ =>
        abort(Error(Reason.UnknownFunction(name)))
