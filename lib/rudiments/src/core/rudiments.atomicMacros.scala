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
package rudiments

import java.util.concurrent.atomic as juca

import scala.collection.immutable as sci
import scala.quoted.*

import fulminate.*
import gigantism.*

// Shape recognition for `Atomic`'s `ere`/`since` transitions.
//
// A transition is written as a lambda literal at the call site and is never allocated: this
// object either replaces it with the JDK intrinsic that performs it in one instruction, or
// beta-reduces it into the body of a compare-and-set retry loop. Nothing reaches runtime as a
// `java.util.function.UnaryOperator`, which is what the four hand-hoisted operator values in
// `parasite.Promise` exist to avoid.
//
// The retry loop may re-run the transition under contention, so a transition must be pure. That
// requirement cannot be *proved* here — no purity oracle exists in `quotes.reflect` — but it
// can be narrowed to shapes for which it is evident, and it is: `accepts` admits a closed set,
// and
// everything else is directed to `revise`, whose signature documents the obligation. The
// load-bearing line is that a method call is admitted only when its RECEIVER is derived from the
// transitioned value (`waiting + strand`, where `waiting` was bound out of it), and never when
// it applies a free function to it (`combine(current, error)`, which could do anything). The
// latter is exactly `contingency.Accrual`'s transition, and exactly the case `revise` exists for.
object atomicMacros:
  // What the transition does, as far as it can be recognised. `General` is admitted but has no
  // intrinsic, so it becomes a retry loop.
  private enum Shape:
    case Identity, Constant, Increase, Decrease, General

  private def isParam(using Quotes)(term: quotes.reflect.Term, symbol: quotes.reflect.Symbol)
  :   Boolean =

    import quotes.reflect.*

    term match
      case ident: Ident => ident.symbol == symbol
      case _            => false

  private def literalOne(using Quotes)(term: quotes.reflect.Term): Boolean =
    import quotes.reflect.*

    term match
      case Literal(IntConstant(1))  => true
      case Literal(LongConstant(1)) => true
      case _                        => false

  private def analyse(using Quotes)(transition: Expr[Any], numeric: Boolean)
  :   (Shape, Option[quotes.reflect.Term]) =

    import quotes.reflect.*

    def strip(term: Term): Term = term match
      case Inlined(_, _, inner) => strip(inner)
      case Block(Nil, inner)    => strip(inner)
      case Typed(inner, _)      => strip(inner)
      case other                => other

    // `params` is destructured with `.head` rather than by a `List(param)` pattern: `List` is
    // `proscenium.List` under this module's prelude, and its extractor does not type the stdlib
    // list that `quotes.reflect` hands back.
    strip(transition.asTerm) match
      case Lambda(params, body) if params.length == 1 =>
        val symbol = params.head.symbol

        // Whether a tree mentions any of `roots` — the transitioned value, and anything bound
        // out of it by a pattern.
        def mentions(term: Tree, roots: sci.Set[Symbol]): Boolean =
          var found = false

          val traverser = new TreeTraverser:
            override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
              case ident: Ident if roots.contains(ident.symbol) => found = true
              case _                                            => traverseTreeChildren(tree)(owner)

          traverser.traverseTree(term)(symbol.owner)

          found

        // The symbols a pattern binds, which are derived from whatever it destructures.
        def bindings(pattern: Tree): sci.Set[Symbol] =
          var found = sci.Set.empty[Symbol]

          val traverser = new TreeTraverser:
            override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
              case bind: Bind => found = found + bind.symbol; traverseTreeChildren(tree)(owner)
              case _          => traverseTreeChildren(tree)(owner)

          traverser.traverseTree(pattern)(symbol.owner)

          found

        // Whether a tree is one of the shapes for which re-running is evidently harmless.
        // Deliberately closed: anything not listed is refused rather than assumed pure.
        def accepts(term: Term, roots: sci.Set[Symbol]): Boolean = strip(term) match
          case _: Ident   => true
          case _: Literal => true

          case Select(receiver, _) => accepts(receiver, roots)

          case If(cond, yes, no) =>
            accepts(cond, roots) && accepts(yes, roots) && accepts(no, roots)

          // Destructuring the transitioned value makes what the patterns bind derived from it,
          // so `case Incomplete(waiting) => Incomplete(waiting + strand)` may call a method on
          // `waiting`.
          case Match(scrutinee, cases) =>
            accepts(scrutinee, roots) && cases.forall: caseDef =>
              val inner =
                if mentions(scrutinee, roots) then roots ++ bindings(caseDef.pattern) else roots

              caseDef.guard.forall(accepts(_, inner)) && accepts(caseDef.rhs, inner)

          // Every application. What is refused is applying a FUNCTION VALUE to the
          // transitioned value: a `Function1` arriving from outside, whose `apply` is virtual
          // and whose behaviour therefore cannot be read at all. That is exactly
          // `contingency.Accrual`'s `combine(current, error)`, and exactly what `revise`
          // exists for.
          //
          // Everything else — construction, methods, extension methods — is admitted. The line
          // is drawn at identifiability, not at purity, and the distinction is worth being
          // plain about: a statically-resolved method CAN still be impure, so this is a
          // hygiene filter and not a proof. What it reliably catches is behaviour smuggled in
          // from outside the expression, which is the case a reader cannot check either.
          //
          // Extension methods must be admitted, not incidentally but centrally: `t.lower` and
          // `waiting + strand` both desugar to a call taking the value as the first argument,
          // indistinguishable in shape from `identity(x)`. Refusing them would refuse most of
          // the idiom this collection is written in.
          case applied @ Apply(function, args) =>
            val method = applied.symbol

            val opaqueFunction =
              method.name == "apply" && method.owner.fullName.startsWith("scala.Function")

            val receiver = function match
              case Select(New(_), _)              => None
              case Select(inner, _)               => Some(inner)
              case TypeApply(Select(inner, _), _) => Some(inner)
              case _                              => None

            if opaqueFunction && args.exists(mentions(_, roots)) then false
            else receiver.forall(accepts(_, roots)) && args.forall(accepts(_, roots))

          // An `inline` extension expands BEFORE this macro sees it, so an ordinary transition
          // such as `_.lower` arrives already rewritten into `text.map(x)(closure)` — a block
          // holding a local `def` and a `Closure`. Refusing that shape would refuse most of the
          // collection's own vocabulary, so the residue of inlining is admitted and its bodies
          // are checked instead. A closure LITERAL is not the thing being guarded against; an
          // opaque function VALUE arriving from outside is, and that check is above.
          case Block(statements, expr) =>
            val bodies = statements.forall:
              case defdef: DefDef => defdef.rhs.forall(accepts(_, roots))
              case valdef: ValDef => valdef.rhs.forall(accepts(_, roots))
              case term: Term     => accepts(term, roots)
              case _              => false

            bodies && accepts(expr, roots)

          case _: Closure => true

          case _ => false

        val roots = sci.Set(symbol)

        val shape =
          if !mentions(body, roots) then Shape.Constant else strip(body) match
            case ident: Ident if ident.symbol == symbol => Shape.Identity

            case Apply(Select(left, "+"), args)
            if numeric && args.length == 1 && isParam(left, symbol)
            && !mentions(args.head, roots) =>
              Shape.Increase

            case Apply(Select(left, "-"), args)
            if numeric && args.length == 1 && isParam(left, symbol)
            && !mentions(args.head, roots) =>
              Shape.Decrease

            case other =>
              if accepts(other, roots) then Shape.General else halt:
                m"""
                  this transition cannot be read here, most often because it applies a function
                  value obtained from outside; a transition may be re-run under contention, so it
                  has to be pure, and that cannot be taken on trust from an expression whose
                  behaviour is not visible. Use `revise`, which takes a function value and states
                  the obligation in its signature.
                """

        val operand = shape match
          case Shape.Constant =>
            Some(strip(body))

          case Shape.Increase | Shape.Decrease =>
            strip(body) match
              case Apply(Select(_, _), args) if args.length == 1 => Some(args.head)
              case _                                             => None

          case _ =>
            None

        (shape, operand)

      case _ =>
        halt:
          m"""
            an atomic transition must be written as a lambda literal, so that its shape can be
            recognised and it need never be allocated
          """

  def count
     ( atomic:     Expr[juca.AtomicInteger],
       transition: Expr[Int => Int],
       prior:      Boolean )
  :   Macro[Int] =

    import quotes.reflect.*

    analyse(transition, numeric = true) match
      case (Shape.Identity, _) =>
        '{$atomic.get()}

      case (Shape.Constant, Some(operand)) =>
        val value = operand.asExprOf[Int]
        if prior then '{$atomic.getAndSet($value)} else '{$atomic.set($value); $value}

      case (Shape.Increase, Some(operand)) =>
        val value = operand.asExprOf[Int]

        if literalOne(operand)
        then if prior then '{$atomic.getAndIncrement()} else '{$atomic.incrementAndGet()}
        else if prior then '{$atomic.getAndAdd($value)} else '{$atomic.addAndGet($value)}

      case (Shape.Decrease, Some(operand)) =>
        val value = operand.asExprOf[Int]

        if literalOne(operand)
        then if prior then '{$atomic.getAndDecrement()} else '{$atomic.decrementAndGet()}
        else if prior then '{$atomic.getAndAdd(-$value)} else '{$atomic.addAndGet(-$value)}

      case _ =>
        ' {
            val cell = $atomic
            var current: Int = cell.get()
            var settled: Boolean = false
            var answer: Int = current

            while !settled do
              val next: Int = ${Expr.betaReduce('{$transition(current)})}

              // A declined transition writes nothing: no compare-and-set, and so no transfer of
              // the cache line. Java's `updateAndGet` issues one even for the identity.
              if next == current then
                answer = current
                settled = true
              else if cell.compareAndSet(current, next) then
                answer = if ${Expr(prior)} then current else next
                settled = true
              else
                current = cell.get()

            answer
          }

  def tally
     ( atomic:     Expr[juca.AtomicLong],
       transition: Expr[Long => Long],
       prior:      Boolean )
  :   Macro[Long] =

    import quotes.reflect.*

    analyse(transition, numeric = true) match
      case (Shape.Identity, _) =>
        '{$atomic.get()}

      case (Shape.Constant, Some(operand)) =>
        val value = operand.asExprOf[Long]
        if prior then '{$atomic.getAndSet($value)} else '{$atomic.set($value); $value}

      case (Shape.Increase, Some(operand)) =>
        val value = operand.asExprOf[Long]

        if literalOne(operand)
        then if prior then '{$atomic.getAndIncrement()} else '{$atomic.incrementAndGet()}
        else if prior then '{$atomic.getAndAdd($value)} else '{$atomic.addAndGet($value)}

      case (Shape.Decrease, Some(operand)) =>
        val value = operand.asExprOf[Long]

        if literalOne(operand)
        then if prior then '{$atomic.getAndDecrement()} else '{$atomic.decrementAndGet()}
        else if prior then '{$atomic.getAndAdd(-$value)} else '{$atomic.addAndGet(-$value)}

      case _ =>
        ' {
            val cell = $atomic
            var current: Long = cell.get()
            var settled: Boolean = false
            var answer: Long = current

            while !settled do
              val next: Long = ${Expr.betaReduce('{$transition(current)})}

              if next == current then
                answer = current
                settled = true
              else if cell.compareAndSet(current, next) then
                answer = if ${Expr(prior)} then current else next
                settled = true
              else
                current = cell.get()

            answer
          }

  // A flag has no arithmetic, so only the identity and constant shapes have an intrinsic; `!_`
  // and anything else become the retry loop.
  def flag
     ( atomic:     Expr[juca.AtomicBoolean],
       transition: Expr[Boolean => Boolean],
       prior:      Boolean )
  :   Macro[Boolean] =

    import quotes.reflect.*

    analyse(transition, numeric = false) match
      case (Shape.Identity, _) =>
        '{$atomic.get()}

      case (Shape.Constant, Some(operand)) =>
        val value = operand.asExprOf[Boolean]
        if prior then '{$atomic.getAndSet($value)} else '{$atomic.set($value); $value}

      case _ =>
        ' {
            val cell = $atomic
            var current: Boolean = cell.get()
            var settled: Boolean = false
            var answer: Boolean = current

            while !settled do
              val next: Boolean = ${Expr.betaReduce('{$transition(current)})}

              if next == current then
                answer = current
                settled = true
              else if cell.compareAndSet(current, next) then
                answer = if ${Expr(prior)} then current else next
                settled = true
              else
                current = cell.get()

            answer
          }

  // A reference cell declines by REFERENCE identity (`eq`), not `==`: a transition returning an
  // equal-but-distinct value is a genuine write, and a value type may define `==` expensively or
  // inconsistently. `Cell` is the one place where the two differ.
  def cell[value: Type]
     ( atomic:     Expr[juca.AtomicReference[value]],
       transition: Expr[value => value],
       prior:      Boolean )
  :   Macro[value] =

    import quotes.reflect.*

    analyse(transition, numeric = false) match
      case (Shape.Identity, _) =>
        '{$atomic.get().asInstanceOf[value]}

      case (Shape.Constant, Some(operand)) =>
        val supplied = operand.asExprOf[value]

        if prior then '{$atomic.getAndSet($supplied).asInstanceOf[value]}
        else '{$atomic.set($supplied); $supplied}

      case _ =>
        ' {
            val box = $atomic
            var current: value = box.get().asInstanceOf[value]
            var settled: Boolean = false
            var answer: value = current

            while !settled do
              val next: value = ${Expr.betaReduce('{$transition(current)})}

              if next.asInstanceOf[AnyRef] eq current.asInstanceOf[AnyRef] then
                answer = current
                settled = true
              else if box.compareAndSet(current, next) then
                answer = if ${Expr(prior)} then current else next
                settled = true
              else
                current = box.get().asInstanceOf[value]

            answer
          }
