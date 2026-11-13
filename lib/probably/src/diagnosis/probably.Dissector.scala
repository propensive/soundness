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
package probably

import java.util.concurrent as juc

import scala.compiletime.*
import scala.quoted.*

import anticipation.*
import chiaroscuro.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

// The engine which diagnoses a failed assertion (see `Diagnosis`). Nothing here runs unless an
// assertion fails and the host asked for a diagnosis; then, once for each failing site:
//
//  1. the predicate's tree is read from the TASTy of the class which encloses it. TASTy is
//     pickled before inlining, so the predicate is there as it was written, fully typed, with
//     every given it uses already resolved;
//  2. a copy is instrumented: each subexpression is wrapped in a recorder, the predicate's
//     parameter becomes the value which failed, and each local it closes over becomes the
//     value the site captured;
//  3. that is compiled, by the staging compiler, in the suite's own classloader. Each recorder
//     is an INLINE method, so which typeclass renders a value — and any derivation it needs
//     — is resolved only now, for the types of this one predicate.
//
// The compiled diagnosis is kept, so a property which fails for many values is compiled once.
// It is applied to the value which actually failed; the test's body is not run again, and the
// predicate is evaluated once more, on the same value. A predicate that cannot be instrumented
// — it reaches something the generated code may not, or its instrumented copy does not
// compile — is diagnosed less finely rather than not at all: first without descending into
// the arguments of inline methods, then by presenting the failed value alone.
object Dissector extends Diagnostician:
  // What one evaluation of an instrumented predicate records: the rendered value of each
  // subexpression it evaluated, by the subexpression's number, and the comparison of the two
  // sides of an equality.
  final class Recorder():
    val values: juc.ConcurrentHashMap[Int, Text] = juc.ConcurrentHashMap()

    @volatile
    var comparison: Optional[Verdict.Detail] = Unset

    def note(id: Int, text: Text): Unit = values.put(id, text)

    def compare(expected: Text, found: Text, juxtaposition: Juxtaposition): Unit =
      comparison = Verdict.Detail.Compare(expected, found, juxtaposition)

  // The recorders are called from the staged code, and expand there.
  inline def inspect[value](recorder: Recorder, id: Int, value: value): value =
    recorder.note(id, inspection[value](value))
    value

  inline def show[value](recorder: Recorder, id: Int, value: value): value =
    recorder.note(id, showing[value](value))
    value

  // A value no instance can be resolved for — one whose type is too vague to choose between
  // them, such as an empty list's — prints itself.
  inline def inspection[value](value: value): Text = summonFrom:
    case inspectable: (`value` is Inspectable) => inspectable.text(value)
    case _                                     => java.lang.String.valueOf(value).nn.tt

  inline def showing[value](value: value): Text = summonFrom:
    case showable: (`value` is Showable) => showable.text(value)
    case _                               => inspection[value](value)

  inline def contrast[value](recorder: Recorder, expected: value, found: value): Unit =
    summonFrom:
      case contrastable: (`value` is Contrastable) =>
        recorder.compare
          ( inspection[value](expected),
            inspection[value](found),
            contrastable.juxtaposition(expected, found) )

      case _ =>
        ()

  // One recorded subexpression of a site: its number, its parent's (or -1), and its offsets
  // within the predicate's source.
  private case class Node(id: Int, parent: Int, start: Int, end: Int)

  // A site's compiled diagnosis: the function, the subexpressions it records, the names of the
  // captured locals it takes, in the order it takes them, and the number of the subexpression
  // which is the predicate's parameter — the value which failed.
  private case class Compiled
    ( function: (Any, scala.Array[Any], Recorder) => Boolean,
      nodes:    scala.List[Node],
      locals:   scala.List[String],
      subject:  Int )

  private class Unsupported(val reason: String) extends java.lang.RuntimeException(reason)

  // How finely a predicate is instrumented; each is tried in turn until one compiles.
  private enum Depth:
    case Full, Shallow, Value

  private val cache: juc.ConcurrentHashMap[(Text, Int, Int, Boolean, Boolean), Optional[Compiled]] =
    juc.ConcurrentHashMap()

  // `-Yretain-trees` is what makes the compiler keep the trees of the classes it loads from
  // TASTy: without it a class's `rootTree` is empty.
  // The rest are the options this build compiles with (see `diagnosis` in `build.mill`):
  // the inline methods a diagnosis expands were written for them.
  // The loader which defined this class is the suite's own (or one that sees everything it
  // sees), which is where the diagnosis must be compiled and loaded: the values it is applied
  // to are instances of that loader's classes.
  // A compiler which has failed to compile a diagnosis is not used again.
  private def makeCompiler(): staging.Compiler =
    staging.Compiler.make(getClass.getClassLoader.nn)
      ( using staging.Compiler.Settings.make
                (None, "-Yretain-trees" :: DissectorOptions.options) )

  private var compiler: staging.Compiler | Null = null

  // A diagnosis which cannot be made is not an error of the run, so its cause is discarded —
  // unless the `probably.diagnosis.debug` system property is set, when it is printed.
  private def discard(error: Throwable): Unit =
    if java.lang.System.getProperty("probably.diagnosis.debug") != null then error.printStackTrace()

  def diagnose
    ( site: Diagnosis.Site, value: Any, captures: List[(Text, Any)], request: Diagnosis.Request )
  :   List[Verdict.Detail] =

    if !site.known || request.nil then Nil else
      try
        val shown: Boolean =
          request.has(Diagnosis.Presentation.Show) && !request.has(Diagnosis.Presentation.Inspect)

        val contrasted: Boolean = request.has(Diagnosis.Presentation.Contrast)
        val key = (site.owner, site.start, site.end, shown, contrasted)

        val compiled: Optional[Compiled] = cache.get(key) match
          case null =>
            val made: Optional[Compiled] = synchronized(compile(site, shown, contrasted))
            cache.put(key, made)
            made

          case found =>
            found

        compiled.lay(Nil)(evaluate(site, _, value, captures, request))

      catch case error: Throwable =>
        discard(error)
        Nil

  private def evaluate
    ( site:     Diagnosis.Site,
      compiled: Compiled,
      value:    Any,
      captures: List[(Text, Any)],
      request:  Diagnosis.Request )
  :   List[Verdict.Detail] =

    val recorder: Recorder = Recorder()
    val available: java.util.HashMap[String, Any] = java.util.HashMap()
    captures.each: (name, value) => available.put(name.s, value)

    if !compiled.locals.forall(available.containsKey(_)) then Nil else
      val locals: scala.Array[Any] = compiled.locals.map(available.get(_)).toArray
      val root: Int = compiled.nodes.headOption.map(_.id).getOrElse(0)

      // Whether the predicate, evaluated again, did what it did the first time: fail, or throw.
      val reproducible: Boolean =
        try !compiled.function(value, locals, recorder)
        catch case error: Throwable =>
          recorder.values.put(-1, ("threw "+error).tt)
          true

      def depth(node: Node): Int =
        compiled.nodes.find(_.id == node.parent).map(depth(_) + 1).getOrElse(0)

      val source: String = site.source.s

      def code(node: Node): Text =
        if node.start >= 0 && node.end <= source.length && node.start < node.end
        then source.substring(node.start, node.end).nn.tt
        else "".tt

      val rows: scala.List[TestEvent.DissectRow] =
        compiled.nodes.flatMap: node =>
          recorder.values.get(node.id) match
            case null =>
              // The root, when the predicate threw before it had a value.
              if node.id == root && recorder.values.containsKey(-1) then
                scala.List:
                  TestEvent.DissectRow
                    ( 0, code(node), node.start, node.end, recorder.values.get(-1).nn, "thrown".tt )
              else scala.Nil

            // A subexpression which renders as it is written — a literal — says nothing.
            case text: Text =>
              if text == code(node) && node.id != root then scala.Nil else
                scala.List:
                  TestEvent.DissectRow
                    ( depth(node), code(node), node.start, node.end, text, "value".tt )

      val comparison: scala.List[Verdict.Detail] = recorder.comparison.lay(scala.Nil)(scala.List(_))

      // Without a dissection, and with no comparison to show it, the value which failed is
      // given on its own.
      val dissection: scala.List[Verdict.Detail] =
        if request.has(Diagnosis.Presentation.Dissect) && rows.length > 1
        then scala.List(Verdict.Detail.Dissection(site.source, List.from(rows), reproducible))
        else if !comparison.isEmpty then scala.Nil
        else recorder.values.get(compiled.subject) match
          case null       => scala.Nil
          case text: Text => scala.List(Verdict.Detail.Message(text))

      List.from(comparison ++ dissection)

  private def compile(site: Diagnosis.Site, shown: Boolean, contrasted: Boolean)
  :   Optional[Compiled] =

    def attempt(depth: Depth): Optional[Compiled] =
      try stage(site, shown, contrasted, depth)
      catch case error: Throwable =>
        discard(error)
        compiler = null

        depth match
          case Depth.Full    => attempt(Depth.Shallow)
          case Depth.Shallow => attempt(Depth.Value)
          case Depth.Value   => Unset

    attempt(Depth.Full)

  private def stage(site: Diagnosis.Site, shown: Boolean, contrasted: Boolean, depth: Depth)
  :   Compiled =

    given staging.Compiler = compiler match
      case null                       => makeCompiler().tap { made => compiler = made }
      case compiler: staging.Compiler => compiler

    var nodes: scala.List[Node] = scala.Nil
    var locals: scala.List[String] = scala.Nil
    var subject: Int = -1

    val function = staging.run[(Any, scala.Array[Any], Recorder) => Boolean]:
      import quotes.reflect.*

      nodes = scala.Nil
      locals = scala.Nil
      subject = -1

      val context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
      val owner: String = site.owner.s

      val ownerClass: Symbol =
        if !owner.endsWith("$") then Symbol.requiredClass(owner)
        else Symbol.requiredModule(owner.substring(0, owner.length - 1).nn).moduleClass

      // The public `Symbol#tree` of a class loaded from TASTy has no method bodies; the
      // compiler's own `rootTree` unpickles the whole class.
      val root: Tree =
        ownerClass.asInstanceOf[dotty.tools.dotc.core.Symbols.ClassSymbol].rootTree(using context)
        . asInstanceOf[Tree]

      def spanned(tree: Tree): Boolean =
        try tree.pos.start == site.start && tree.pos.end == site.end
        catch case error: Throwable => false

      var found: Option[(ValDef, Term)] = None

      // The trees which enclose the predicate, innermost first, once it is found.
      var enclosing: scala.List[Tree] = scala.Nil

      object finder extends TreeTraverser:
        private var path: scala.List[Tree] = scala.Nil

        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          if found.isEmpty then tree match
            case Block(scala.List(DefDef(_, scala.List(clause), _, Some(body))), Closure(_, _))
              if spanned(tree) =>

              clause match
                case TermParamClause(scala.List(parameter)) =>
                  found = Some((parameter, body))
                  enclosing = path

                case _ =>
                  ()

            case _ =>
              path = tree :: path
              try super.traverseTree(tree)(owner) catch case error: MatchError => ()
              path = path.drop(1)

      finder.traverseTree(root)(Symbol.spliceOwner)

      // The instances which render a value are searched for when the diagnosis is compiled,
      // from inside it — so what was in scope where the predicate is written has to be put
      // in scope there: every import which precedes it in a scope enclosing it, and the
      // givens of each object and package it is written in. Givens which are local to a
      // method, or members of a class rather than an object, cannot be reached and are not seen.
      def scope(owner: Symbol): scala.List[Statement] =
        import dotty.tools.dotc.ast.{tpd, untpd}
        import dotty.tools.dotc.core.StdNames.nme

        val context2 = context.withOwner(owner.asInstanceOf[dotty.tools.dotc.core.Symbols.Symbol])

        def before(tree: Tree): Boolean =
          try tree.pos.end <= site.start catch case error: Throwable => false

        // A wildcard selector is not replayed. It imports no givens, which are all that is
        // wanted here; what it does import are old-style implicits, and one of those — the
        // umbrella's diagnostic catch-all — outranks every instance in implicit scope and
        // cannot be expanded where a diagnosis is compiled.
        def imports(statements: scala.List[Tree]): scala.List[Statement] =
          statements.flatMap:
            case tree@Import(_, _) if before(tree) =>
              val original = tree.asInstanceOf[tpd.Import]

              val selectors = original.selectors.filter: selector =>
                selector.isGiven || !selector.isWildcard

              if selectors.isEmpty then scala.Nil else
                scala.List:
                  tpd.Import(original.expr, selectors)(using context2).asInstanceOf[Statement]

            case _ =>
              scala.Nil

        def givens(symbol: Symbol): scala.List[Statement] =
          val reference = Ref(symbol).asInstanceOf[tpd.Tree]
          given dotty.tools.dotc.util.SourceFile = context2.source
          val selector = untpd.ImportSelector(untpd.Ident(nme.EMPTY))
          val importing = tpd.Import(reference, scala.List(selector))(using context2)
          scala.List(importing.asInstanceOf[Statement])

        enclosing.reverse.flatMap:
          case tree@PackageClause(_, statements) =>
            givens(tree.symbol) ++ imports(statements)

          case tree@ClassDef(_, _, _, _, statements) =>
            val own: scala.List[Statement] =
              if tree.symbol.flags.is(Flags.Module) then givens(tree.symbol.companionModule)
              else scala.Nil

            own ++ imports(statements)

          case Block(statements, _) =>
            imports(statements)

          case _ =>
            scala.Nil

      val (parameter, body) = found.getOrElse(throw Unsupported("the predicate was not found"))
      val parameterSymbol: Symbol = parameter.symbol

      def within(symbol: Symbol): Boolean = symbol.pos.exists: position =>
        position.start >= site.start && position.end <= site.end

      def local(symbol: Symbol): Boolean =
        symbol.isTerm && !symbol.maybeOwner.isNoSymbol &&
          (symbol.maybeOwner.isDefDef || symbol.maybeOwner.isValDef)

      def methodic(tpe: TypeRepr): Boolean = tpe.widen match
        case MethodType(_, _, _) => true
        case PolyType(_, _, _)   => true
        case _                   => false

      // A type with `Nothing` in it — an empty list's — is one for which the search for an
      // instance is ambiguous, and an ambiguity is an error, not a fallback.
      def vague(tpe: TypeRepr): Boolean = tpe.dealias match
        case AppliedType(_, arguments) => arguments.exists(vague(_))
        case other                     => other =:= TypeRepr.of[Nothing]

      def recordable(term: Term): Boolean =
        !methodic(term.tpe) && !(term.tpe.widen =:= TypeRepr.of[Unit]) &&
          !vague(term.tpe.widen) && !term.symbol.flags.is(Flags.Module) &&
          !term.symbol.flags.is(Flags.Package)

      var counter: Int = 0

      def fresh(parent: Int, term: Tree): Int =
        val id = counter
        counter += 1

        val (start, end) =
          try (term.pos.start - site.start, term.pos.end - site.start)
          catch case error: Throwable => (-1, -1)

        nodes = Node(id, parent, start, end) :: nodes
        id

      ' {
          (value: Any, captures: scala.Array[Any], recorder: Recorder) =>
            $ {
                def record(id: Int, term: Term): Term = term.tpe.widen.asType match
                  case '[t] =>
                    val expr = term.asExprOf[t]

                    if shown then '{Dissector.show[t](recorder, ${Expr(id)}, $expr)}.asTerm
                    else '{Dissector.inspect[t](recorder, ${Expr(id)}, $expr)}.asTerm

                def live(term: Term): Term = term.tpe.widen.asType match
                  case '[t] => '{value.asInstanceOf[t]}.asTerm

                def captured(term: Term): Term =
                  val name: String = term.symbol.name
                  if !locals.contains(name) then locals = locals :+ name
                  val index: Expr[Int] = Expr(locals.indexOf(name))

                  term.tpe.widen.asType match
                    case '[t] => '{captures($index).asInstanceOf[t]}.asTerm

                def accessible(symbol: Symbol): Boolean =
                  !symbol.flags.is(Flags.Private) && !symbol.flags.is(Flags.Protected)

                class Instrument(parent: Int) extends TreeMap:
                  def descend(id: Int, term: Term, owner: Symbol): Term = term match
                    case Apply(function, arguments) =>
                      val inlined: Boolean = function.symbol.flags.is(Flags.Inline)

                      val byName: scala.List[Boolean] = function.tpe.widen match
                        case MethodType(_, types, _) =>
                          types.map:
                            case ByNameType(_) => true
                            case _             => false

                        case _ =>
                          arguments.map(_ => false)

                      // The receiver is evaluated before the arguments, and numbered before them.
                      val instrument = Instrument(id)
                      val function2: Term = instrument.transformTerm(function)(owner)

                      val arguments2: scala.List[Term] =
                        if inlined && depth != Depth.Full then arguments
                        else
                          arguments.zip(byName.padTo(arguments.length, false)).map:
                            (argument, lazily) =>
                              if lazily then argument
                              else instrument.transformTerm(argument)(owner)

                      Apply(function2, arguments2)

                    case _ =>
                      Instrument(id).descendInto(term, owner)

                  def descendInto(term: Term, owner: Symbol): Term =
                    super.transformTerm(term)(owner)

                  override def transformTerm(term: Term)(owner: Symbol): Term = term match
                    case Literal(_) =>
                      term

                    case New(_) =>
                      term

                    // What a transparent inline method or a macro expanded to is already in
                    // the tree, positioned in the method's own source: it is recorded whole,
                    // as the call that was written.
                    case Inlined(Some(_), _, _) =>
                      if recordable(term) then record(fresh(parent, term), term) else term

                    case Block(scala.List(DefDef(_, _, _, _)), Closure(_, _)) =>
                      term

                    case This(_) =>
                      if term.symbol.flags.is(Flags.Module) then Ref(term.symbol.companionModule)
                      else throw Unsupported("the predicate refers to an enclosing instance")

                    case Ident(_) if term.symbol == parameterSymbol =>
                      if depth == Depth.Value then live(term) else
                        val id = fresh(parent, term)
                        if subject < 0 then subject = id
                        record(id, live(term))

                    case Ident(_) if local(term.symbol) && !within(term.symbol) =>
                      if term.symbol.flags.is(Flags.Method)
                      then throw Unsupported("the predicate calls a local method")
                      else record(fresh(parent, term), captured(term))

                    case Ident(_) | Select(_, _) if !accessible(term.symbol) =>
                      throw Unsupported("the predicate uses an inaccessible member")

                    case Ident(_) =>
                      if recordable(term) && term.symbol.isTerm && !within(term.symbol)
                      then record(fresh(parent, term), term)
                      else term

                    case Apply(_, _) | TypeApply(_, _) | Select(_, _) if recordable(term) =>
                      val id = fresh(parent, term)
                      record(id, descend(id, term, owner))

                    case _ =>
                      super.transformTerm(term)(owner)

                val body2: Term = body.changeOwner(Symbol.spliceOwner)

                def unwrap(term: Term): Term = term match
                  case Inlined(_, scala.Nil, term)        => unwrap(term)
                  case Typed(term, _)                     => unwrap(term)
                  case Block(scala.Nil, term)             => unwrap(term)
                  case term                               => term

                val instrumented: Term =
                  if depth == Depth.Value then
                    val id = fresh(-1, parameter)
                    subject = id
                    val identity: Term = Ref(parameterSymbol)
                    Block(scala.List(record(id, live(identity))), Literal(BooleanConstant(false)))
                  else unwrap(body2) match
                    // An equality is evaluated one side at a time, so that the two sides can be
                    // contrasted, as values of the type of whichever side is the wider.
                    case equality@Apply(select@Select(left, "=="), scala.List(right))
                      if contrasted =>

                      val id = fresh(-1, equality)
                      val instrument = Instrument(id)
                      val left2 = instrument.transformTerm(left)(Symbol.spliceOwner)
                      val right2 = instrument.transformTerm(right)(Symbol.spliceOwner)
                      val reversed: Boolean = right.symbol == parameterSymbol
                      val leftType = left.tpe.widen
                      val rightType = right.tpe.widen

                      (leftType.asType, rightType.asType) match
                        case ('[l], '[r]) =>
                          val result: Expr[Boolean] =
                            ' {
                                val leftValue: l = ${left2.asExprOf[l]}
                                val rightValue: r = ${right2.asExprOf[r]}

                                $ {
                                    // The side which is not the predicate's parameter is the
                                    // expected one; the comparison is made at the wider type.
                                    def sides[t: Type](left: Expr[t], right: Expr[t]): Expr[Unit] =
                                      if reversed
                                      then '{Dissector.contrast[t](recorder, $left, $right)}
                                      else '{Dissector.contrast[t](recorder, $right, $left)}

                                    val contrast: Expr[Unit] =
                                      if rightType <:< leftType
                                      then sides[l]('leftValue, '{rightValue.asInstanceOf[l]})
                                      else if leftType <:< rightType
                                      then sides[r]('{leftValue.asInstanceOf[r]}, 'rightValue)
                                      else '{()}

                                    val test: Expr[Boolean] =
                                      Apply
                                        ( Select('leftValue.asTerm, select.symbol),
                                          scala.List('rightValue.asTerm) )
                                      . asExprOf[Boolean]

                                    '{$contrast; $test}
                                  }
                              }

                          record(id, result.asTerm)

                    case other =>
                      Instrument(-1).transformTerm(other)(Symbol.spliceOwner)

                Block(scope(Symbol.spliceOwner), instrumented).asExprOf[Boolean]
              }
        }

    Compiled(function, nodes.reverse, locals, subject)
