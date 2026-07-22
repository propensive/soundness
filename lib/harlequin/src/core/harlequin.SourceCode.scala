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
package harlequin

import scala.collection.mutable as scm

import dotty.tools.dotc.*, core.*, parsing.*, util.*
import dotty.tools.dotc.core.NameOps.isVarPattern
import dotty.tools.dotc.reporting.Diagnostic as CompilerDiagnostic
import dotty.tools.dotc.reporting.HideNonSensicalMessages
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.reporting.UniqueMessagePositions

import anthology.*
import anticipation.*
import denominative.*
import gossamer.*
import hellenism.*
import rudiments.*
import stenography.*
import vacuous.*

object SourceCode:
  private def accent(token: Int): Accent =
    if token <= 2 then Accent.Error
    else if token == 3 || token == 10 || token == 11 then Accent.String
    else if token >= 4 && token <= 9 || token == 23 || token == 24 || token == 42 || token == 43
    then Accent.Number
    else if token == 14 then Accent.Term
    else if token >= 20 && token <= 62 && Tokens.modifierTokens.has(token) then Accent.Modifier
    else if token >= 20 && token <= 66 then Accent.Keyword
    else if token >= 78 && token <= 99 then Accent.Symbol
    else Accent.Parens

  // This list was found in scala/scala3:compiler/src/dotty/tools/dotc/core/parsing/Parsers.scala
  private val soft: Set[Text] =
    Set(t"inline", t"opaque", t"open", t"transparent", t"infix", t"update", t"erased", t"tracked")

  def apply(language: ProgrammingLanguage, text: Text, caret: Optional[Ordinal])
    ( using highlighting: Highlight )
  :   SourceCode =

    // For the typechecked/compiled pipelines, run the compiler frontend (and, for
    // `Compiled`, the post-typer phases) to resolve types, collect diagnostics, and
    // — when a caret is given — compute completions. Types are always read from the
    // typer-stopped run, so they stay source-level.
    val resolved
    :   Optional[(Map[(Int, Int), Syntax], List[Diagnostic], Optional[Completions])] =

      if highlighting.depth == Depth.Tokenized then Unset else
        (highlighting.scalac, highlighting.classpath) match
          case (scalac: Scalac[?, ?], classpath: LocalClasspath) =>
            resolveTypes
              ( text, scalac, classpath, highlighting.depth == Depth.Compiled, caret )

          case _ =>
            Unset

    val metaMap: Map[(Int, Int), Syntax] = resolved.lay(Map())(_(0))
    val diagnostics: List[Diagnostic] = resolved.lay(Nil)(_(1))
    val resolvedCompletions: Optional[Completions] = resolved.lay(Unset)(_(2))

    // Keyword completions come from the curated pattern tree over the reversed lexeme
    // context at the caret — no compiler run, so they are offered in every depth, including
    // `Tokenized` (where they are the only completions). In a binding position (a fresh name
    // is expected) the resolved member completions are deliberately dropped.
    val completions: Optional[Completions] =
      if language != Scala then resolvedCompletions else
        caret.lay(resolvedCompletions): caret =>
          val (prefix, context) = Lexis.context(text, caret)
          val found = prophesy.ScalaKeywords.pattern(context)

          val words = prefix.lay(found.keywords): p =>
            found.keywords.filter(_.starts(p))

          val prefixLength = prefix.lay(0)(_.length)
          val replace = Span.offset((caret.n0 - prefixLength).z, prefixLength)

          val items = words.to(List).sortBy(_.s).map: word =>
            Completion(word, Completion.Kind.Keyword, Syntax.Symbolic(word))

          val binding =
            found.expectation == prophesy.KeywordPattern.Expectation.TermBinding ||
              found.expectation == prophesy.KeywordPattern.Expectation.TypeBinding

          val typal =
            found.expectation == prophesy.KeywordPattern.Expectation.TypeIdentifier

          // In a pure type position, only type-shaped resolved items (and the packages and
          // modules that prefix them) are offered.
          val resolvedItems: List[Completion] =
            if typal then
              resolvedCompletions.lay(Nil): resolved =>
                resolved.items.filter: item =>
                  item.kind == Completion.Kind.Type || item.kind == Completion.Kind.Module ||
                    item.kind == Completion.Kind.Package
            else
              resolvedCompletions.lay(Nil)(_.items)

          val merged = items ::: resolvedItems

          if binding then Completions(replace, items)
          else if merged.isEmpty then resolvedCompletions
          else Completions(replace, merged)

    val source: SourceFile = SourceFile.virtual("<highlighting>", text.s)
    val context0 = Contexts.ContextBase().initialCtx.fresh.setReporter(Reporter.NoReporter)

    context0.setSetting(context0.settings.source, "future")

    given context: Contexts.Context =
      context0.setCompilationUnit(CompilationUnit(source, mustExist = false)(using context0))

    val trees = Trees()
    val parser = Parsers.Parser(source)

    trees.traverse(parser.compilationUnit())

    val scanner =
      if language == Java then JavaScanners.JavaScanner(source) else Scanners.Scanner(source)

    def untab(text: Text): LazyList[Token] =
      LazyList(Token(text.sub(t"\t", t"  "), Accent.Unparsed), Token.Newline)

    def hard(stream: LazyList[Token]): Boolean = stream match
      case Token(_, Accent.Unparsed, _, _, _) #:: more                  => hard(more)
      case Token(text, Accent.Term, _, _, _) #:: more if soft.has(text) => hard(more)
      case Token(text, Accent.Keyword | Accent.Modifier, _, _, _) #:: _ => true
      case other                                                        => false

    def soften(stream: LazyList[Token]): LazyList[Token] = stream match
      case (Token(text@(t"using" | t"erased"), Accent.Term, _, _, _)) #:: more =>
        Token(text, Accent.Modifier) #:: soften(more)

      case (token@Token(text, Accent.Term, _, _, _)) #:: more if soft.has(text) =>
        if hard(more) then Token(text, Accent.Modifier) #:: soften(more)
        else token #:: soften(more)

      case token #:: more =>
        token #:: soften(more)

      case _ =>
        LazyList()

    def stream(lastEnd: Int = 0): LazyList[Token] = scanner.token match
      case Tokens.EOF => untab(text.segment(lastEnd.z till text.limit)).filter(_.length > 0)

      case token =>
        val start = scanner.offset max lastEnd

        val unparsed: LazyList[Token] =
          if lastEnd == start then LazyList() else
            text.segment(lastEnd.z thru start.u)
            . cut(t"\n")
            . to(LazyList)
            . flatMap(untab(_).filter(_.length > 0))
            . init

        scanner.nextToken()
        val end = scanner.lastOffset max start

        val meta: Optional[Token.Meta] = metaMap.get((start, end)) match
          case Some(syntax) => Token.Meta(syntax)
          case None         => Unset

        val annotation: Optional[TokenTag] = trees(start, end)
        val tokenAccent: Accent = annotation.lay(accent(token))(_.accent)
        val role: Optional[Role] = annotation.let(_.role)

        val content: LazyList[Token] =
          if start == end then LazyList() else
            text.segment(start.z thru end.u).cut(t"\n").to(LazyList).flatMap: line =>
              LazyList(Token(line, tokenAccent, meta, role = role), Token.Newline)

            . init

        unparsed #::: content #::: stream(end)


    def lines(sequence: List[Token], acc: List[List[Token]] = Nil)
    :   List[List[Token]] =

      sequence match
        case Nil => acc

        case xs =>
          xs.indexOf(Token.Newline) match
            case -1    => xs :: acc
            case index => lines(xs.drop(index + 1), xs.take(index) :: acc)

    def quoted(text: Text): Boolean =
      text.length > 0 &&
        { val first = text.s.charAt(0); first == '"' || first == '\'' || first == '`' }

    // Recovering from an unterminated quoted literal at the end of the input, the
    // Scala scanner spills the literal's final character into a separate token — so
    // `"ab` lexes as an error token `"a` followed by an identifier `b`. Fold each
    // such trailing run back into the error token, so an in-progress string (or char
    // or backquoted identifier) highlights as one consistent unit rather than having
    // its last character mis-coloured.
    @annotation.tailrec
    def coalesce(sequence: List[Token], done: List[Token] = Nil): List[Token] = sequence match
      case Nil => done.reverse

      case head :: tail if head.accent == Accent.Error && quoted(head.text) =>
        val (spill, rest) = tail.span(_.accent != Accent.Unparsed)

        if spill.nil then coalesce(tail, head :: done)
        else coalesce(rest, head.copy(text = t"${head.text}${spill.map(_.text).join}") :: done)

      case head :: tail =>
        coalesce(tail, head :: done)

    val tokens: List[Token] = coalesce(soften(stream()).to(List))

    // Give each token a `Line`-mode `Span` with its 0-based line and column,
    // accumulating token widths along each assembled line.
    val positioned = lines(tokens).reverse.zipWithIndex.map: (tokens, index) =>
      tokens.zip(tokens.scanLeft(0)(_ + _.length)).map: (token, column) =>
        token.copy(span = Span.line(index.z, column.z, token.length))

    SourceCode
      ( language, 1, IArray(positioned*), diagnostics = diagnostics, completions = completions )

  // The accent (colour category) and role (binding vs usage) resolved for a token span
  // from the parse tree.
  private case class TokenTag(accent: Accent, role: Role)

  private class Trees() extends ast.untpd.UntypedTreeTraverser:
    import ast.*, untpd.*

    private val trees: scm.HashMap[(Int, Int), TokenTag] = scm.HashMap()

    def apply(start: Int, end: Int): Optional[TokenTag] = trees.get((start, end)) match
      case Some(tag) => tag
      case None      => Unset

    private def tag(span: Spans.Span, accent: Accent, role: Role): Unit =
      if span.exists then trees += (span.start, span.end) -> TokenTag(accent, role)

    def ignored(tree: NameTree): Boolean =
      val name = tree.name.toTermName
      name == StdNames.nme.ERROR || name == StdNames.nme.CONSTRUCTOR

    // An `Ident` reached in pattern position that genuinely *binds* a name: a simple
    // lowercase-led var-pattern name, not the `_` wildcard, not a backquoted
    // stable-identifier usage (`` case `x` => ``), and not a type.
    private def isBinderIdent(tree: Ident): Boolean =
      tree.name != StdNames.nme.WILDCARD && tree.name.isVarPattern && !tree.isBackquoted &&
        !tree.isType

    // Walk a subtree known to be in *pattern* position. Simple var-pattern binders and
    // `Bind` names are term bindings; everything else — extractor and stable-id usages,
    // literals, type ascriptions — routes back through the ordinary traversal so it keeps
    // its usage role.
    private def traversePattern(tree: Tree)(using Contexts.Context): Unit = tree match
      case tree: Ident if isBinderIdent(tree) =>
        tag(tree.span, Accent.Term, Role.Binding)

      case tree: Bind =>
        tag(tree.nameSpan, Accent.Term, Role.Binding)
        traversePattern(tree.body)

      case Typed(expr, tpt) =>
        traversePattern(expr)
        traverse(tpt)

      case Parens(pattern) =>
        traversePattern(pattern)

      case Tuple(elements) =>
        elements.foreach(traversePattern)

      case Alternative(alternatives) =>
        alternatives.foreach(traversePattern)

      case Apply(function, arguments) =>
        traverse(function)
        arguments.foreach(traversePattern)

      case TypeApply(function, arguments) =>
        traverse(function)
        arguments.foreach(traverse)

      case NamedArg(_, pattern) =>
        traversePattern(pattern)

      case other =>
        traverse(other)

    def traverse(tree: Tree)(using Contexts.Context): Unit =
      tree match
        case tree: NameTree if ignored(tree) => traverseChildren(tree)

        case tree: TypeDef if tree.mods.is(Flags.Param) =>
          tag(tree.nameSpan, Accent.Typal, Role.Binding)
          traverseChildren(tree)

        case tree: ValOrDefDef =>
          tag(tree.nameSpan, Accent.Term, Role.Binding)
          traverseChildren(tree)

        case tree: MemberDef =>
          tag(tree.nameSpan, Accent.Typal, Role.Binding)
          traverseChildren(tree)

        case tree: Ident if tree.isType =>
          tag(tree.span, Accent.Typal, Role.Usage)
          traverseChildren(tree)

        case tree: Ident =>
          tag(tree.span, Accent.Term, Role.Usage)
          traverseChildren(tree)

        case tree: TypTree =>
          tag(tree.span, Accent.Typal, Role.Usage)
          traverseChildren(tree)

        case CaseDef(pattern, guard, body) =>
          traversePattern(pattern)
          traverse(guard)
          traverse(body)

        case GenFrom(pattern, expr, _) =>
          traversePattern(pattern)
          traverse(expr)

        case GenAlias(pattern, expr) =>
          traversePattern(pattern)
          traverse(expr)

        case tree: PatDef =>
          tree.pats.foreach(traversePattern)
          traverse(tree.tpt)
          traverse(tree.rhs)

        case other =>
          traverseChildren(tree)

  // Render a classpath to the `-classpath` argument string without needing an
  // ambient `System` capability (we only have directory and jar entries here).
  private def classpathText(classpath: LocalClasspath): Text =
    classpath.entries.flatMap:
      case ClasspathEntry.Directory(directory) => List(directory)
      case ClasspathEntry.Jar(jar)             => List(jar)
      case _                                   => Nil

    . join(java.io.File.pathSeparator.nn.tt)

  private def resolveTypes
    ( text:      Text,
      scalac:    Scalac[?, ?],
      classpath: LocalClasspath,
      full:      Boolean,
      caret:     Optional[Ordinal] )
  :   (Map[(Int, Int), Syntax], List[Diagnostic], Optional[Completions]) =

    val cp = classpathText(classpath)

    // Types always come from a typer-stopped run, so they are source-level even
    // in `Compiled` mode (later phases such as erasure would rewrite them).
    val (typerRun, _, typerDiagnostics) =
      frontend(text, scalac, cp): context =>
        context.setSetting(context.settings.YstopAfter, List("typer"))

    val metaMap = collectTypes(typerRun)

    val completions = caret.let: caret =>
      // The interactive driver is the primary completion source: unlike the batch typer run,
      // it retains error trees, so a bare unresolved identifier — the normal state of the
      // very name being completed — still has a tree at the caret to complete against. The
      // batch run remains as the fallback if the driver fails outright.
      val standard =
        interactiveCompletions(text, scalac, cp, caret).or(collectCompletions(typerRun, caret))

      dynamicCompletions(text, scalac, cp, caret).lay(standard): dynamic =>
        Completions(dynamic.replace, dynamic.items ::: standard.items)

    // `Compiled` runs the post-typer phases (stopping before bytecode generation,
    // so nothing is written to disk) purely to surface later diagnostics.
    val diagnostics =
      if !full then typerDiagnostics else
        frontend(text, scalac, cp): context =>
          context.setSetting(context.settings.YstopBefore, List("genBCode"))

        ._3

    (metaMap, diagnostics, completions)

  private def frontend
    ( text: Text, scalac: Scalac[?, ?], cp: Text )
    ( stop: Contexts.FreshContext => Contexts.FreshContext )
  :   (Run, Contexts.Context, List[Diagnostic]) =

    val collected: scm.ListBuffer[Diagnostic] = scm.ListBuffer()

    object reporter extends Reporter, UniqueMessagePositions, HideNonSensicalMessages:
      def doReport(diagnostic: CompilerDiagnostic)(using Contexts.Context): Unit =
        val pos = diagnostic.pos.span

        if pos.exists then
          val importance = diagnostic.level match
            case dotty.tools.dotc.interfaces.Diagnostic.ERROR   => Importance.Error
            case dotty.tools.dotc.interfaces.Diagnostic.WARNING => Importance.Warning
            case _                                              => Importance.Info

          val span = Span.offset(pos.start.z, pos.end - pos.start)
          collected += Diagnostic(span, diagnostic.message.tt, importance)

    object driver extends Driver:
      def context: Contexts.Context =
        val base = initCtx.fresh

        // The trailing empty argument stops the driver from treating an argument
        // list with no source files as a request to print usage and bail out.
        val arguments =
          (t"-classpath" :: cp :: scalac.commandLineArguments ::: List(t"")).map(_.s).toArray

        setup(arguments, base).map(_(1)).get

    val base: Contexts.FreshContext = driver.context.fresh
    base.setReporter(reporter)
    given context: Contexts.Context = stop(base)

    val source = SourceFile.virtual("<highlighting>", text.s)
    val run = Scalac.compiler().newRun
    run.compileSources(List(source))

    (run, context, collected.to(List))

  private def syntaxOf(using quotes: scala.quoted.Quotes)(tpe: Types.Type): Syntax =
    Syntax(tpe.asInstanceOf[quotes.reflect.TypeRepr])

  private def completionKind(symbol: Symbols.Symbol)(using Contexts.Context): Completion.Kind =
    if symbol.is(Flags.Extension) then Completion.Kind.Extension
    else if symbol.is(Flags.Package) then Completion.Kind.Package
    else if symbol.is(Flags.Module) then Completion.Kind.Module
    else if symbol.isType then Completion.Kind.Type
    else if symbol.is(Flags.Given) then Completion.Kind.Given
    else if symbol.is(Flags.Method) then Completion.Kind.Method
    else Completion.Kind.Term

  // Completions from a dedicated `InteractiveDriver` run: the presentation compiler's entry
  // point, which types in interactive mode and *retains error trees*, so scope completions
  // work for bare (necessarily unresolved) identifiers in both term and type position — the
  // batch run destroys the enclosing statement's tree in exactly those cases. Any failure
  // degrades to `Unset` and the batch route below takes over.
  private def interactiveCompletions(text: Text, scalac: Scalac[?, ?], cp: Text, caret: Ordinal)
  :   Optional[Completions] =

    try
      val settings = ("-classpath" :: cp.s :: scalac.commandLineArguments.map(_.s)).map(_.nn)
      val driver = interactive.InteractiveDriver(settings)
      // The driver resolves the URI as a path, so it must use the `file` scheme, though no
      // file exists there: the source text is supplied directly.
      val uri = java.nio.file.Path.of("/harlequin-highlighting.scala").nn.toUri.nn
      driver.run(uri, text.s)

      val unit = driver.compilationUnits(uri)

      given context: Contexts.Context =
        dotty.tools.dotc.quoted.QuotesCache.init(driver.currentCtx.fresh.setCompilationUnit(unit))

      given quotes: scala.quoted.Quotes = scala.quoted.runtime.impl.QuotesImpl()(using context)

      val position = SourcePosition(unit.source, Spans.Span(caret.n0))
      val (offset, raw) = interactive.Completion.completions(position)

      val items = raw.flatMap: completion =>
        completion.symbols.map: symbol =>
          Completion
            ( completion.label.tt, completionKind(symbol), syntaxOf(symbol.info.widenTermRefExpr) )

      if items.isEmpty then Unset else Completions(Span.offset(offset.z, 0), items)

    catch case scala.util.control.NonFatal(_) => Unset

  // Reuse the compiler's interactive completion engine over the typed tree, so we
  // inherit its full behaviour — direct members, inherited members, extension
  // methods (all four resolution routes), implicit-conversion members and givens.
  private def collectCompletions(run: Run, caret: Ordinal): Completions =
    val unit = run.units.head

    given context: Contexts.Context =
      dotty.tools.dotc.quoted.QuotesCache.init(run.runContext.fresh.setCompilationUnit(unit))

    given quotes: scala.quoted.Quotes = scala.quoted.runtime.impl.QuotesImpl()(using context)

    val position = SourcePosition(unit.source, Spans.Span(caret.n0))
    val (offset, raw) = dotty.tools.dotc.interactive.Completion.completions(position)

    val items = raw.flatMap: completion =>
      completion.symbols.map: symbol =>
        Completion
          ( completion.label.tt, completionKind(symbol), syntaxOf(symbol.info.widenTermRefExpr) )

    Completions(Span.offset(offset.z, 0), items)

  // Members reached through a `Dynamic` receiver are not symbols, so the interactive engine
  // cannot offer them; but the receiver's refined type often determines them fully (Xenophile's
  // `Foreign`, say, records the foreign type in its `Topic` refinement). If the caret sits on a
  // member selection whose qualifier derives from `scala.Dynamic`, find the base `Dynamic`
  // type's companion object and — if it implements `prophesy.Completable` — load it
  // reflectively in this JVM (as Xenophile loads its grammars at macro time) and ask it which
  // members the refinement admits.
  //
  // The qualifier's type comes from a dedicated typer run over the source with `.partial`
  // excised: in the main run the very selection being completed is usually an error (the
  // partial name is no member — a macro-checked `selectDynamic` halts on it), and the error
  // destroys the statement's typed tree, taking the qualifier with it. Truncated, the
  // statement is just the qualifier expression, which types. Any failure — an untyped
  // qualifier, no companion, a companion that is not `Completable`, a provider exception —
  // degrades to `Unset`, never an error.
  private def dynamicCompletions(text: Text, scalac: Scalac[?, ?], cp: Text, caret: Ordinal)
  :   Optional[Completions] =

    val content = text.s.toCharArray.nn
    val point = caret.n0.min(content.length)
    var start = point

    while start > 0 && Lexis.identifierChar(content(start - 1)) do start -= 1

    // A member selection needs a `.` before the partial name, and a qualifier before that.
    if start < 2 || content(start - 1) != '.' then Unset else
      val prefix: Text = String(content, start, point - start).tt

      val truncated: Text =
        t"${String(content, 0, start - 1)}${String(content, point, content.length - point)}"

      val (run, _, _) =
        frontend(truncated, scalac, cp): context =>
          context.setSetting(context.settings.YstopAfter, List("typer"))

      val unit = run.units.head

      given context: Contexts.Context =
        dotty.tools.dotc.quoted.QuotesCache.init(run.runContext.fresh.setCompilationUnit(unit))

      given quotes: scala.quoted.Quotes = scala.quoted.runtime.impl.QuotesImpl()(using context)

      // The qualifier is the OUTERMOST `Dynamic`-typed, error-free term tree ending exactly at
      // the removed `.` — a macro-expanded qualifier (an `Inlined` chain such as Xenophile's)
      // contains call-site-spanned inner trees of unrelated types, so neither "deepest" nor
      // type-agnostic selection would find the expansion's own refined type.
      val qualifierEnd = start - 1

      var qualifier: Optional[ast.tpd.Tree] = Unset

      object finder extends ast.tpd.TreeTraverser:
        def traverse(tree: ast.tpd.Tree)(using Contexts.Context): Unit =
          val matches =
            tree.span.exists && tree.span.end == qualifierEnd && tree.isTerm &&
              !tree.tpe.isError && tree.tpe.derivesFrom(Symbols.defn.DynamicClass)

          if matches then
            val wider = qualifier.let: previous =>
              tree.span.start < previous.span.start

            if wider.or(true) then qualifier = tree

          traverseChildren(tree)

      finder.traverse(unit.tpdTree)

      qualifier.lay(Unset): tree =>
        import quotes.reflect.*

        val repr = tree.tpe.widen.asInstanceOf[TypeRepr]
        val dynamicClass = Symbol.requiredClass("scala.Dynamic")

        if !repr.derivesFrom(dynamicClass) then Unset else
          // The base classes that introduce `Dynamic`, tried in linearization order: the first
          // whose companion loads and implements `Completable` provides the completions.
          val candidates = repr.baseClasses.filter: cls =>
            cls != dynamicClass && cls.typeRef.derivesFrom(dynamicClass) &&
              cls.companionModule.exists

          val provided = candidates.view.map: cls =>
            try
              // The module class's full name is the JVM binary name (already `$`-suffixed);
              // its singleton is the static `MODULE$` field.
              val module =
                Class.forName(cls.companionModule.moduleClass.fullName).nn
                . getField("MODULE$").nn
                . get(null).nn

              module match
                case completable: prophesy.Completable =>
                  val items =
                    completable.completions(repr, prefix).filter(_.name.s.startsWith(prefix.s))

                  if items.isEmpty then Unset
                  else Completions(Span.offset(start.z, prefix.length), items)

                case _ =>
                  Unset

            catch case scala.util.control.NonFatal(_) => Unset

          provided.collectFirst { case completions: Completions => completions }.getOrElse(Unset)

  private def collectTypes(run: Run): Map[(Int, Int), Syntax] =
    // Use the run's own context: the compilation advanced the compiler's periods,
    // so denotations created during typing are only valid relative to it. The
    // reflection API additionally needs a quote cache in the context property
    // (normally installed by macro expansion).
    given context: Contexts.Context =
      dotty.tools.dotc.quoted.QuotesCache.init(run.runContext.fresh)

    given quotes: scala.quoted.Quotes = scala.quoted.runtime.impl.QuotesImpl()(using context)

    val types: scm.HashMap[(Int, Int), Syntax] = scm.HashMap()

    // Highlighting must never fail on a type Stenography can't render; such a
    // token simply carries no type metadata.
    def record(start: Int, end: Int, tpe: Types.Type): Unit =
      if tpe.exists then
        try types((start, end)) = syntaxOf(tpe)
        catch case scala.util.control.NonFatal(_) => ()

    def ignored(name: Names.Name): Boolean =
      name == StdNames.nme.ERROR || name == StdNames.nme.CONSTRUCTOR

    object traverser extends ast.tpd.TreeTraverser:
      import ast.tpd.*

      def traverse(tree: Tree)(using Contexts.Context): Unit =
        tree match
          case tree: ValOrDefDef if tree.nameSpan.exists && !ignored(tree.name) =>
            record(tree.nameSpan.start, tree.nameSpan.end, tree.tpt.tpe)

          case tree: Select if tree.nameSpan.exists && !ignored(tree.name) =>
            record(tree.nameSpan.start, tree.nameSpan.end, tree.tpe)

          case tree: Ident if tree.span.exists && !ignored(tree.name) =>
            record(tree.span.start, tree.span.end, tree.tpe)

          case tree: TypeTree if tree.span.exists =>
            record(tree.span.start, tree.span.end, tree.tpe)

          case _ =>
            ()

        traverseChildren(tree)

    run.units.foreach: unit =>
      traverser.traverse(unit.tpdTree)

    types.to(Map)


case class SourceCode
  ( language:    ProgrammingLanguage,
    offset:      Int,
    lines:       IArray[List[Token]],
    focus:       Optional[Span] = Unset,
    diagnostics: List[Diagnostic] = Nil,
    completions: Optional[Completions] = Unset ):

  def lastLine: Int = offset + lines.length - 1
  def apply(line: Int): List[Token] = lines(line - offset)

  def extract(range: Span): SourceCode =
    val startLine = range.startLine.lay(0)(_.n0)
    val endLine = range.endLine.lay(startLine)(_.n0)

    if startLine != endLine
    then fragment(startLine, (endLine + 2).min(lastLine), range)
    else fragment(startLine, (endLine + 1).min(lastLine), range)


  def fragment(startLine: Int, endLine: Int, focus: Optional[Span] = Unset): SourceCode =
    SourceCode(language, startLine, lines.slice(startLine - offset, endLine - offset + 1), focus)
