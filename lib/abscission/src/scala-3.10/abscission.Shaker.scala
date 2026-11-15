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
package abscission

import scala.collection.mutable

import dotty.tools.dotc.*
import ast.tpd, tpd.*
import core.*, Constants.*, Contexts.*, Decorators.*, Denotations.staticRef
import core.DenotTransformers.IdentityDenotTransformer
import core.Flags.*, NameKinds.*, NameOps.*, Names.*, NullOpsDecorator.*, Phases.Phase
import core.StdNames.*, Symbols.*, Types.*
import fromtasty.{TASTYCompilationUnit, TASTYCompiler, TASTYRun}
import reporting.{Diagnostic, Reporter, StoreReporter}
import transform.{MegaPhase, ResolveSuper}
import dotty.tools.io.AbstractFile

// Recompiles a closed world of classes from the TASTy a classpath holds for them, leaving out
// whatever nothing can reach from a set of roots. It is ProGuard's job, done where the compiler
// still knows what the code means: before the phases which generate a trait's forwarders, a
// lazy value's plumbing or a bridge, so that none of those is generated for a member which is
// not kept, and none needs a rule to keep it.
//
// TASTy is what the compiler had before it inlined anything, so a recompilation inlines again
// and runs every macro again; what can be reached is only known afterwards. The analysis
// therefore sits between the phases which do that and the ones which transform the trees, and
// it reads units as it finds it needs them: a unit nothing reaches is never read, and the
// macros in it never run.
object Shaker:
  // Something to keep, as bytecode would name it: `owner` is a class's binary name with dots
  // (`fume.Selection$`, or `fume.Selection` for the static side of an object), and `member` the
  // name of a method or field of it — empty for the class itself, which is then one that may
  // be instantiated by reflection. `whole` keeps every member of the class.
  final case class Root(owner: String, member: String, whole: Boolean)

  final case class Problem(file: String, line: Int, message: String)

  // What a run did: the units it read, of those it could have; the classes it left trees for,
  // and how many of those are only shells, with no initialization code; and the members it
  // kept and removed in those classes.
  final case class Tally
    ( units:      Int,
      classes:    Int,
      shells:     Int,
      discarded:  Int,
      members:    Int,
      removed:    Int )

  // `dynamic` are the classes named in `Class.forName` calls the analysis reached, which
  // bytecode does not otherwise refer to.
  final case class Outcome(problems: List[Problem], tally: Tally, dynamic: List[String])

  // The compiler this is: what a TASTy file's `tooling` must equal for the file to be one this
  // can recompile compatibly.
  def compiler: String = "Scala "+config.Properties.simpleVersionString

  // Recompiles every unit, removing nothing.
  def recompile
    ( units: List[String], classpath: List[String], output: String, options: List[String] )
  :   Outcome =

    drive(new Plan(units.toSet, Nil, false, _ => ()), units, classpath, output, options)

  // Recompiles what the roots reach, of the units in `universe`: each is the name of a
  // top-level class with TASTy on the classpath (`fume.Selection` for `fume/Selection.tasty`).
  def shake
    ( universe:  List[String],
      roots:     List[Root],
      classpath: List[String],
      output:    String,
      options:   List[String],
      progress:  String => Unit = _ => () )
  :   Outcome =

    drive(new Plan(universe.toSet, roots, true, progress), Nil, classpath, output, options)

  private def drive
    ( plan:      Plan,
      initial:   List[String],
      classpath: List[String],
      output:    String,
      options:   List[String] )
  :   Outcome =

    val reporter: StoreReporter = new StoreReporter(null)
    java.nio.file.Files.createDirectories(java.nio.file.Path.of(output))

    val driver: Driver = new Driver:
      override protected def sourcesRequired: Boolean = false

      override protected def newCompiler(using Context): Compiler = new ShakeCompiler(plan, initial)

      // The units are named by class, not by file, so there are no files to ask for.
      override protected def doCompile(compiler: Compiler, files: List[AbstractFile])
        ( using Context )
      :   Reporter =

        super.doCompile(compiler, List(new dotty.tools.io.VirtualFile("units", new Array[Byte](0))))

    val arguments: List[String] =
      List("-from-tasty", "-nowarn", "-d", output, "-classpath")
      ::: List(classpath.mkString(java.io.File.pathSeparator.nn))
      ::: ShakerOptions.options
      ::: options

    // The compiler recurses deeply, and whichever thread a host calls from was not sized for it.
    var failure: Throwable | Null = null

    val compilation: Runnable = () =>
      try driver.process(arguments.toArray, reporter)
      catch case error: Throwable => failure = error

    val thread: Thread = new Thread(null, compilation, "abscission-shaker", 512L*1024*1024)

    thread.start()
    thread.join()

    val crashed: Throwable | Null = failure

    val problems: List[Problem] =
      reporter.allErrors.map(describe) ++
        (if crashed == null then Nil
         else List(Problem("", 0, "the compiler failed: "+trace(crashed))))

    Outcome(problems, plan.tally, plan.dynamic.toList.sorted)

  private def trace(error: Throwable): String =
    val writer: java.io.StringWriter = new java.io.StringWriter()
    error.printStackTrace(new java.io.PrintWriter(writer))
    writer.toString

  private def describe(diagnostic: Diagnostic): Problem =
    val position = diagnostic.pos

    if position.exists then Problem(position.source.path, position.line + 1, diagnostic.message)
    else Problem("", 0, diagnostic.message)

  // What the phases of one run share: what was asked for, and what the analysis found.
  private class Plan
    ( val universe: Set[String],
      val roots:    List[Root],
      val prune:    Boolean,
      val progress: String => Unit ):

    val kept: mutable.HashSet[Symbol] = mutable.HashSet()
    val deleted: mutable.HashSet[Symbol] = mutable.HashSet()
    val dynamic: mutable.HashSet[String] = mutable.HashSet()
    var tally: Tally = Tally(0, 0, 0, 0, 0, 0)

    private val tiers: mutable.HashMap[Symbol, Boolean] = mutable.HashMap()

    // The name of the unit which defines a top-level class, as its TASTy file has it: with
    // whatever a file name cannot hold encoded, as the dot is in the name of the object which
    // holds the top-level definitions of a file `proscenium.List.scala`.
    def unit(top: Symbol)(using Context): String = top.fullName.mangledString.stripSuffix("$")

    // Whether a symbol is defined in a unit which this run may read and recompile.
    def loadable(symbol: Symbol)(using Context): Boolean =
      symbol.exists && !symbol.is(Package) && locally:
        val top: Symbol = symbol.topLevelClass
        top.exists && tiers.getOrElseUpdate(top, universe.contains(unit(top)))

  private class ShakeCompiler(plan: Plan, initial: List[String]) extends TASTYCompiler:
    // The checks of overriding are of source: a class which is kept without a member it once
    // had to define would fail them, and no class can be newly wrong.
    override protected def transformPhases: List[List[Phase]] =
      val phases: List[List[Phase]] = super.transformPhases.map: group =>
        group.map:
          case _: typer.RefChecks => new Unchecked
          case phase              => phase

      if plan.prune then List(new Shake(plan)) :: phases else List(new Bare) :: phases

    override protected def backendPhases: List[List[Phase]] =
      if plan.prune then List(new Verify(plan)) :: super.backendPhases else super.backendPhases

    override def newRun(using Context): Run =
      reset()
      new ShakeRun(this, ctx.addMode(Mode.ReadPositions), initial)

  private class ShakeRun(compiler: Compiler, context: Context, initial: List[String])
  extends TASTYRun(compiler, context):
    override def compile(files: List[AbstractFile]): Unit =
      compileUnits(initial.map(new TASTYCompilationUnit(_)))

  private class Unchecked extends MegaPhase.MiniPhase:
    override def phaseName: String = typer.RefChecks.name
    override def description: String = "the place of the checks of overriding, which are of source"
    override def runsAfter: Set[String] = Set(transform.ElimRepeated.name)

  // A unit which was compiled with capture checking is one the later phases generate different
  // code for, and TASTy records that it was, where a compilation from source would have found
  // it in the unit's imports or the options. `ReadTasty` restores it on the units it makes,
  // which are the first ones of a run; a unit read later, as the analysis finds it needs one,
  // is made here, so it is restored here too. With it restored for each unit, one run
  // recompiles units which were checked beside units which were not, each to the code it had.
  private def restore(unit: CompilationUnit)(using Context): Unit =
    unit.tastyInfo.foreach: info =>
      if info.attributes.captureChecked then
        unit.needsCaptureChecking = true
        ctx.run.nn.ccEnabledSomewhere = true

      if info.attributes.withPureFuns then unit.knowsPureFuns = true

  // Has no class written with its TASTy: what is emitted is for running, not for compiling
  // against.
  private class Bare extends Phase:
    def phaseName: String = "bare"

    protected def run(using Context): Unit = ctx.compilationUnit.pickled = Map.empty

  private class Shake(plan: Plan) extends Phase, IdentityDenotTransformer:
    def phaseName: String = "shake"
    override def changesMembers: Boolean = true

    protected def run(using Context): Unit = ()

    override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
      new Analysis(plan, this).run(units)

  // Fails the compilation if anything which is left refers to a member that was removed, or to
  // a member of a class that was: a later phase wrote a reference the analysis did not foresee,
  // which would otherwise be a `NoSuchMethodError` when the code ran.
  private class Verify(plan: Plan) extends Phase:
    def phaseName: String = "shakeVerify"

    protected def run(using Context): Unit = ()

    override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
      val emitted: mutable.HashSet[Symbol] = mutable.HashSet()

      val collector: TreeTraverser = new TreeTraverser:
        def traverse(tree: Tree)(using Context): Unit = tree match
          case tree: TypeDef if tree.isClassDef =>
            emitted += tree.symbol
            traverseChildren(tree)

          case _: PackageDef | _: Template | _: Block | _: DefDef | _: ValDef =>
            traverseChildren(tree)

          case _ =>
            ()

      val checker: TreeTraverser = new TreeTraverser:
        private var within: Symbol = NoSymbol

        private def check(tree: Tree)(using Context): Unit =
          val symbol: Symbol = tree.symbol

          if symbol.exists && symbol.isTerm then
            val owner: Symbol = symbol.owner

            val lost: Boolean =
              plan.deleted.contains(symbol) ||
                owner.isClass && !owner.is(Package) && !emitted.contains(owner) &&
                  plan.loadable(owner)

            if lost then
              report.error
                ( em"""${within.showLocated} refers to ${symbol.showLocated}, which was removed:
                      |a phase after the analysis wrote the reference, and the analysis has no
                      |rule which foresees it""",
                  tree.srcPos )

        def traverse(tree: Tree)(using Context): Unit = tree match
          case tree: (DefDef | TypeDef) =>
            val within0: Symbol = within
            within = tree.symbol
            try traverseChildren(tree) finally within = within0

          case tree: (Ident | Select) =>
            if tree.isTerm then check(tree)
            traverseChildren(tree)

          case _ =>
            traverseChildren(tree)

      units.foreach { unit => collector.traverse(unit.tpdTree) }
      units.foreach: unit =>
        checker.traverse(unit.tpdTree)(using ctx.fresh.setCompilationUnit(unit))

      units

  // The names the pattern matcher selects from the result of an extractor.
  private val extractions: List[String] =
    List("isEmpty", "get", "lengthCompare", "length", "apply", "drop", "toSeq", "toList")

  // The names which serialization finds by reflection.
  private val serializations: List[String] =
    List("writeReplace", "readResolve", "writeObject", "readObject", "readObjectNoData")

  private class Analysis(plan: Plan, phase: Shake):
    // What each class and member is defined by, in the units read so far. A class which is
    // local to a method is defined by neither: it is part of the method's body.
    private val templates: mutable.HashMap[Symbol, TypeDef] = mutable.HashMap()
    private val definitions: mutable.HashMap[Symbol, ValOrDefDef] = mutable.HashMap()
    private val units: mutable.ArrayBuffer[CompilationUnit] = mutable.ArrayBuffer()
    private val read: mutable.HashSet[String] = mutable.HashSet()
    private val wanted: mutable.LinkedHashSet[String] = mutable.LinkedHashSet()

    // Members whose bodies run, and classes whose initialization does.
    private val live: mutable.HashSet[Symbol] = mutable.HashSet()
    private val instantiated: mutable.HashSet[Symbol] = mutable.HashSet()
    private val initialized: mutable.HashSet[Symbol] = mutable.HashSet()
    private val pending: mutable.ArrayDeque[Symbol] = mutable.ArrayDeque()
    private val parked: mutable.ArrayBuffer[Symbol] = mutable.ArrayBuffer()

    // Classes which are kept, whose parents have yet to be looked at.
    private val outlines: mutable.ArrayDeque[Symbol] = mutable.ArrayDeque()
    private val shelved: mutable.ArrayBuffer[Symbol] = mutable.ArrayBuffer()

    // For each class, the classes instantiated which inherit from it, and the members of it
    // which are called and may be overridden; and likewise the classes kept, and the super
    // accessors, which each class that mixes a trait in must implement.
    private val instances: mutable.HashMap[Symbol, mutable.ArrayBuffer[Symbol]] = mutable.HashMap()
    private val targets: mutable.HashMap[Symbol, mutable.ArrayBuffer[Symbol]] = mutable.HashMap()
    private val called: mutable.HashSet[Symbol] = mutable.HashSet()
    private val mixers: mutable.HashMap[Symbol, mutable.ArrayBuffer[Symbol]] = mutable.HashMap()
    private val accessors: mutable.HashMap[Symbol, mutable.ArrayBuffer[Symbol]] = mutable.HashMap()

    // The names a member has: as it is written, and as it is compiled, which differs for one
    // that is annotated `@targetName`.
    private val compilations: mutable.HashMap[Symbol, (String, String)] = mutable.HashMap()

    private val foreign: mutable.HashMap[Symbol, Set[Name]] = mutable.HashMap()
    private val candidates: mutable.HashMap[Symbol, List[Symbol]] = mutable.HashMap()
    private val reflective: mutable.HashSet[Name] = mutable.HashSet()

    def run(initial: List[CompilationUnit])(using Context): List[CompilationUnit] =
      initial.foreach(index)
      plan.roots.foreach(seed)
      drain()

      if ctx.reporter.hasErrors then units.toList else
        val pruned: List[CompilationUnit] = units.toList.filter(prune)

        plan.tally =
          plan.tally.copy
            ( units   = read.size,
              classes = templates.keysIterator.count(plan.kept.contains),
              shells  =
                templates.keysIterator.count: symbol =>
                  plan.kept.contains(symbol) && !initialized.contains(symbol),
              discarded = templates.keysIterator.count(!plan.kept.contains(_)) )

        pruned

    // Units

    private def index(unit: CompilationUnit)(using Context): Unit =
      units += unit

      def member(tree: Tree): Unit = tree match
        case tree: PackageDef =>
          tree.stats.foreach(member)

        case tree: TypeDef if tree.isClassDef =>
          templates(tree.symbol) = tree
          val template: Template = tree.rhs.asInstanceOf[Template]
          definitions(template.constr.symbol) = template.constr
          template.body.foreach(member)

        case tree: ValOrDefDef =>
          definitions(tree.symbol) = tree

        case _ =>
          ()

      def top(tree: Tree): Unit = tree match
        case tree: PackageDef                  => tree.stats.foreach(top)
        case tree: TypeDef if tree.isClassDef  => read += plan.unit(tree.symbol)
        case _                                 => ()

      top(unit.tpdTree)
      member(unit.tpdTree)

    // The unit which defines a top-level class and its companion, read from its TASTy.
    private def unit(name: String)(using Context): Option[CompilationUnit] =
      def made(symbol: Symbol): Option[CompilationUnit] = symbol match
        case symbol: ClassSymbol if symbol.exists =>
          val tree: Tree = symbol.rootTree
          if tree.isEmpty then None
          else Some(CompilationUnit(symbol.classDenot, tree, forceTrees = true))

        case _ =>
          None

      staticRef(name.toTypeName, generateStubs = false) match
        case denotation: SymDenotations.ClassDenotation =>
          made(denotation.symbol).orElse(made(denotation.symbol.linkedClass))

        case _ =>
          staticRef(name.toTypeName.moduleClassName, generateStubs = false) match
            case denotation: SymDenotations.ClassDenotation => made(denotation.symbol)
            case _                                          => None

    // Reads the units which are wanted, and takes them through the phases the first units
    // have been through: the ones which inline, and so run macros.
    private def load()(using Context): Unit =
      val names: List[String] = wanted.toList.filterNot(read.contains)
      wanted.clear()
      read ++= names
      plan.progress(s"reading ${names.length} units, after ${read.size - names.length}")

      var fresh: List[CompilationUnit] = names.flatMap: name =>
        val found: Option[CompilationUnit] = unit(name)
        if found.isEmpty then report.error(em"the unit $name could not be read from its TASTy")
        found.foreach(restore)
        found

      val earlier: List[Phase] =
        ctx.base.allPhases.toList.takeWhile(_ ne phase).filter(_.phaseName != "readTasty")

      earlier.foreach: earlier =>
        if earlier.isRunnable then fresh = earlier.runOn(fresh)

      fresh.foreach(index)

    private def drain()(using Context): Unit =
      def unread(symbol: Symbol): Boolean = !read.contains(plan.unit(symbol.topLevelClass))
      var continue: Boolean = true

      while continue do
        while pending.nonEmpty || outlines.nonEmpty do
          if pending.nonEmpty then
            val symbol: Symbol = pending.removeHead()

            if unread(symbol) then
              parked += symbol
              wanted += plan.unit(symbol.topLevelClass)
            else if symbol.isClass then initialize(symbol)
            else body(symbol)
          else
            val symbol: Symbol = outlines.removeHead()

            if unread(symbol) then
              shelved += symbol
              wanted += plan.unit(symbol.topLevelClass)
            else outline(symbol)

        if wanted.nonEmpty && !ctx.reporter.hasErrors then
          load()
          pending ++= parked
          outlines ++= shelved
          parked.clear()
          shelved.clear()
        else continue = false

    // Roots

    // The classes a binary name may stand for: a class, and the class of an object, whose
    // static side has the name without the `$`.
    private def named(binary: String)(using Context): List[Symbol] =
      def top(name: String): List[Symbol] =
        List(staticRef(name.toTypeName, generateStubs = false).symbol)
        . filter { symbol => symbol.exists && symbol.isClass && !symbol.is(Package) }

      def module(name: String): List[Symbol] =
        List(staticRef(name.toTypeName.moduleClassName, generateStubs = false).symbol)
        . filter { symbol => symbol.exists && symbol.isClass }

      // A nested class is found by the name it is compiled with, which is its own unless it
      // was given another, as `&` is when it is annotated `@targetName("And")`.
      def nested(name: String): List[Symbol] =
        val cut: Int = name.lastIndexOf('$', name.length - 2)

        if cut <= 0 then Nil else
          val inner: String = name.substring(cut + 1).nn
          val simple: String = inner.stripSuffix("$")

          named(name.substring(0, cut).nn).filter(plan.loadable).flatMap: outer =>
            outer.info.decls.toList.flatMap: member =>
              if member.targetName.mangledString != simple then Nil
              else if member.isTerm then
                if member.is(Module) then List(member.moduleClass) else Nil
              else if member.isClass && !member.is(ModuleClass) && !inner.endsWith("$")
              then List(member)
              else Nil

      val direct: List[Symbol] =
        if binary.endsWith("$") then module(binary.dropRight(1)) else top(binary) ::: module(binary)

      // A classpath gives every class file a symbol in its package, so a nested class is
      // found by its binary name too, as a class which is nothing; only one which turns out
      // to be defined in a unit is of any use, and that is for the caller to tell.
      (direct ::: nested(binary)).distinct

    // Whether a member may be compiled to something of a given name: itself, or one of the
    // members the compiler derives from it, whose names begin or end with its own.
    private def compiles(symbol: Symbol, name: String)(using Context): Boolean =
      def derived(own: String): Boolean =
        name == own || name.startsWith(own+"$") || name.endsWith("$$"+own) ||
          name.contains("$$"+own+"$") || name == own+"_$eq"

      val (written, compiled) = compilations.getOrElseUpdate
        ( symbol, (symbol.name.mangledString, symbol.targetName.mangledString) )

      derived(written) || (written != compiled) && derived(compiled)

    private def seed(root: Shaker.Root)(using Context): Unit =
      val owners: List[Symbol] = named(root.owner)

      // A class which is not recompiled, but is on the classpath to be instantiated, is an
      // instance of whatever it inherits from that is.
      if root.member.isEmpty && !root.whole then
        owners.filterNot(plan.loadable).foreach(instantiate(_, true))

      owners.filter(plan.loadable).foreach: owner =>
        if root.whole then
          instantiate(owner)
          owner.info.decls.toList.foreach { member => if member.isTerm then refer(member) }
        else if root.member.isEmpty || root.member == "<init>" || root.member == "$init$" ||
          root.member == "MODULE$"
        then reflected(owner)
        else
          keep(owner)
          if owner.is(ModuleClass) then instantiate(owner)

          owner.asClass.baseClasses.filter(plan.loadable).foreach: base =>
            base.info.decls.toList.foreach: member =>
              if member.isTerm && compiles(member, root.member) then refer(member)

    // Liveness

    private def keep(symbol: Symbol)(using Context): Unit =
      if plan.loadable(symbol) && plan.kept.add(symbol) then
        wanted += plan.unit(symbol.topLevelClass)
        val owner: Symbol = symbol.owner
        if owner.isClass && !owner.is(Package) then keep(owner)
        val bases: List[Symbol] = symbol.asClass.baseClasses.tail
        bases.foreach(keep)

        // The methods of a value class are compiled as methods of its companion.
        if symbol.isDerivedValueClass then instantiate(symbol.companionModule.moduleClass)

        // A class is compiled with a static method for each method its companion still has,
        // which the companion must be there to be asked.
        if !symbol.is(ModuleClass) then
          val companion: Symbol = symbol.companionModule
          if companion.exists && companion.isStatic then keep(companion.moduleClass)
        else
          // And an object's `@static` members are compiled as members of its companion class.
          val statics: Boolean =
            symbol.info.decls.toList.exists(_.hasAnnotation(defn.ScalaStaticAnnot))

          if statics then keep(symbol.companionClass)

        if !symbol.is(Trait) then mix(symbol, bases)
        outlines += symbol

    // A class implements the super accessors of the traits it mixes in, each with a call to
    // whichever member comes next in the class's own order of inheritance.
    private def mix(symbol: Symbol, bases: List[Symbol])(using Context): Unit =
      bases.foreach: base =>
        val mixed: mutable.ArrayBuffer[Symbol] = mixers.getOrElseUpdate(base, mutable.ArrayBuffer())

        if !mixed.contains(symbol) then
          mixed += symbol
          accessors.get(base).foreach(_.toList.foreach(rebind(symbol, _)))

    // The constructor a parent of a class is called with.
    private def constructor(tree: Tree)(using Context): Symbol = tree match
      case tree: Apply     => constructor(tree.fun)
      case tree: TypeApply => constructor(tree.fun)
      case tree: Block     => constructor(tree.expr)
      case tree: Select    => if tree.symbol.isConstructor then tree.symbol else NoSymbol
      case _               => NoSymbol

    // A class which is kept, though never instantiated, still names the constructors of its
    // parents, and one which is not a class's first is a method like any other.
    private def outline(symbol: Symbol)(using Context): Unit =
      templates.get(symbol).foreach: definition =>
        definition.rhs.asInstanceOf[Template].parents.foreach: parent =>
          val called: Symbol = constructor(parent)
          if called.exists && !called.isPrimaryConstructor then enliven(called)

    private def rebind(symbol: Symbol, accessor: Symbol)(using Context): Unit =
      refer(ResolveSuper.rebindSuper(symbol, accessor))

    // The names of the members a class inherits from classes this run does not recompile:
    // whatever is defined with one of them may be called from there.
    private def inherited(symbol: Symbol)(using Context): Set[Name] =
      foreign.getOrElseUpdate
        ( symbol,
          symbol.info.decls.toList.filter { member => member.isTerm && !member.isConstructor }
          . map(_.name).toSet )

    // `foreign` is for a class which is not recompiled: it is not kept, and has no code to
    // run here, but what it inherits from classes which are recompiled is as for any other.
    private def instantiate(symbol: Symbol, foreign: Boolean = false)(using Context): Unit =
      val known: Boolean = symbol.exists && symbol.isClass && (plan.loadable(symbol) || foreign)

      if known && instantiated.add(symbol) then
        keep(symbol)
        val bases: List[Symbol] = symbol.asClass.baseClasses
        val foreigners: List[Symbol] = bases.filterNot(plan.loadable)
        val serializable: Boolean = symbol.derivesFrom(defn.JavaSerializableClass)
        val externalizable: Boolean = symbol.derivesFrom(requiredClass("java.io.Externalizable"))

        bases.foreach: base =>
          instances.getOrElseUpdate(base, mutable.ArrayBuffer()) += symbol

          if plan.loadable(base) then
            if initialized.add(base) then
              keep(base)
              pending += base

            val members: List[Symbol] =
              candidates.getOrElseUpdate
                ( base,
                  base.info.decls.toList.filter: member =>
                    member.isTerm && !member.isConstructor )

            members.foreach: member =>
              val name: Name = member.name

              val external: Boolean =
                foreigners.exists(inherited(_).contains(name)) || reflective.contains(name) ||
                  serializable && serializations.contains(name.toString)

              if external then refer(member)

            // Deserialization makes an instance with the constructor, of no parameters, of
            // the nearest class it inherits from which is not serializable; and of the
            // class itself, if it reads and writes its own state.
            val constructed: Boolean =
              serializable && !base.derivesFrom(defn.JavaSerializableClass) ||
                externalizable && (base eq symbol)

            if constructed then
              base.info.decls.toList.foreach: member =>
                if member.isConstructor && member.info.firstParamTypes.isEmpty then refer(member)

          targets.get(base).foreach(_.toList.foreach(resolve(symbol, _)))

        // A lambda of a trait's type is compiled as a class which mixes the trait in.
        if symbol.is(Trait) then mix(symbol, bases)

        // The JDK finds the values of an enumeration of Java's by calling the static `values`
        // of its class, which is compiled from the companion's.
        if symbol.derivesFrom(defn.JavaEnumClass) then enumerated(symbol)

    private def enumerated(symbol: Symbol)(using Context): Unit =
      symbol.asClass.baseClasses.filter(plan.loadable).foreach: base =>
        val companion: Symbol = base.companionModule

        if companion.exists && base.derivesFrom(defn.JavaEnumClass) then
          refer(companion)

          companion.moduleClass.info.decls.toList.foreach: member =>
            val name: String = member.name.toString
            if member.isTerm && (name == "values" || name == "valueOf") then refer(member)

    // A class which reflection may make an instance of, with any of its constructors.
    private def reflected(symbol: Symbol)(using Context): Unit =
      instantiate(symbol)

      if plan.loadable(symbol) then
        symbol.info.decls.toList.foreach { member => if member.isConstructor then refer(member) }

    private def resolve(symbol: Symbol, target: Symbol)(using Context): Unit =
      val member: Symbol = target.matchingMember(symbol.thisType)
      if member.exists && (member ne target) then refer(member)

    // A term which code that runs refers to.
    private def refer(symbol: Symbol)(using Context): Unit =
      if symbol.exists && symbol.isTerm then
        val owner: Symbol = symbol.owner

        if symbol.is(Module) then
          instantiate(symbol.moduleClass)

          if owner.isClass && !owner.is(Package) then
            keep(owner)
            dispatch(symbol)
        else if owner.isClass && !owner.is(Package) then
          if symbol.isConstructor then
            instantiate(owner)
            if !symbol.isPrimaryConstructor then enliven(symbol)
          else if symbol.is(Method) || symbol.is(Lazy) then
            // A method which is always inlined is not there to call.
            if !(symbol.is(Inline) && !symbol.isRetainedInline) then
              enliven(symbol)
              dispatch(symbol)

              // A member of an object is reached through the object, whose prefix a tree
              // read from TASTy need not show: it is not there after `import Globe.*`.
              if owner.is(ModuleClass) then instantiate(owner)
          else
            if symbol.is(ParamAccessor) then keep(owner) else instantiate(owner)

            // What implements an abstract field need not be a field: it may be an object, or
            // a method, and for a variable a method to set it with.
            dispatch(symbol)

            if symbol.is(Mutable) then
              val setter: Symbol = symbol.setter
              if setter.exists then dispatch(setter)

    private def enliven(symbol: Symbol)(using Context): Unit =
      if plan.loadable(symbol) && live.add(symbol) then
        keep(symbol.owner)
        pending += symbol

        if symbol.name.is(SuperAccessorName) then
          accessors.getOrElseUpdate(symbol.owner, mutable.ArrayBuffer()) += symbol
          mixers.get(symbol.owner).foreach(_.toList.foreach(rebind(_, symbol)))

        if symbol.isRetainedInlineMethod then
          symbol.owner.info.decls.toList.foreach: member =>
            if member.name.is(BodyRetainerName) &&
              member.name.asTermName.exclude(BodyRetainerName) == symbol.name
            then enliven(member)

    // A call may be to whatever overrides its target, in a class which is instantiated.
    private def dispatch(symbol: Symbol)(using Context): Unit =
      if !symbol.is(Private) && called.add(symbol) then
        val owner: Symbol = symbol.owner
        targets.getOrElseUpdate(owner, mutable.ArrayBuffer()) += symbol
        instances.get(owner).foreach(_.toList.foreach(resolve(_, symbol)))

    // Traversal

    private def body(symbol: Symbol)(using Context): Unit =
      definitions.get(symbol).foreach: definition =>
        // The body of a method which is inlined, and yet is called, is in its retainer.
        if !symbol.isRetainedInlineMethod
        then walker.traverse(definition.rhs)(using ctx.withOwner(symbol))

    // The code which runs when a class is instantiated: the arguments to its parents'
    // constructors, its fields' initial values and the statements of its body.
    private def initialize(symbol: Symbol)(using Context): Unit =
      templates.get(symbol).foreach: definition =>
        val template: Template = definition.rhs.asInstanceOf[Template]
        given Context = ctx.withOwner(template.symbol)
        template.parents.foreach { parent => if parent.isTerm then walker.traverse(parent) }

        template.body.foreach:
          case field: ValDef  => if !field.symbol.is(Lazy) then walker.traverse(field.rhs)
          case _: MemberDef   => ()
          case _: Import      => ()
          case statement      => walker.traverse(statement)

        // Whether a parameter of a class is a field of it, and whether a field is cleared
        // once the one lazy value which reads it has been computed, are decided later by
        // what refers to it, which taking a method out would change; and for a class which
        // is serializable, the fields are what is written.
        if symbol.derivesFrom(defn.JavaSerializableClass) then
          template.body.foreach:
            case member: ValOrDefDef if member.symbol.is(Method) || member.symbol.is(Lazy) =>
              if confides(member.rhs, symbol) then refer(member.symbol)

            case _ =>
              ()

    // Whether a tree refers to a field which is private to an instance of a class.
    private def confides(tree: Tree, owner: Symbol)(using Context): Boolean =
      tree.existsSubTree: tree =>
        val symbol: Symbol = tree match
          case tree: (Ident | Select) => tree.symbol
          case _                      => NoSymbol

        symbol.exists && symbol.isTerm && (symbol.owner eq owner) && !symbol.is(Method) &&
          symbol.is(Private)

    private def singletons(tpe: Type)(using Context): Unit = tpe.dealias match
      case tpe: TermRef       => refer(tpe.symbol)
      case tpe: AndOrType     => singletons(tpe.tp1); singletons(tpe.tp2)
      case tpe: AnnotatedType => singletons(tpe.parent)
      case tpe: RefinedType   => singletons(tpe.parent)
      case _                  => ()

    // The members the pattern matcher will select from an extractor's result, which no tree
    // refers to until it has.
    private def extracted(tpe: Type, depth: Int)(using Context): Unit =
      if depth < 3 then
        tpe.classSymbols.filter(plan.loadable).foreach: symbol =>
          keep(symbol)

          symbol.asClass.baseClasses.filter(plan.loadable).foreach: base =>
            base.info.decls.toList.foreach: member =>
              val name: String = member.name.toString

              val selected: Boolean =
                member.isTerm &&
                  (member.is(CaseAccessor) || extractions.contains(name) ||
                    name.startsWith("_") && name.drop(1).forall(_.isDigit) && name.length > 1)

              if selected then refer(member)

        val get: Denotations.Denotation = tpe.member(nme.get)
        if get.exists then extracted(get.info.finalResultType, depth + 1)

    // Whether a tree is a path of packages and of objects which are nested in nothing but
    // those: one which is not evaluated on the way to an object that it leads to.
    private def path(tree: Tree)(using Context): Boolean =
      def fixed(symbol: Symbol): Boolean =
        symbol.is(Package) || symbol.isStatic && (symbol.is(Module) || symbol.is(ModuleClass))

      tree match
        case tree: Ident  => fixed(tree.symbol)
        case tree: Select => fixed(tree.symbol) && path(tree.qualifier)
        case tree: This   => fixed(tree.symbol)
        case _            => false

    private val walker: TreeTraverser = new TreeTraverser:
      def traverse(tree: Tree)(using Context): Unit = tree match
        case tree if tree.isType =>
          singletons(tree.tpe)

          // A test for a class which is nested in an instance of another is compiled as a
          // test of that instance too, through the nested class's own reference to it.
          tree.tpe.classSymbols.foreach: symbol =>
            if symbol.owner.isClass && !symbol.isStatic then keep(symbol)

        case tree: Ident =>
          refer(tree.symbol)

          // An identifier read from TASTy may stand for a selection from an object, as it
          // does after `import Chorus.*`: the object is in its type, and nowhere else.
          tree.tpe match
            case tpe: TermRef =>
              tpe.prefix match
                case prefix: TermRef  => refer(prefix.symbol)
                case prefix: ThisType => if prefix.cls.is(ModuleClass) then instantiate(prefix.cls)
                case _                => ()

            case _ =>
              ()

        case tree: Select =>
          // A member of a type which may be null has no one symbol, where selecting it was
          // allowed: it is whatever has its name, in what the type is when it is not null.
          if tree.symbol.exists then refer(tree.symbol) else
            tree.qualifier.tpe.widenDealias.stripNull().member(tree.name).alternatives.foreach:
              member => refer(member.symbol)

          // A reference to an object which is nested in others, all the way out, loads it
          // without loading those.
          if !(tree.symbol.is(Module) && tree.symbol.isStatic && path(tree.qualifier))
          then traverse(tree.qualifier)

        case tree: This =>
          if tree.symbol.is(ModuleClass) then instantiate(tree.symbol)

        case tree: Inlined =>
          tree.bindings.foreach(traverse)
          traverse(tree.expansion)

        case tree: Closure =>
          // A lambda of a type other than a function's is an instance of that type.
          if !tree.tpt.isEmpty then
            val symbol: Symbol = tree.tpt.tpe.classSymbol
            instantiate(symbol)

            if plan.loadable(symbol) then
              tree.tpt.tpe.possibleSamMethods.foreach { method => refer(method.symbol) }

          traverseChildren(tree)

        case tree: UnApply =>
          extracted(tree.fun.tpe.widen.finalResultType, 0)
          traverseChildren(tree)

        case Literal(constant) =>
          if constant.tag == Constants.ClazzTag then reflected(constant.typeValue.classSymbol)

        // `classOf`, as it is written, is not yet the constant it becomes.
        case tree: TypeApply if tree.fun.symbol == defn.Predef_classOf =>
          tree.args.foreach { argument => reflected(argument.tpe.classSymbol) }

        case tree: Apply =>
          val symbol: Symbol = tree.fun.symbol

          tree.args match
            case Literal(Constant(name: String)) :: _ if symbol.exists =>
              // A static method of a Java class is a member of the class's companion.
              val owner: String = symbol.owner.fullName.toString.stripSuffix("$")
              val method: String = symbol.name.toString

              if owner == "scala.reflect.Selectable" &&
                (method == "selectDynamic" || method == "applyDynamic")
              then reflect(name.toTermName.decode)
              else if owner == "java.lang.Class" && method == "forName" then
                plan.dynamic += name
                named(name).foreach(reflected)

            case _ =>
              ()

          traverseChildren(tree)

        case tree: TypeDef if tree.isClassDef =>
          // A class which is local to code that runs is kept whole.
          instantiate(tree.symbol)
          traverseChildren(tree)

        case _: Import =>
          ()

        case _ =>
          traverseChildren(tree)

    // A member selected by name, through `reflectiveSelectable`, may be any member of that name.
    private def reflect(name: Name)(using Context): Unit =
      if reflective.add(name) then
        instantiated.toList.foreach: symbol =>
          symbol.asClass.baseClasses.filter(plan.loadable).foreach: base =>
            base.info.decl(name).alternatives.foreach { member => refer(member.symbol) }

    // Pruning

    // In place of an argument to the constructor of a parent of a class which is never
    // instantiated.
    private def absent(tree: Tree)(using Context): Tree =
      Throw(Literal(Constant(null)).withSpan(tree.span)).withSpan(tree.span)

    private def unparented(tree: Tree)(using Context): Tree = tree match
      case tree: Apply     => cpy.Apply(tree)(unparented(tree.fun), tree.args.map(absent))
      case tree: TypeApply => cpy.TypeApply(tree)(unparented(tree.fun), tree.args)
      case tree: Block     => unparented(tree.expr)
      case _               => tree

    // Whether a definition of a class's body stays: `whole` is whether the class is ever
    // instantiated, and `fields` are the names of the fields of it which stay, whose setters
    // must, since an assignment refers to the field and is compiled to a call of the setter.
    private def retained(tree: Tree, whole: Boolean, fields: Set[Name])(using Context): Boolean =
      tree match
        case tree: TypeDef =>
          !tree.isClassDef || plan.kept.contains(tree.symbol)

        case tree: ValDef =>
          val symbol: Symbol = tree.symbol

          if symbol.is(Module) then plan.kept.contains(symbol.moduleClass)
          else if symbol.is(Lazy) then live.contains(symbol)
          else whole || symbol.is(ParamAccessor)

        case tree: DefDef =>
          val symbol: Symbol = tree.symbol

          if symbol.is(Accessor) && symbol.name.isSetterName
          then fields.contains(symbol.name.asTermName.getterName)
          else live.contains(symbol) || symbol.isErased

        case _: Import =>
          true

        case _ =>
          whole

    private def pruned(tree: Tree)(using Context): Tree = tree match
      case tree: PackageDef =>
        cpy.PackageDef(tree)(tree.pid, tree.stats.filter(retained(_, true, Set())).map(pruned))

      case tree: TypeDef if tree.isClassDef =>
        val symbol: Symbol = tree.symbol
        val template: Template = tree.rhs.asInstanceOf[Template]
        val whole: Boolean = initialized.contains(symbol)
        val fields: Set[Name] =
          template.body.collect:
            case field: ValDef if retained(field, whole, Set()) => field.symbol.name
          . toSet

        val (stays, goes) = template.body.partition(retained(_, whole, fields))
        var removed: Int = 0

        goes.foreach:
          case member: ValOrDefDef =>
            plan.deleted += member.symbol
            member.symbol.dropAfter(phase)
            removed += 1

          case _ =>
            ()

        plan.tally =
          plan.tally.copy
            ( members = plan.tally.members + stays.count(_.isInstanceOf[ValOrDefDef]),
              removed = plan.tally.removed + removed )

        val body: List[Tree] = stays.map:
          case member: DefDef if member.symbol.isRetainedInlineMethod =>
            cpy.DefDef(member)(rhs = absent(member.rhs))

          case member =>
            pruned(member)

        val parents: List[Tree] =
          if whole then template.parents else template.parents.map(unparented)

        cpy.TypeDef(tree)
          ( rhs = cpy.Template(template)(template.constr, parents, Nil, template.self, body) )

      case _ =>
        tree

    // Whether anything of a unit is left, once what is not kept has been taken out of it.
    private def prune(unit: CompilationUnit)(using Context): Boolean =
      given Context = ctx.fresh.setCompilationUnit(unit)
      unit.tpdTree = pruned(unit.tpdTree)
      unit.pickled = Map.empty

      // A unit's classes may be within several packages, one inside another.
      def classes(tree: Tree): Boolean = tree match
        case tree: PackageDef => tree.stats.exists(classes)
        case tree: TypeDef    => tree.isClassDef
        case _                => false

      classes(unit.tpdTree)
