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

import scala.collection.mutable.ListBuffer

import dotty.tools.dotc.*
import ast.tpd, tpd.*
import core.*, Constants.Constant, Contexts.*, Flags.*, NameKinds.UniqueName
import core.Names.*, core.Phases.Phase, StdNames.nme, Symbols.*
import fromtasty.TASTYCompiler
import reporting.{Diagnostic, StoreReporter}
import util.Spans.Span

// Recompiles classes from their TASTy with a probe at every method entry and every branch, for a
// host — fume — which wants to know which of them a test reaches. Each probe is a store of `true`
// into an array which the method gets, once, from `probably.Coverage`: that object, on the
// classpath of the code under test, decides which test the array belongs to.
//
// TASTy is what the compiler had before it inlined anything, so a recompilation inlines again,
// and the probes go in afterwards: only into code written in the unit itself, and never into
// what was inlined from elsewhere, though a lambda or an argument passed to an inline method is
// the unit's own and is probed wherever it lands.
//
// The recompiled classes must link against classes which were not recompiled, so the compiler
// must be the one which compiled them (`tooling` reads which that was) and must be given the
// options they were compiled with. Soundness's own are compiled in; capture checking is left
// out, since it affects no generated code and does not accept every tree it accepted from source.
object Instrumenter:
  // One unit to instrument: the path of its TASTy file, the name of the top-level class it
  // defines (`fume.Selection` for `fume/Selection.tasty`), and the index its probes are
  // recorded under in `Coverage`.
  final case class Target(tasty: String, name: String, index: Int)

  // A place a probe was put. The kinds are `class` (a class's body), `method` (a method's or a
  // lambda's entry), `then` and `else`, `case`, `catch`, `loop` (a loop's body), and `and` and
  // `or` (the right operand). Lines and columns count from one; `scope` is the method or class
  // the probe is within.
  final case class Probe
    ( index:       Int,
      kind:        String,
      file:        String,
      startLine:   Int,
      startColumn: Int,
      endLine:     Int,
      endColumn:   Int,
      scope:       String )

  // A unit which was instrumented — the classes one TASTy file defines, known by the name of
  // the class the file is named after — with the index its probes are recorded under.
  final case class Unit(index: Int, name: String, probes: List[Probe])

  final case class Result(units: List[Unit], errors: List[String])

  // What a host needs to cover a run: the directory of recompiled classes, to go ahead of the
  // rest of the classpath, and the units they were recompiled from; or, with neither, why not.
  final case class Plan(classes: String, units: List[Unit], problems: List[String])

  // The probes a test reached, each as a unit's index and a probe's. The test is known by the
  // key of its `TestEvent.Ref`; the empty name is what was reached outside any test.
  final case class Reached(test: String, probes: List[(Int, Int)])

  // Where, beneath the output directory, each unit's probes are listed.
  val directory: String = "META-INF/probably/coverage"

  // The compiler this is: what a TASTy file's `tooling` must equal for the file to be one this
  // can recompile compatibly.
  def compiler: String = "Scala "+config.Properties.simpleVersionString

  // The compiler which wrote a TASTy file, from its header; `null` if it is not one.
  def tooling(tasty: Array[Byte]): String | Null =
    var offset: Int = 4

    def natural(): Int =
      var result: Int = 0
      var continue: Boolean = true

      while continue do
        val byte: Int = tasty(offset) & 0xff
        offset += 1
        result = (result << 7) | (byte & 0x7f)
        continue = (byte & 0x80) == 0

      result

    val magic: Boolean =
      tasty.length > 8 && (tasty(0) & 0xff) == 0x5c && (tasty(1) & 0xff) == 0xa1 &&
        (tasty(2) & 0xff) == 0xab && (tasty(3) & 0xff) == 0x1f

    if !magic then null else
      try
        natural()
        natural()
        natural()
        val length: Int = natural()
        new String(tasty, offset, length, "UTF-8")
      catch case _: IndexOutOfBoundsException => null

  // The source file a TASTy file was compiled from, as the file records it — relative to the
  // compilation's source root, where it had one; `null` if it is not a TASTy file this compiler
  // reads, or records none.
  def source(tasty: Array[Byte]): String | Null =
    import core.tasty.{AttributeUnpickler, TastyUnpickler}
    import dotty.tools.tasty.{TastyFormat, TastyReader}

    val section: TastyUnpickler.SectionUnpickler[AttributeUnpickler] =
      new TastyUnpickler.SectionUnpickler[AttributeUnpickler](TastyFormat.AttributesSection):
        def unpickle(reader: TastyReader, names: TastyUnpickler.NameTable): AttributeUnpickler =
          new AttributeUnpickler(reader, names)

    try new TastyUnpickler(tasty).unpickle(section).flatMap(_.attributes.sourceFile).orNull
    catch case _: Exception => null

  // Recompiles the targets into `output`, against `classpath`, which must hold `probably.Coverage`
  // and everything the targets were compiled against. `options` are added to those compiled in.
  // Nothing is written for a unit unless every unit compiles.
  def instrument
    ( targets: List[Target], classpath: List[String], output: String, options: List[String] )
  :   Result =

    val units: ListBuffer[Unit] = ListBuffer()
    val reporter: StoreReporter = new StoreReporter(null)
    java.nio.file.Files.createDirectories(java.nio.file.Path.of(output))
    val indices: Map[String, Target] = targets.map { target => target.name -> target }.toMap

    val driver: Driver = new Driver:
      override protected def newCompiler(using Context): Compiler = new TASTYCompiler:
        override protected def transformPhases: List[List[Phase]] =
          List(new Probes(indices, unit => units.synchronized(units += unit))) ::
            super.transformPhases

    val arguments: List[String] =
      List("-from-tasty", "-nowarn", "-d", output, "-classpath")
      ::: List(classpath.mkString(java.io.File.pathSeparator.nn))
      ::: InstrumenterOptions.options
      ::: options
      ::: targets.map(_.tasty)

    // The compiler recurses deeply, and whichever thread a host calls from was not sized for it.
    var failure: Throwable | Null = null

    val compilation: Runnable = () =>
      try driver.process(arguments.toArray, reporter)
      catch case error: Throwable => failure = error

    val thread: Thread = new Thread(null, compilation, "probably-instrumenter", 256L*1024*1024)

    thread.start()
    thread.join()

    val crashed: Throwable | Null = failure

    val errors: List[String] =
      reporter.allErrors.map(describe) ++
        (if crashed == null then Nil else List("the compiler failed: "+trace(crashed)))

    if errors.isEmpty then
      val root: java.nio.file.Path = java.nio.file.Path.of(output, directory).nn
      java.nio.file.Files.createDirectories(root)

      units.foreach: unit =>
        java.nio.file.Files.writeString(root.resolve(unit.name), serialize(unit))

    Result(units.toList.sortBy(_.index), errors)

  // One line for each probe, tab-separated, below a line naming the unit's index and how many
  // probes it has.
  def serialize(unit: Unit): String =
    val lines: List[String] = unit.probes.map: probe =>
      List
        ( probe.index, probe.kind, probe.file, probe.startLine, probe.startColumn, probe.endLine,
          probe.endColumn, probe.scope )
      . mkString("\t")

    (s"# unit\t${unit.index}\t${unit.probes.length}\t${unit.name}" :: lines)
    . mkString("", "\n", "\n")

  // A unit's listing, read back.
  def parse(listing: String): Option[Unit] =
    def number(text: String): Int = text.toIntOption.getOrElse(0)

    listing.split("\n").nn.toList.map(_.nn).filter(_.nonEmpty) match
      case heading :: lines => heading.split("\t").nn.toList.map(_.nn) match
        case "# unit" :: index :: _ :: name :: _ =>
          val probes: List[Probe] = lines.flatMap: line =>
            line.split("\t", -1).nn.toList.map(_.nn) match
              case index :: kind :: file :: line :: column :: endLine :: endColumn :: scope :: _ =>
                List
                  ( Probe
                      ( number(index), kind, file, number(line), number(column), number(endLine),
                        number(endColumn), scope ) )

              case _ =>
                Nil

          Some(Unit(number(index), name, probes))

        case _ =>
          None

      case _ =>
        None

  private val suffix: String = ".tasty"

  private def jar(entry: String): Boolean =
    java.nio.file.Files.isRegularFile(java.nio.file.Path.of(entry))

  // The names of the TASTy files of a classpath entry, without the suffix: `fume/Selection`.
  private def names(entry: String): List[String] =
    try
      if jar(entry) then
        val zip: java.util.zip.ZipFile = new java.util.zip.ZipFile(entry)

        try
          val found: ListBuffer[String] = ListBuffer()
          val entries = zip.entries().nn

          while entries.hasMoreElements do
            val name: String = entries.nextElement().nn.getName.nn
            if name.endsWith(suffix) then found += name.dropRight(suffix.length)

          found.toList
        finally zip.close()
      else
        val root: java.nio.file.Path = java.nio.file.Path.of(entry).nn
        val found: ListBuffer[String] = ListBuffer()

        if java.nio.file.Files.isDirectory(root) then
          val walk = java.nio.file.Files.walk(root).nn

          try
            walk.forEach: file =>
              val name: String = root.relativize(file).nn.toString.replace('\\', '/').nn
              if name.endsWith(suffix) then found += name.dropRight(suffix.length)
          finally walk.close()

        found.toList
    catch case _: java.io.IOException => Nil

  private def read(entry: String, name: String): Option[Array[Byte]] =
    try
      if jar(entry) then
        val zip: java.util.zip.ZipFile = new java.util.zip.ZipFile(entry)

        try
          val found: java.util.zip.ZipEntry | Null = zip.getEntry(name)
          if found == null then None else Some(zip.getInputStream(found).nn.readAllBytes().nn)
        finally zip.close()
      else
        val file: java.nio.file.Path = java.nio.file.Path.of(entry, name).nn

        if !java.nio.file.Files.isRegularFile(file) then None
        else Some(java.nio.file.Files.readAllBytes(file).nn)
    catch case _: java.io.IOException => None

  private def contains(bytes: Array[Byte], part: Array[Byte]): Boolean =
    val last: Int = bytes.length - part.length
    var index: Int = 0
    var found: Boolean = false

    while !found && index <= last do
      var offset: Int = 0
      while offset < part.length && bytes(index + offset) == part(offset) do offset += 1
      found = offset == part.length
      index += 1

    found

  // Whether the source a TASTy file records is the one asked for: either may be the longer
  // path, since a file records its source relative to wherever its build was rooted.
  private def sourced(recorded: String | Null, asked: String): Boolean =
    recorded != null &&
      (recorded == asked || recorded.endsWith("/"+asked) || asked.endsWith("/"+recorded))

  // The units a term names, as the name and the bytes of each one's TASTy file. A term naming
  // a source file (`src/fume.Selection.scala`) is every unit compiled from it; any other is a
  // class (`fume.Selection`), or a member of one, or — ending `.*` — the classes of a package.
  // The first classpath entry to hold a unit is the one read, as a classloader would have it.
  def find(classpath: List[String], term: String): List[(String, Array[Byte])] =
    val text: String = term.replace('\\', '/').nn

    def first(name: String): Option[(String, Array[Byte])] =
      classpath.iterator.flatMap(read(_, name+suffix)).nextOption().map((name, _))

    def each(admit: String => Boolean): List[(String, Array[Byte])] =
      val seen: scala.collection.mutable.Set[String] = scala.collection.mutable.Set()

      classpath.flatMap: entry =>
        names(entry).filter { name => admit(name) && seen.add(name) }.flatMap: name =>
          read(entry, name+suffix).map((name, _))

    val found: List[(String, Array[Byte])] =
      if text.endsWith(".scala") || text.contains("/") then
        val basename: Array[Byte] =
          text.substring(text.lastIndexOf('/') + 1).nn.getBytes("UTF-8").nn

        each(_ => true).filter: (_, bytes) =>
          contains(bytes, basename) && sourced(source(bytes), text)
      else if text.endsWith(".*") then
        val prefix: String = text.dropRight(1).replace('.', '/').nn
        each { name => name.startsWith(prefix) && !name.drop(prefix.length).contains("/") }
      else
        // `a.b.C.d` may be the class `a.b.C.d`, or something within `a.b.C`, or within `a.b`.
        def recur(name: String): List[(String, Array[Byte])] = first(name) match
          case Some(unit) =>
            List(unit)

          case None =>
            val cut: Int = name.lastIndexOf('/')
            if cut < 0 then Nil else recur(name.substring(0, cut).nn)

        recur(text.replace('.', '/').nn)

    found.map { (name, bytes) => (name.replace('/', '.').nn, bytes) }

  // What identifies a recompilation: the compiler, this instrumenter — as the classes which put
  // the probes in, so that a recompilation by an earlier one is not mistaken for this one's —
  // the options, and every unit's name and TASTy, in the order which gives each its index.
  private def digest(units: List[(String, Array[Byte])], options: List[String]): String =
    val sha: java.security.MessageDigest = java.security.MessageDigest.getInstance("SHA-256").nn
    sha.update(compiler.getBytes("UTF-8"))

    List("Instrumenter$.class", "Instrumenter$Insertion.class", "Instrumenter$Probes.class")
    . foreach: name =>
        val stream: java.io.InputStream | Null = getClass.getResourceAsStream(name)

        if stream != null then
          try sha.update(stream.readAllBytes()) finally stream.close()


    (InstrumenterOptions.options ::: options).foreach: option =>
      sha.update(option.getBytes("UTF-8"))

    units.foreach: (name, bytes) =>
      sha.update(name.getBytes("UTF-8"))
      sha.update(bytes)

    sha.digest().nn.take(16).map { byte => String.format("%02x", Byte.box(byte)).nn }.mkString

  // Plans the coverage of the units the terms name, on a classpath, recompiling them beneath
  // `cache` unless that has been done before for the same units, compiler and options.
  def plan(classpath: List[String], terms: List[String], options: List[String], cache: String)
  :   Plan =

    val runtime: Boolean = classpath.exists(read(_, "probably/Coverage.class").isDefined)

    val found: List[(String, List[(String, Array[Byte])])] =
      terms.map { term => term -> find(classpath, term) }

    val missing: List[String] = found.filter(_(1).isEmpty).map(_(0))
    val units: List[(String, Array[Byte])] = found.flatMap(_(1)).distinctBy(_(0)).sortBy(_(0))

    val foreign: List[String] = units.flatMap: (name, bytes) =>
      val wrote: String | Null = tooling(bytes)

      if wrote == compiler then Nil else
        val writer: String = if wrote == null then "an unknown compiler" else wrote
        List(s"$name was compiled by $writer; only what $compiler compiled can be recompiled")

    def failed(problems: List[String]): Plan = Plan("", Nil, problems)

    if !runtime then
      failed
        ( List
            ( "the classpath's Probably does not record coverage: it has no probably.Coverage",
              "  coverage needs suites built against Soundness 0.76.0 or later" ) )
    else if missing.nonEmpty then
      failed(missing.map { term => s"no class or source file on the classpath matches $term" })
    else if foreign.nonEmpty then failed(foreign)
    else
      val root: java.nio.file.Path = java.nio.file.Path.of(cache, digest(units, options)).nn
      val classes: java.nio.file.Path = root.resolve("classes").nn
      val marker: java.nio.file.Path = root.resolve("complete").nn

      val errors: List[String] =
        if java.nio.file.Files.exists(marker) then Nil else
          try
            val targets: List[Target] = units.zipWithIndex.map: (unit, index) =>
              val name: String = "source/"+unit(0).replace('.', '/')+suffix
              val file: java.nio.file.Path = root.resolve(name).nn
              java.nio.file.Files.createDirectories(file.getParent)
              java.nio.file.Files.write(file, unit(1))
              Target(file.toString, unit(0), index)

            val result: Result = instrument(targets, classpath, classes.toString, options)
            if result.errors.isEmpty then java.nio.file.Files.writeString(marker, "")
            result.errors
          catch case error: java.io.IOException =>
            List(s"the recompiled classes could not be written beneath $root: $error")

      if errors.nonEmpty
      then failed("the units could not be recompiled with probes:" :: errors.map("  "+_))
      else
        val listed: List[Unit] = units.flatMap: (name, _) =>
          parse(java.nio.file.Files.readString(classes.resolve(directory).nn.resolve(name)).nn)

        Plan(classes.toString, listed, Nil)

  // The entries a classloader loads from, where it says — a host which was given a classpath
  // has no need to ask — and the JVM's own otherwise.
  def classpath(loader: ClassLoader): List[String] = loader match
    case loader: java.net.URLClassLoader =>
      loader.getURLs.nn.toList.map { url => java.nio.file.Path.of(url.nn.toURI).nn.toString }

    case _ =>
      System.getProperty("java.class.path").nn.split(java.io.File.pathSeparator).nn.toList
      . map(_.nn)

  private def runtime(loader: ClassLoader): (Class[?], AnyRef) =
    val module: Class[?] = loader.loadClass("probably.Coverage$").nn
    (module, module.getField("MODULE$").nn.get(null))

  // Has the `probably.Coverage` a classloader loads — the one the suites it runs will use —
  // open a collector for each test from now on; false if there is none to ask.
  def activate(loader: ClassLoader): Boolean =
    try
      val (module, instance) = runtime(loader)
      module.getMethod("activate").nn.invoke(instance)
      true
    catch case _: ReflectiveOperationException => false

  // What the tests run through a classloader have reached since the last time it was asked.
  def drain(loader: ClassLoader): List[Reached] =
    try
      val (module, instance) = runtime(loader)

      val drained: java.util.Map[String, Array[Array[Boolean]]] =
        module.getMethod("drain").nn.invoke(instance)
        . asInstanceOf[java.util.Map[String, Array[Array[Boolean]]]]

      val reached: ListBuffer[Reached] = ListBuffer()

      drained.forEach: (test, reachedUnits) =>
        val units: Array[Array[Boolean]] = reachedUnits.nn

        val probes: List[(Int, Int)] =
          units.indices.toList.flatMap: unit =>
            units(unit).indices.toList.filter(units(unit)(_)).map((unit, _))

        reached += Reached(test.nn, probes)

      reached.toList.sortBy(_.test)
    catch case _: ReflectiveOperationException => Nil

  private def trace(error: Throwable): String =
    val writer: java.io.StringWriter = new java.io.StringWriter()
    error.printStackTrace(new java.io.PrintWriter(writer))
    writer.toString

  private def describe(diagnostic: Diagnostic): String =
    val position = diagnostic.pos

    val place: String =
      if position.exists then s"${position.source.path}:${position.line + 1}: " else ""

    place+diagnostic.message

  private class Probes(targets: Map[String, Target], record: Unit => scala.Unit) extends Phase:
    def phaseName: String = "probes"

    def run(using Context): scala.Unit =
      val unit: CompilationUnit = ctx.compilationUnit

      val names: List[String] = unit.tpdTree match
        case PackageDef(_, statements) => statements.collect:
          case definition: TypeDef => definition.symbol.fullName.toString.stripSuffix("$")

        case _ => Nil

      // The file the unit was compiled from is the one its classes are positioned in.
      val home: Option[util.SourceFile] = unit.tpdTree match
        case PackageDef(_, statements) => statements.collectFirst:
          case definition: TypeDef => definition.source

        case _ => None

      names.flatMap(targets.get).headOption.foreach: target =>
        val coverage: Symbol = requiredModule("probably.Coverage")
        val probes: Symbol = coverage.requiredMethod("probes")
        val insertion: Insertion = Insertion(target, home, coverage, probes)
        val probed: Tree = insertion.transform(unit.tpdTree)
        unit.tpdTree = insertion.sized(probed)
        record(Unit(target.index, target.name, insertion.probes.toList))

  private class Insertion
    ( target: Target, home: Option[util.SourceFile], coverage: Symbol, method: Symbol )
  extends TreeMap:
    val probes: ListBuffer[Probe] = ListBuffer()

    // What would own the array of the code being transformed, the array itself once a probe has
    // needed it, whether that code is the unit's own, and whether the code around each
    // enclosing inlined call was.
    private var holder: Symbol = NoSymbol
    private var array: Symbol = NoSymbol
    private var own: Boolean = true
    private var callers: List[Boolean] = Nil
    private var scope: String = target.name

    // The number of probes is not known until the last has been put in, so each request for an
    // array is made with a placeholder, which this replaces.
    def sized(tree: Tree)(using Context): Tree =
      val size: Int = probes.length

      val sizing: TreeMap = new TreeMap:
        override def transform(tree: Tree)(using Context): Tree = tree match
          case Apply(function, List(index, _)) if function.symbol == method =>
            cpy.Apply(tree)(function, List(index, Literal(Constant(size)).withSpan(tree.span)))

          case _ =>
            super.transform(tree)

      sizing.transform(tree)

    private def store(kind: String, tree: Tree)(using Context): Tree =
      val span: Span = tree.span

      if !array.exists then
        val name: TermName = UniqueName.fresh(termName("probes"))
        val info = defn.ArrayOf(defn.BooleanType)
        array = newSymbol(holder, name, Synthetic, info, coord = span)

      val position = tree.sourcePos

      probes +=
        ( if !position.exists then Probe(probes.length, kind, "", 0, 0, 0, 0, scope) else
            Probe
              ( probes.length,
                kind,
                position.source.path,
                position.startLine + 1,
                position.startColumn + 1,
                position.endLine + 1,
                position.endColumn + 1,
                scope ) )

      ref(array).withSpan(span).select(nme.update).withSpan(span)
      . appliedTo
          ( Literal(Constant(probes.length - 1)).withSpan(span),
            Literal(Constant(true)).withSpan(span) )
      . withSpan(span)

    // Whether a tree is the unit's own code: not within the expansion of an inlined call,
    // unless it came from the caller, and positioned in the file the unit was compiled from —
    // which what an expansion of an expansion brought from elsewhere is not.
    private def native(tree: Tree): Boolean = own && home.forall(_ == tree.source)

    private def probed(kind: String, tree: Tree)(using Context): Tree =
      if !native(tree) || tree.isEmpty then tree
      else Block(List(store(kind, tree)), tree).withSpan(tree.span)

    // Transforms code which gets an array of its own, if it turns out to need one: a method's
    // body, a field's initializer, or a statement of a class's body.
    private def contained(owner: Symbol, span: Span)(body: Context ?=> Tree)(using Context): Tree =
      val holder0: Symbol = holder
      val array0: Symbol = array
      holder = owner
      array = NoSymbol

      val result: Tree =
        try body(using ctx.withOwner(owner)) finally holder = holder0

      val made: Symbol = array
      array = array0

      if !made.exists then result else
        val request: Tree =
          ref(coverage).withSpan(span).select(method).withSpan(span)
          . appliedTo
              ( Literal(Constant(target.index)).withSpan(span),
                Literal(Constant(0)).withSpan(span) )
          . withSpan(span)

        Block(List(ValDef(made.asTerm, request).withSpan(span)), result).withSpan(result.span)

    private def scoped[result](name: String)(body: => result): result =
      val scope0: String = scope
      scope = name
      try body finally scope = scope0

    private def cases(kind: String, cases: List[CaseDef])(using Context): List[CaseDef] =
      cases.map: caseDef =>
        val body: Tree = caseDef.body match
          case body: Match if body.isSubMatch => transform(body)
          case body                           => probed(kind, transform(body))

        cpy.CaseDef(caseDef)(caseDef.pat, transform(caseDef.guard), body)

    override def transform(tree: Tree)(using Context): Tree = tree match
      case tree: DefDef =>
        val symbol: Symbol = tree.symbol

        // Left as they are: what has no body or keeps none (an inline method's is gone once it
        // is inlined), a constructor, in which nothing may precede the call to another, and
        // whatever the compiler wrote — a setter, whose body later phases expect to find as
        // they left it, an export's forwarder, a case class's methods — though not a lambda,
        // which is as synthetic as those and is the unit's own code.
        val skipped: Boolean =
          tree.rhs.isEmpty || symbol.isConstructor || symbol.is(Inline) || symbol.is(Macro) ||
            symbol.is(Erased) || symbol.is(Accessor) || symbol.is(Exported) ||
            (symbol.is(Synthetic) && !symbol.isAnonymousFunction)

        if skipped then tree else
          def body(using Context): Tree =
            val entry: Tree = if native(tree) then store("method", tree) else EmptyTree
            val rhs: Tree = transform(tree.rhs)
            if entry.isEmpty then rhs else Block(List(entry), rhs).withSpan(rhs.span)

          val name: String =
            if symbol.isAnonymousFunction then scope else symbol.showFullName

          cpy.DefDef(tree)(rhs = scoped(name)(contained(symbol, tree.rhs.span)(body)))

      case tree: Template =>
        val dummy: Symbol = tree.symbol
        val owner: Symbol = ctx.owner

        val statements: List[Tree] = tree.body.map:
          case statement: ValDef if !statement.rhs.isEmpty =>
            val symbol: Symbol = statement.symbol
            val rhs: Tree = contained(symbol, statement.rhs.span)(transform(statement.rhs))
            cpy.ValDef(statement)(rhs = rhs)

          case statement: (DefDef | TypeDef | ValDef) =>
            transform(statement)

          case statement if statement.isTerm =>
            contained(dummy, statement.span)(transform(statement))

          case statement =>
            transform(statement)

        // A class the compiler has found to have no initialization code is compiled as having
        // none, and a value class may have none, so neither is given any.
        val probeless: Boolean =
          !native(tree) || !dummy.exists || owner.is(Synthetic) || owner.isDerivedValueClass ||
            owner.is(NoInits)

        val body: List[Tree] =
          if probeless then statements else
            val entry: Tree =
              scoped(owner.showFullName)(contained(dummy, tree.span)(store("class", tree)))

            entry :: statements

        cpy.Template(tree)(body = body)

      case tree: TypeDef if tree.isClassDef =>
        scoped(tree.symbol.showFullName)(super.transform(tree))

      case tree: If if !tree.isInline =>
        cpy.If(tree)
          ( transform(tree.cond),
            probed("then", transform(tree.thenp)),
            probed("else", transform(tree.elsep)) )

      case tree: Match if !tree.isInline =>
        cpy.Match(tree)(transform(tree.selector), cases("case", tree.cases))

      case tree: Try =>
        cpy.Try(tree)(transform(tree.expr), cases("catch", tree.cases), transform(tree.finalizer))

      case tree: WhileDo =>
        val body: Tree = tree.body match
          case body @ Literal(_) => body
          case body              => probed("loop", transform(body))

        cpy.WhileDo(tree)(transform(tree.cond), body)

      case Apply(function @ Select(left, name), List(right))
        if function.symbol == defn.Boolean_&& || function.symbol == defn.Boolean_|| =>

        val kind: String = if function.symbol == defn.Boolean_&& then "and" else "or"

        cpy.Apply(tree)
          ( cpy.Select(function)(transform(left), name), List(probed(kind, transform(right))) )

      // The arguments of an inlined call are bound in the caller's code; its expansion is the
      // inline method's, apart from whatever is marked, by an `Inlined` with no call, as having
      // come from the caller.
      case tree: Inlined if !tree.call.isEmpty =>
        val bindings: List[MemberDef] = transformSub(tree.bindings)
        val own0: Boolean = own
        callers ::= own
        own = false

        val expansion: Tree =
          try transform(tree.expansion) finally
            own = own0
            callers = callers.tail

        cpy.Inlined(tree)(tree.call, bindings, expansion)

      case tree: Inlined if callers.nonEmpty =>
        val own0: Boolean = own
        val callers0: List[Boolean] = callers
        own = callers.head
        callers = callers.tail

        try super.transform(tree) finally
          own = own0
          callers = callers0

      case _ =>
        super.transform(tree)
