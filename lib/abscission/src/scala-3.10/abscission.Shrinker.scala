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

import java.lang.classfile as jlc
import java.lang.classfile.attribute as jlca
import java.lang.classfile.constantpool as jlcc
import java.lang.classfile.instruction as jlci
import java.lang.constant as jlcn
import java.nio.file as jnf
import java.util.zip as juz

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

// Makes a closed world of a classpath: the classes which a set of entry points can reach, and
// nothing else. Classes which were compiled by this compiler are recompiled from their TASTy by
// `Shaker`, without the members nothing reaches; the rest, which are Java, Kotlin and the
// standard library, are copied or left out, a class at a time, by following the references
// bytecode makes.
//
// A class which was compiled by this compiler, and which what is kept refers to only as a type —
// in a signature, a cast or a test — is written as a shell: its name, its flags and its parents,
// which is all that a virtual machine asks of a class it never runs.
object Shrinker:
  // What to shrink: the classpath's entries, directories and JARs, in a classloader's order; the
  // directory to write the closed world to; the compiler options the classes were compiled
  // with; the classes whose `main` methods are entry points; patterns of classes to keep whole,
  // and of classes not to recompile, in which `*` stands for part of a name and `**` for any
  // number of packages; whether every implementation of a service is an entry point; and
  // whether a library with no TASTy is taken a class at a time, rather than whole once any
  // class of it is reached.
  final case class Request
    ( classpath: List[String],
      output:    String,
      options:   List[String],
      mains:     List[String],
      keep:      List[String],
      pin:       List[String],
      services:  Boolean,
      fine:      Boolean )

  // What was done. `units` are the units which could be recompiled and `read` those which
  // were; `classes` and `bytes` are the classpath's, and `kept` and `written` the closed
  // world's, of which `recompiled` classes came from TASTy, `shells` are only shells and
  // `copied` are as they were. `members` and `removed` count the methods and fields of the
  // recompiled classes. `pinned` are the units which were not recompiled, by request or
  // because they could not be, and `notes` say why of the latter. `packages` are the packages
  // of the classpath, each with the number of classes it has there and in the closed world.
  final case class Report
    ( problems:   List[String],
      units:      Int,
      read:       Int,
      classes:    Int,
      bytes:      Long,
      kept:       Int,
      written:    Long,
      recompiled: Int,
      shells:     Int,
      copied:     Int,
      members:    Int,
      removed:    Int,
      pinned:     List[String],
      notes:      List[String],
      packages:   List[(String, Int, Int)],
      reasons:    Map[String, String] ):

    // Why a class is in the closed world: the chain of classes, each of which refers to the
    // one before, back to one that was an entry point or was recompiled.
    def why(name: String): List[String] =
      def recur(name: String, seen: Set[String]): List[String] = reasons.get(name) match
        case Some(referrer) if !seen.contains(referrer) => name :: recur(referrer, seen + name)
        case _                                         => List(name)

      recur(name, Set())

  private val classSuffix: String = ".class"
  private val tastySuffix: String = ".tasty"

  // An entry of the classpath.
  private final class Source(val path: String):
    private val root: jnf.Path = jnf.Path.of(path).nn
    val jar: Boolean = jnf.Files.isRegularFile(root)
    private val zip: juz.ZipFile | Null = if jar then new juz.ZipFile(path) else null

    val names: List[String] =
      val archive: juz.ZipFile | Null = zip

      if archive != null then
        archive.entries().nn.asScala.toList.filter(!_.isDirectory).map(_.getName.nn)
      else if !jnf.Files.isDirectory(root) then Nil
      else
        val walk = jnf.Files.walk(root).nn

        try
          walk.iterator.nn.asScala.toList.filter(jnf.Files.isRegularFile(_)).map: file =>
            root.relativize(file).nn.toString.replace('\\', '/').nn
        finally walk.close()

    def read(name: String): Array[Byte] =
      val archive: juz.ZipFile | Null = zip

      if archive != null then
        val stream = archive.getInputStream(archive.getEntry(name)).nn
        try stream.readAllBytes().nn finally stream.close()
      else jnf.Files.readAllBytes(root.resolve(name)).nn

    def size(name: String): Long =
      val archive: juz.ZipFile | Null = zip
      if archive != null then archive.getEntry(name).nn.getSize
      else jnf.Files.size(root.resolve(name))

    def close(): Unit =
      val archive: juz.ZipFile | Null = zip
      if archive != null then archive.close()

  private def pattern(glob: String): java.util.regex.Pattern =
    val quoted: String =
      glob.split("\\*\\*", -1).nn.toList.map(_.nn).map: part =>
        part.split("\\*", -1).nn.toList.map(_.nn).map(java.util.regex.Pattern.quote(_).nn)
        . mkString("[^.]*")
      . mkString(".*")

    java.util.regex.Pattern.compile(quoted).nn

  // The compiler which wrote a TASTy file, if it is one this compiler reads.
  private def tooling(tasty: Array[Byte]): String | Null =
    import dotty.tools.dotc.core.tasty.TastyUnpickler
    import dotty.tools.tasty.{TastyHeaderUnpickler, UnpickleException}

    try
      new TastyHeaderUnpickler(TastyUnpickler.scala3CompilerConfig, tasty).readFullHeader()
      . toolingVersion
    catch
      case _: UnpickleException          => null
      case _: IndexOutOfBoundsException  => null

  // The source file a TASTy file was compiled from, as the file records it.
  private def source(tasty: Array[Byte]): String | Null =
    import dotty.tools.dotc.core.tasty.{AttributeUnpickler, TastyUnpickler}
    import dotty.tools.tasty.{TastyFormat, TastyReader}

    val section: TastyUnpickler.SectionUnpickler[AttributeUnpickler] =
      new TastyUnpickler.SectionUnpickler[AttributeUnpickler](TastyFormat.AttributesSection):
        def unpickle(reader: TastyReader, names: TastyUnpickler.NameTable): AttributeUnpickler =
          new AttributeUnpickler(reader, names)

    try new TastyUnpickler(tasty).unpickle(section).flatMap(_.attributes.sourceFile).orNull
    catch case _: Exception => null

  // What a class file refers to: the classes it cannot be loaded or verified without, the
  // members it names, and the classes its own declaration names.
  private final case class Refs
    ( name:       String,
      flags:      Int,
      major:      Int,
      minor:      Int,
      superclass: String | Null,
      interfaces: List[String],
      classes:    Set[String],
      members:    Set[(String, String, String)],
      declared:   Set[(String, String)] ):

    def parents: List[String] =
      val parent: String | Null = superclass
      if parent == null then interfaces else parent :: interfaces

  private def refs(bytes: Array[Byte]): Refs =
    val model: jlc.ClassModel = jlc.ClassFile.of().nn.parse(bytes).nn
    val classes: mutable.HashSet[String] = mutable.HashSet()
    val members: mutable.HashSet[(String, String, String)] = mutable.HashSet()
    val declared: mutable.HashSet[(String, String)] = mutable.HashSet()

    def described(descriptor: String): Unit =
      var index: Int = 0

      while index < descriptor.length do
        if descriptor.charAt(index) == 'L' then
          val end: Int = descriptor.indexOf(';', index)
          classes += descriptor.substring(index + 1, end).nn
          index = end + 1
        else index += 1

    def classed(entry: jlcc.ClassEntry): Unit =
      val name: String = entry.asInternalName.nn
      if name.startsWith("[") then described(name) else classes += name

    def member(entry: jlcc.MemberRefEntry): Unit =
      val owner: String = entry.owner.nn.asInternalName.nn
      val descriptor: String = entry.`type`.nn.stringValue.nn
      classed(entry.owner.nn)
      described(descriptor)
      if !owner.startsWith("[") then members += ((owner, entry.name.nn.stringValue.nn, descriptor))

    def loadable(entry: jlcc.PoolEntry): Unit = entry match
      case entry: jlcc.ConstantDynamicEntry =>
        bootstrap(entry.bootstrap.nn)
        described(entry.`type`.nn.stringValue.nn)

      case entry: jlcc.ClassEntry           => classed(entry)
      case entry: jlcc.MethodTypeEntry      => described(entry.descriptor.nn.stringValue.nn)
      case entry: jlcc.MethodHandleEntry    => member(entry.reference.nn)
      case _                                => ()

    def bootstrap(entry: jlc.BootstrapMethodEntry): Unit =
      member(entry.bootstrapMethod.nn.reference.nn)
      entry.arguments.nn.asScala.foreach(loadable)

    def verified(info: jlca.StackMapFrameInfo.VerificationTypeInfo): Unit = info match
      case info: jlca.StackMapFrameInfo.ObjectVerificationTypeInfo => classed(info.className.nn)
      case _                                                       => ()

    def coded(code: jlc.CodeModel): Unit =
      code.forEach:
        case instruction: jlci.FieldInstruction             => member(instruction.field.nn)
        case instruction: jlci.InvokeInstruction            => member(instruction.method.nn)
        case instruction: jlci.NewObjectInstruction         => classed(instruction.className.nn)
        case instruction: jlci.NewReferenceArrayInstruction => classed(instruction.componentType.nn)
        case instruction: jlci.NewMultiArrayInstruction     => classed(instruction.arrayType.nn)
        case instruction: jlci.TypeCheckInstruction         => classed(instruction.`type`.nn)
        case instruction: jlci.ExceptionCatch               =>
          val caught = instruction.catchType.nn
          if caught.isPresent then classed(caught.get.nn)

        case instruction: jlci.InvokeDynamicInstruction =>
          described(instruction.`type`.nn.stringValue.nn)
          bootstrap(instruction.invokedynamic.nn.bootstrap.nn)

        case instruction: jlci.ConstantInstruction.LoadConstantInstruction =>
          loadable(instruction.constantEntry.nn)

        case _ =>
          ()

      code match
        case code: jlca.CodeAttribute =>
          code.findAttribute(jlc.Attributes.stackMapTable).nn.ifPresent: table =>
            table.nn.entries.nn.asScala.foreach: frame =>
              frame.locals.nn.asScala.foreach(verified)
              frame.stack.nn.asScala.foreach(verified)

        case _ =>
          ()

    val parent = model.superclass.nn
    val superclass: String | Null = if parent.isPresent then parent.get.nn.asInternalName else null
    val interfaces: List[String] = model.interfaces.nn.asScala.toList.map(_.asInternalName.nn)
    if superclass != null then classes += superclass
    classes ++= interfaces

    model.fields.nn.asScala.foreach: field =>
      val descriptor: String = field.fieldType.nn.stringValue.nn
      declared += ((field.fieldName.nn.stringValue.nn, descriptor))
      described(descriptor)

    model.methods.nn.asScala.foreach: method =>
      val descriptor: String = method.methodType.nn.stringValue.nn
      declared += ((method.methodName.nn.stringValue.nn, descriptor))
      described(descriptor)
      method.code.nn.ifPresent { code => coded(code.nn) }

      method.findAttribute(jlc.Attributes.exceptions).nn.ifPresent: thrown =>
        thrown.nn.exceptions.nn.asScala.foreach(classed)

    model.findAttribute(jlc.Attributes.nestHost).nn.ifPresent: host =>
      classed(host.nn.nestHost.nn)

    val name: String = model.thisClass.nn.asInternalName.nn

    // A class which is nested in another is asked for that one by `getSimpleName`.
    model.findAttribute(jlc.Attributes.innerClasses).nn.ifPresent: inner =>
      inner.nn.classes.nn.asScala.foreach: info =>
        if info.innerClass.nn.asInternalName == name
        then info.outerClass.nn.ifPresent { outer => classed(outer.nn) }

    Refs
      ( name, model.flags.nn.flagsMask, model.majorVersion, model.minorVersion, superclass,
        interfaces, classes.toSet - name, members.toSet, declared.toSet )

  // A class file which says nothing of classes that are not there to be asked for: the classes
  // nested in it, its nest and its permitted subclasses are all loaded by reflection which
  // asks for them.
  private def trimmed(bytes: Array[Byte], present: String => Boolean): Array[Byte] =
    val file: jlc.ClassFile = jlc.ClassFile.of().nn
    val model: jlc.ClassModel = file.parse(bytes).nn
    val name: String = model.thisClass.nn.asInternalName.nn
    def absent(entry: jlcc.ClassEntry): Boolean = !present(entry.asInternalName.nn)

    def nested(info: jlca.InnerClassInfo): Boolean =
      val outer = info.outerClass.nn
      outer.isPresent && outer.get.nn.asInternalName == name && absent(info.innerClass.nn)

    val inner = model.findAttribute(jlc.Attributes.innerClasses).nn
    val nest = model.findAttribute(jlc.Attributes.nestMembers).nn
    val permitted = model.findAttribute(jlc.Attributes.permittedSubclasses).nn

    val stale: Boolean =
      inner.isPresent && inner.get.nn.classes.nn.asScala.exists(nested) ||
        nest.isPresent && nest.get.nn.nestMembers.nn.asScala.exists(absent) ||
        permitted.isPresent && permitted.get.nn.permittedSubclasses.nn.asScala.exists(absent)

    if !stale then bytes else
      val transform: jlc.ClassTransform = (builder, element) =>
        element match
          case attribute: jlca.InnerClassesAttribute =>
            val kept = attribute.classes.nn.asScala.filterNot(nested).asJava
            builder.nn.`with`(jlca.InnerClassesAttribute.of(kept))

          case attribute: jlca.NestMembersAttribute =>
            val kept = attribute.nestMembers.nn.asScala.filterNot(absent).asJava
            builder.nn.`with`(jlca.NestMembersAttribute.of(kept))

          case attribute: jlca.PermittedSubclassesAttribute =>
            val kept = attribute.permittedSubclasses.nn.asScala.filterNot(absent).asJava
            builder.nn.`with`(jlca.PermittedSubclassesAttribute.of(kept))

          case element =>
            builder.nn.`with`(element)

        ()

      file.transformClass(model, transform).nn

  // A class of the same name, flags and parents, with nothing in it.
  private def shell(refs: Refs): Array[Byte] =
    def described(name: String): jlcn.ClassDesc = jlcn.ClassDesc.ofInternalName(name).nn

    jlc.ClassFile.of().nn.build
      ( described(refs.name),
        builder =>
          builder.nn.withVersion(refs.major, refs.minor)
          builder.nn.withFlags(refs.flags)
          val parent: String | Null = refs.superclass
          if parent != null then builder.nn.withSuperclass(described(parent))
          builder.nn.withInterfaceSymbols(refs.interfaces.map(described)*) )
    . nn

  // Whether a class file names a class which is of interest at all, as a class or within a
  // descriptor, which is cheap to tell.
  private def mentions(bytes: Array[Byte], interesting: String => Boolean): Boolean =
    val pool = jlc.ClassFile.of().nn.parse(bytes).nn.constantPool.nn

    def described(text: String): Boolean =
      var index: Int = text.indexOf('L')
      var found: Boolean = false

      while !found && index >= 0 do
        val end: Int = text.indexOf(';', index)

        if end < 0 then index = -1 else
          found = interesting(text.substring(index + 1, end).nn)
          index = text.indexOf('L', index + 1)

      found

    pool.iterator.nn.asScala.exists:
      case entry: jlcc.ClassEntry => interesting(entry.asInternalName.nn)
      case entry: jlcc.Utf8Entry  => described(entry.stringValue.nn)
      case _                      => false

  // The classes a class file says it inherits from.
  private def parents(bytes: Array[Byte]): List[String] =
    val model: jlc.ClassModel = jlc.ClassFile.of().nn.parse(bytes).nn
    val parent = model.superclass.nn
    val interfaces: List[String] = model.interfaces.nn.asScala.toList.map(_.asInternalName.nn)
    if parent.isPresent then parent.get.nn.asInternalName.nn :: interfaces else interfaces

  // The resources which are for a compiler, or for the archive they came in.
  private def discarded(name: String): Boolean =
    name.endsWith(tastySuffix) || name.endsWith(".betasty") || name.endsWith(".sjsir") ||
      name.endsWith(".nir") || name.endsWith(".semanticdb") || name == "META-INF/MANIFEST.MF" ||
      name == "module-info.class" || name.startsWith("META-INF/versions/") ||
      name.startsWith("META-INF/givens/") || name.startsWith("META-INF/services/") ||
      name.startsWith("META-INF/") &&
        List(".SF", ".DSA", ".RSA", ".EC").exists(name.endsWith(_))

  // The entries a classloader loads from, where it says — a host which was given a classpath
  // has no need to ask — and the JVM's own otherwise; each as an absolute path.
  def classpath(loader: ClassLoader): List[String] = loader match
    case loader: java.net.URLClassLoader =>
      loader.getURLs.nn.toList.map { url => jnf.Path.of(url.nn.toURI).nn.toString }

    case _ =>
      System.getProperty("java.class.path").nn.split(java.io.File.pathSeparator).nn.toList
      . map { entry => jnf.Path.of(entry.nn).nn.toAbsolutePath.nn.toString }

  // Shrinks a classpath. `progress` is told what is being done, as it is begun: a large
  // classpath takes minutes, most of them in the compiler.
  def shrink(request: Request, progress: String => Unit = _ => ()): Report =
    val sources: List[Source] = request.classpath.map(new Source(_))
    try shrink(request, sources, Nil, Nil, 0, progress) finally sources.foreach(_.close())

  private def shrink
    ( request:  Request,
      sources:  List[Source],
      unfit:    List[String],
      notes:    List[String],
      attempt:  Int,
      progress: String => Unit )
  :   Report =

    // The first entry to hold a name is the one a classloader would read it from.
    val holders: mutable.LinkedHashMap[String, Source] = mutable.LinkedHashMap()
    sources.foreach: source =>
      source.names.foreach { name => holders.getOrElseUpdate(name, source) }


    val keeps: List[java.util.regex.Pattern] = request.keep.map(pattern)
    val pins: List[java.util.regex.Pattern] = (request.pin ::: unfit).map(pattern)
    def dotted(name: String): String = name.replace('/', '.').nn
    def matched(patterns: List[java.util.regex.Pattern], name: String): Boolean =
      patterns.exists(_.matcher(name).nn.matches)

    // The units which can be recompiled: those this compiler compiled, which are not the
    // language's own library, nor pinned.
    val pinned: mutable.ListBuffer[String] = mutable.ListBuffer()

    val units: Set[String] =
      holders.iterator.flatMap: (name, source) =>
        if !name.endsWith(tastySuffix) then None else
          val unit: String = name.dropRight(tastySuffix.length)
          val standard: Boolean = unit.startsWith("scala/") || unit.startsWith("dotty/")

          def held(name: String): Boolean = holders.get(name+classSuffix).contains(source)

          if standard || !held(unit) && !held(unit+"$") then None
          else if tooling(source.read(name)) != Shaker.compiler then None
          else if matched(pins, dotted(unit)) then
            pinned += dotted(unit)
            None
          else Some(unit)
      . toSet

    // The unit a class file was compiled from, if it is one to recompile: the longest unit
    // whose name the class's begins with.
    def unit(name: String): Option[String] =
      def recur(prefix: String): Option[String] =
        if units.contains(prefix) then Some(prefix) else
          val cut: Int = prefix.lastIndexOf('$')
          if cut <= prefix.lastIndexOf('/') then None else recur(prefix.substring(0, cut).nn)

      recur(name)

    val classes: List[String] =
      holders.keysIterator.filter { name => name.endsWith(classSuffix) && !discarded(name) }
      . map(_.dropRight(classSuffix.length)).toList

    val recompilable: Set[String] = classes.filter(unit(_).isDefined).toSet
    val parsed: mutable.HashMap[String, Refs] = mutable.HashMap()

    def original(name: String): Option[Refs] =
      holders.get(name+classSuffix).map: source =>
        parsed.getOrElseUpdate(name, refs(source.read(name+classSuffix)))

    def failure(problems: List[String], read: Int): Report =
      Report
        ( problems, units.size, read, classes.length, 0L, 0, 0L, 0, 0, 0, 0, 0, pinned.toList,
          notes, Nil, Map() )

    // Services: the implementations each file names, over every entry which has the file.
    val services: mutable.LinkedHashMap[String, mutable.LinkedHashSet[String]] =
      mutable.LinkedHashMap()

    sources.foreach: source =>
      source.names.filter(_.startsWith("META-INF/services/")).foreach: name =>
        val lines: List[String] =
          new String(source.read(name), "UTF-8").split("\n").nn.toList.map(_.nn.trim.nn)
          . map: line =>
              val comment: Int = line.indexOf('#')
              if comment < 0 then line else line.substring(0, comment).nn.trim.nn
          . filter(_.nonEmpty)

        services.getOrElseUpdate(name, mutable.LinkedHashSet()) ++= lines

    // Roots
    val roots: mutable.LinkedHashSet[Shaker.Root] = mutable.LinkedHashSet()
    val entries: mutable.LinkedHashSet[String] = mutable.LinkedHashSet()

    request.mains.foreach: main =>
      roots += Shaker.Root(main, "main", false)
      entries += main.replace('.', '/').nn

    if request.services then
      services.valuesIterator.flatten.foreach: implementation =>
        roots += Shaker.Root(implementation, "", false)
        entries += implementation.replace('.', '/').nn

    classes.foreach: name =>
      if matched(keeps, dotted(name)) then
        roots += Shaker.Root(dotted(name), "", true)
        entries += name

    // A class which is not recompiled may refer to one which is, and what it refers to must
    // then be kept whether or not anything reaches the class itself.
    val foreign: List[String] = classes.filterNot(recompilable.contains)

    // The classes of the classpath which a class inherits from, itself among them.
    val ancestries: mutable.HashMap[String, Set[String]] = mutable.HashMap()

    def ancestry(name: String): Set[String] =
      def recur(name: String, seen: mutable.HashSet[String]): Unit =
        if seen.add(name) then original(name).foreach(_.parents.foreach(recur(_, seen)))

      ancestries.getOrElseUpdate
        ( name,
          locally:
            val seen: mutable.HashSet[String] = mutable.HashSet()
            recur(name, seen)
            seen.toSet )

    // Whether a class is one which is recompiled, or inherits from one: what refers to it
    // may be referring to something of a class which is recompiled.
    val descents: mutable.HashMap[String, Boolean] = mutable.HashMap()

    def descends(name: String): Boolean =
      recompilable.contains(name) || descents.get(name).getOrElse:
        // Until it is known, a class does not descend from itself.
        descents(name) = false

        val result: Boolean =
          holders.get(name+classSuffix).exists: source =>
            parents(source.read(name+classSuffix)).exists(descends)

        descents(name) = result
        result

    if recompilable.nonEmpty then
      foreign.foreach: name =>
        holders.get(name+classSuffix).foreach: source =>
          // Most classes of a library name nothing of the application.
          if mentions(source.read(name+classSuffix), descends) then
            original(name).foreach: found =>
              // A member may be named as one of a class which only inherits it.
              found.members.foreach: (owner, member, _) =>
                ancestry(owner).foreach: ancestor =>
                  if recompilable.contains(ancestor)
                  then roots += Shaker.Root(dotted(ancestor), member, false)

              // A class which is named may be instantiated, and a class which inherits from
              // one that is recompiled is an instance of it which no tree shows.
              found.classes.foreach: named =>
                if recompilable.contains(named) then roots += Shaker.Root(dotted(named), "", false)

              if (ancestry(name) - name).exists(recompilable.contains)
              then roots += Shaker.Root(dotted(name), "", false)

    val absent: List[String] =
      request.mains.filterNot { main => holders.contains(main.replace('.', '/').nn+classSuffix) }

    // Compilation
    val staging: jnf.Path = jnf.Files.createTempDirectory("abscission").nn

    // What the compiler writes is read back from a directory of its own, which does not outlive
    // this.
    def cleanup(): Unit =
      if jnf.Files.exists(staging) then
        val walk = jnf.Files.walk(staging).nn
        try walk.sorted(java.util.Comparator.reverseOrder()).nn.forEach(jnf.Files.delete(_))
        finally walk.close()

    try
      val outcome: Shaker.Outcome =
        if absent.nonEmpty then
          val problems: List[Shaker.Problem] = absent.map: main =>
            Shaker.Problem("", 0, s"the entry point $main is not a class of the classpath")

          Shaker.Outcome(problems, Shaker.Tally(0, 0, 0, 0, 0, 0), Nil)
        else if units.isEmpty then Shaker.Outcome(Nil, Shaker.Tally(0, 0, 0, 0, 0, 0), Nil)
        else
          progress(s"recompiling what ${roots.size} roots reach, of ${units.size} units")

          Shaker.shake
            ( units.toList.sorted.map(dotted), roots.toList, request.classpath, staging.toString,
              request.options, progress )

      def emitted(): Map[String, jnf.Path] =
        val walk = jnf.Files.walk(staging).nn

        try
          walk.iterator.nn.asScala.toList.filter(_.toString.endsWith(classSuffix)).map: file =>
            val name: String = staging.relativize(file).nn.toString.replace('\\', '/').nn
            name.dropRight(classSuffix.length) -> file
          . toMap
        finally walk.close()

      // A unit which fails to recompile is one to copy as it is instead; whatever it refers to
      // is then kept, and the rest is shrunk again.
      val failed: List[String] =
        if outcome.problems.isEmpty || attempt >= 3 then Nil else
          val files: Set[String] = outcome.problems.map(_.file).filter(_.nonEmpty).toSet

          if files.isEmpty || outcome.problems.exists(_.file.isEmpty) then Nil else
            units.toList.filter: unit =>
              holders.get(unit+tastySuffix).exists: holder =>
                val recorded: String | Null = source(holder.read(unit+tastySuffix))
                recorded != null &&
                  files.exists { file => file == recorded || file.endsWith("/"+recorded) }
            . map(dotted)

      def described: List[String] = outcome.problems.map: problem =>
        if problem.file.isEmpty then problem.message
        else s"${problem.file}:${problem.line}: ${problem.message}"

      if failed.nonEmpty then
        progress(s"pinning ${failed.mkString(", ")}, which could not be recompiled")
        shrink(request, sources, unfit ::: failed, notes ::: described, attempt + 1, progress)
      else if outcome.problems.nonEmpty then
        failure(described, outcome.tally.units)
      else
        val output: jnf.Path = jnf.Path.of(request.output).nn
        val produced: Map[String, jnf.Path] = emitted()
        progress(s"following the references of ${produced.size} recompiled classes")
        val reasons: mutable.HashMap[String, String] = mutable.HashMap()
        val included: mutable.LinkedHashMap[String, Refs] = mutable.LinkedHashMap()
        val contents: mutable.HashMap[String, Array[Byte]] = mutable.HashMap()
        val shells: mutable.HashSet[String] = mutable.HashSet()
        val whole: mutable.HashSet[Source] = mutable.HashSet()
        val queue: mutable.ArrayDeque[String] = mutable.ArrayDeque()
        val problems: mutable.ListBuffer[String] = mutable.ListBuffer()

        // Whether a library is one to take a class at a time: one of Scala, which has TASTy.
        val scala: mutable.HashMap[Source, Boolean] = mutable.HashMap()
        def fine(source: Source): Boolean =
          request.fine ||
            scala.getOrElseUpdate(source, source.names.exists(_.endsWith(tastySuffix)))

        def reach(name: String, from: String): Unit =
          if !included.contains(name) && !reasons.contains(name) then
            reasons(name) = from
            queue += name

        def follow(): Unit =
          while queue.nonEmpty do
            val name: String = queue.removeHead()

            if !included.contains(name) then
              produced.get(name) match
                case Some(file) =>
                  val bytes: Array[Byte] = jnf.Files.readAllBytes(file).nn
                  val found: Refs = refs(bytes)
                  included(name) = found
                  contents(name) = bytes
                  found.classes.foreach(reach(_, name))

                case None =>
                  holders.get(name+classSuffix).foreach: source =>
                    if recompilable.contains(name) then
                      original(name).foreach: found =>
                        included(name) =
                          found.copy
                            ( classes = found.parents.toSet, members = Set(), declared = Set() )

                        contents(name) = shell(found)
                        shells += name
                        found.parents.foreach(reach(_, name))
                    else
                      val bytes: Array[Byte] = source.read(name+classSuffix)
                      val found: Refs = parsed.getOrElseUpdate(name, refs(bytes))
                      included(name) = found
                      contents(name) = bytes
                      found.classes.foreach(reach(_, name))

                      if !fine(source) && whole.add(source) then
                        source.names.foreach: other =>
                          val own: Boolean = holders.get(other).contains(source)

                          if other.endsWith(classSuffix) && !discarded(other) && own
                          then reach(other.dropRight(classSuffix.length), name)

        produced.keysIterator.foreach(reach(_, ""))
        entries.foreach(reach(_, ""))
        outcome.dynamic.foreach { name => reach(name.replace('.', '/').nn, "") }
        pinned.foreach: unit =>
          val prefix: String = unit.replace('.', '/').nn
          classes.foreach: name =>
            if name == prefix || name.startsWith(prefix+"$") then reach(name, "")


        follow()

        // An implementation of a service which is kept, in a library which is copied, is one
        // its loader may ask for.
        var growing: Boolean = true

        while growing do
          growing = false

          services.foreach: (file, implementations) =>
            val service: String = file.drop("META-INF/services/".length).replace('.', '/').nn

            if included.contains(service) || !holders.contains(service+classSuffix) then
              implementations.foreach: implementation =>
                val name: String = implementation.replace('.', '/').nn

                val copied: Boolean =
                  !recompilable.contains(name) && holders.contains(name+classSuffix)

                if !included.contains(name) && copied && request.services then
                  reach(name, service)
                  growing = true

          follow()

        // Every member which is named must be there: declared by a class of the closed world
        // which the named class inherits from, unless it was never declared by a class of the
        // classpath at all, and so is the JDK's.
        type Member = (String, String)

        def declares(name: String, member: Member, seen: mutable.HashSet[String]): Boolean =
          seen.add(name) && included.get(name).exists: found =>
            !shells.contains(name) && found.declared.contains(member) ||
              found.parents.exists(declares(_, member, seen))

        def declared(name: String, member: Member, seen: mutable.HashSet[String]): Boolean =
          seen.add(name) && original(name).exists: found =>
            found.declared.contains(member) || found.parents.exists(declared(_, member, seen))

        val missing: mutable.HashMap[(String, String, String), Boolean] = mutable.HashMap()

        included.foreach: (name, found) =>
          found.members.foreach: (owner, member, descriptor) =>
            if included.contains(owner) then
              val signature: Member = (member, descriptor)

              val lost: Boolean = missing.getOrElseUpdate
                ( (owner, member, descriptor),
                  !declares(owner, signature, mutable.HashSet()) &&
                    (shells.contains(owner) || declared(owner, signature, mutable.HashSet())) )

              if lost then
                val referent: String = s"$member$descriptor of ${dotted(owner)}"
                problems += s"${dotted(name)} refers to $referent, which was removed"

        if problems.isEmpty then
          jnf.Files.createDirectories(output)
          var written: Long = 0L

          def write(name: String, bytes: Array[Byte]): Unit =
            val file: jnf.Path = output.resolve(name).nn
            jnf.Files.createDirectories(file.getParent)
            jnf.Files.write(file, bytes)
            written += bytes.length

          included.keysIterator.foreach: name =>
            write(name+classSuffix, trimmed(contents(name), included.contains))

          // The resources of every entry which has given a class, or which holds classes that
          // were recompiled, or which has no classes to give and is there for its resources.
          val givers: Set[Source] =
            included.keysIterator.flatMap { name => holders.get(name+classSuffix) }.toSet ++
              units.iterator.flatMap { unit => holders.get(unit+tastySuffix) } ++
              sources.filterNot(_.names.exists(_.endsWith(classSuffix)))

          var resources: Long = 0L

          holders.foreach: (name, source) =>
            if !name.endsWith(classSuffix) && !discarded(name) && givers.contains(source) then
              val bytes: Array[Byte] = source.read(name)
              val file: jnf.Path = output.resolve(name).nn
              jnf.Files.createDirectories(file.getParent)
              jnf.Files.write(file, bytes)
              resources += bytes.length

          services.foreach: (file, implementations) =>
            val kept: List[String] =
              implementations.toList.filter { name => included.contains(name.replace('.', '/').nn) }

            if kept.nonEmpty then
              val target: jnf.Path = output.resolve(file).nn
              jnf.Files.createDirectories(target.getParent)
              jnf.Files.writeString(target, kept.mkString("", "\n", "\n"))

          val size: Long =
            classes.iterator.map { name => holders(name+classSuffix).size(name+classSuffix) }.sum

          def packaged(names: Iterable[String]): Map[String, Int] =
            names.groupMapReduce { name => dotted(name.take(name.lastIndexOf('/').max(0))) }
              ( _ => 1 )
              ( _ + _ )

          val before: Map[String, Int] = packaged(classes)
          val after: Map[String, Int] = packaged(included.keys)

          val packages: List[(String, Int, Int)] =
            before.toList.sortBy(_(0)).map: (name, count) =>
              (name, count, after.getOrElse(name, 0))

          Report
            ( Nil, units.size, outcome.tally.units, classes.length, size, included.size, written,
              produced.size, shells.size, included.size - produced.size - shells.size,
              outcome.tally.members, outcome.tally.removed, pinned.toList, notes, packages,
              reasons.iterator.filter(_(1).nonEmpty)
              . map { (name, from) => dotted(name) -> dotted(from) }.toMap )
        else
          failure(problems.toList, outcome.tally.units)
    finally cleanup()
