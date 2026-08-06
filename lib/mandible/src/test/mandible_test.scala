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
package mandible

import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.IteratorHasAsScala

import soundness.*
import galilei.Linux.pathOnLinux

import proscenium.compat.*

import alphabets.hexLowerCase
import classloaders.threadContextClassloader
import logging.silentLogging
import probates.cancelProbate
import strategies.throwUnsafely
import systems.javaSystem
import temporaryDirectories.systemTemporaryDirectory
import threading.platformThreading

object Tests extends Suite(m"Mandible tests"):

  // Java, not Scala, for the classfile fixtures: `classfile/1` is a contract over bytecode, and
  // Java is the language whose surface maps onto bytecode without a compilation scheme in
  // between — `static final` constants, `protected` members and bridge methods all say what they
  // mean here.
  val base: Text =
    t"""|package fixture;
        |public class Base {
        |  public static final int CONSTANT = 7;
        |  public int inherited() { return 1; }
        |  protected int guarded() { return 2; }
        |  private int hidden() { return 3; }
        |}
        |""".s.stripMargin.tt

  val derived: Text =
    t"""|package fixture;
        |public class Derived extends Base {
        |  public int own() { return 4; }
        |}
        |""".s.stripMargin.tt

  val api: Text =
    t"""|package fixture;
        |public interface Api {
        |  int one();
        |}
        |""".s.stripMargin.tt

  val holder: Text =
    t"""|package fixture;
        |public class Holder<T> {
        |  public Object get() { return null; }
        |}
        |""".s.stripMargin.tt

  // Fixture variants are made by editing the source text, so each test states exactly the one
  // change whose grade it is asserting.
  def edit(source: Text, from: Text, to: Text): Text = source.s.replace(from.s, to.s).nn.tt

  def sources(base: Text, derived: Text, api: Text, holder: Text = holder): Map[Text, Text] =
    Map
      ( t"fixture/Base.java"    -> base,
        t"fixture/Derived.java" -> derived,
        t"fixture/Api.java"     -> api,
        t"fixture/Holder.java"  -> holder )

  def run(): Unit =
    classfileDisciplineTests()
    jvmProfileTests()
    test(m"Locate a known method on a classfile"):
      val rewrite =
        Classfile[StackTrace].let(_.methods.stdlib.find(_.name == t"rewrite").getOrElse(Unset)).vouch
    . assert()

    test(m"Disassemble a known method's bytecode"):
      val bytecode =
        Classfile[StackTrace]
        . let(_.methods.stdlib.find(_.name == t"rewrite").getOrElse(Unset))
        . let(_.bytecode)
        . vouch
      bytecode.instructions.size
    . assert(_ > 0)

    test(m"Bytecode carries declared maxStack and maxLocals"):
      val bytecode =
        Classfile[StackTrace]
        . let(_.methods.stdlib.find(_.name == t"rewrite").getOrElse(Unset))
        . let(_.bytecode)
        . vouch
      (bytecode.maxStack, bytecode.maxLocals)
    . assert((s, l) => s >= 0 && l >= 0)

    test(m"Method descriptor parser handles primitives and references"):
      Bytecode.Descriptor.parse(t"(Ljava/lang/String;I)V")
    . assert: parsed =>
        parsed.args.size == 2 && parsed.result.absent

    test(m"Method descriptor parser handles array types and return"):
      Bytecode.Descriptor.parse(t"([[Ljava/lang/Object;J)Z")
    . assert: parsed =>
        parsed.args.size == 2 && parsed.result == Bytecode.Frame.Z

    test(m"Detect virtual call as effectively static when receiver is a singleton"):
      // Construct: GETSTATIC Foo$.MODULE$:LFoo$;  followed by  INVOKEVIRTUAL Foo$.doIt()V
      val moduleFrame = Bytecode.Frame.L(t"Foo$$")
      val getstatic =
        Bytecode.Instruction
          ( Bytecode.Opcode.Getstatic(t"Foo$$", t"MODULE$$", t"LFoo$$;"),
            Unset,
            proscenium.List(moduleFrame),
            0 )

      val invoke =
        Bytecode.Instruction
          ( Bytecode.Opcode.Invokevirtual(t"Foo$$", t"doIt", t"()V"),
            Unset,
            Nil,
            3 )

      Bytecode(Unset, List(getstatic, invoke), 1, 0).effectivelyStaticCalls
    . assert(_ == Set(3))

    test(m"A virtual call on an opaque receiver is not flagged as static"):
      val opaqueFrame = Bytecode.Frame.L(t"?")
      val getstatic =
        Bytecode.Instruction
          ( Bytecode.Opcode.Getstatic(t"Bar", t"thing", t"Ljava/lang/Object;"),
            Unset,
            proscenium.List(opaqueFrame),
            0 )

      val invoke =
        Bytecode.Instruction
          ( Bytecode.Opcode.Invokevirtual(t"Foo$$", t"doIt", t"()V"),
            Unset,
            Nil,
            3 )

      Bytecode(Unset, List(getstatic, invoke), 1, 0).effectivelyStaticCalls
    . assert(_.stdlib.isEmpty)

    test(m"Linearizer inlines a resolvable static-dispatchable call"):
      val moduleFrame = Bytecode.Frame.L(t"Foo$$")
      val getstatic =
        Bytecode.Instruction
          ( Bytecode.Opcode.Getstatic(t"Foo$$", t"MODULE$$", t"LFoo$$;"),
            Unset,
            proscenium.List(moduleFrame),
            0 )

      val invoke =
        Bytecode.Instruction
          ( Bytecode.Opcode.Invokevirtual(t"Foo$$", t"doIt", t"()V"),
            Unset,
            Nil,
            3 )

      val caller = Bytecode(Unset, List(getstatic, invoke), 1, 0)

      val calleeBody = proscenium.List
        ( Bytecode.Instruction(Bytecode.Opcode.Iconst1, Unset, proscenium.List(Bytecode.Frame.I), 0),
          Bytecode.Instruction(Bytecode.Opcode.Ireturn, Unset, Nil, 1) )
      val callee = Bytecode(Unset, calleeBody, 1, 0)

      val resolver: (Text, Text, Text) => Optional[Bytecode] =
        (o, n, d) => if o == t"Foo$$" && n == t"doIt" then callee else Unset

      caller.linearize(resolver, maxDepth = 2).map(_.depth)
    . assert(_ == List(0, 0, 1, 1))

    test(m"Linearizer respects maxInstructions budget"):
      val noop =
        Bytecode.Instruction(Bytecode.Opcode.Nop, Unset, Nil, 0)
      val caller = Bytecode(Unset, List.fill(50)(noop), 0, 0)

      caller.linearize((_, _, _) => Unset, maxInstructions = 7).size
    . assert(_ == 7)

  def classfileDisciplineTests(): Unit =
    import reliquary.*

    // Compiles the fixtures and returns the emitted classfiles as discipline content, plus the
    // output directory to use as the atomization classpath.
    def compile(base: Text, derived: Text, api: Text, holder: Text = holder) =
      // The compilation runs inside `supervise`, but the classfile bytes are read outside it:
      // `Data` is a frozen-array capability, and returning one out of the supervisor's scope
      // freshens its capture set past what the enclosing method can admit.
      val out: Text = supervise:
        val directory: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())
        Files.createDirectories(Paths.get(directory.encode.s))
        Javac(Nil)(LocalClasspath())(sources(base, derived, api, holder), directory).complete()

        directory.encode

      val root = Paths.get(out.s).nn

      val content = Files.walk(root).nn.iterator.nn.asScala
        . to(scala.List)
        . filter { path => path.toString.endsWith(".class") }
        . sortBy(_.toString)
        . map: path =>
            val name = Text(root.relativize(path).nn.toString)
            (TreePath(name), Array.unsafeFrozen(Files.readAllBytes(path).nn))

      (List.from(content), out)

    def atomize(base: Text, derived: Text, api: Text, holder: Text = holder): Atomization =
      val (content, out) = compile(base, derived, api, holder)
      ClassfileDiscipline.atomize(content, Discipline.Context(t"jvm", classpath = List(out)))

    def listing(atomization: Atomization): scala.List[(Text, Text)] =
      atomization.atoms.stdlib
      . map { atom => (atom.key, atom.valueHash.serialize[Hex]) }
      . sortBy(_(0).s)

    val baseline = atomize(base, derived, api)
    val keys = listing(baseline).map(_(0).s).toSet

    test(m"a compiled fixture yields a nonempty atom listing"):
      baseline.atoms.stdlib.size
    . assert(_ > 0)

    test(m"each compiled class yields a class atom"):
      (keys.contains("fixture/Base"), keys.contains("fixture/Derived"), keys.contains("fixture/Api"))
    . assert(_ == (true, true, true))

    test(m"a declared method yields a member atom keyed by its owner"):
      keys.contains("fixture/Base#inherited:()I")
    . assert(identity)

    test(m"membership keying presents an inherited method under the subclass"):
      keys.contains("fixture/Derived#inherited:()I")
    . assert(identity)

    test(m"membership keying presents an inherited protected method too"):
      keys.contains("fixture/Derived#guarded:()I")
    . assert(identity)

    test(m"a private method is not consumer surface and yields no atom"):
      keys.exists(_.contains("hidden"))
    . assert(_ == false)

    test(m"a constructor is not inherited by the subclass"):
      keys.contains("fixture/Derived#<init>:()V") && !keys.contains("fixture/Derived#Base:()V")
    . assert(identity)

    test(m"an inherited member's atom differs in value from the declared one"):
      val table = listing(baseline).toMap
      table.get(t"fixture/Base#inherited:()I") != table.get(t"fixture/Derived#inherited:()I")
    . assert(identity)

    // A constant presents through the subclass too, and javac will have inlined `Derived.CONSTANT`
    // into consumers just as readily as `Base.CONSTANT`, so both are replaceable.
    test(m"a static final constant is replaceable wherever it presents"):
      baseline.replaceable.stdlib.map(_.key.s).toSet
    . assert(_ == Set("fixture/Base.CONSTANT:I", "fixture/Derived.CONSTANT:I"))

    test(m"atomization is deterministic across separate compilations"):
      listing(atomize(base, derived, api)) == listing(atomize(base, derived, api))
    . assert(identity)

    // The §12.3 grades, computed over real bytecode.

    def grade(base2: Text, derived2: Text, api2: Text): Grade =
      Grade.between(List(baseline), List(atomize(base2, derived2, api2)))

    test(m"an unchanged release grades as a patch"):
      grade(base, derived, api)
    . assert(_ == Grade.Patch)

    test(m"adding a concrete method grades as a minor"):
      val added = t"public int added() { return 9; }\n  public int inherited"
      grade(edit(base, t"public int inherited", added), derived, api)
    . assert(_ == Grade.Minor)

    test(m"changing a static final constant's value grades as a minor"):
      grade(edit(base, t"CONSTANT = 7", t"CONSTANT = 8"), derived, api)
    . assert(_ == Grade.Minor)

    test(m"removing a protected method grades as a major"):
      grade(edit(base, t"protected int guarded() { return 2; }", t""), derived, api)
    . assert(_ == Grade.Major)

    test(m"narrowing a method's accessibility grades as a major"):
      grade(edit(base, t"protected int guarded", t"private int guarded"), derived, api)
    . assert(_ == Grade.Major)

    test(m"adding an abstract method to an open interface grades as a major"):
      grade(base, derived, edit(api, t"int one();", t"int one();\n  int two();"))
    . assert(_ == Grade.Major)

    // Registry ordering (§11.2): `tasty/1` claims `.class` atomless, and the registry claims by
    // first match, so a registry that lists it first leaves `classfile/1` nothing to atomize.

    test(m"the discipline claims classfiles and nothing else"):
      val data = Array.freeze(Array[Byte](0))

      (ClassfileDiscipline.claims(TreePath(t"fixture/Base.class"), data),
       ClassfileDiscipline.claims(TreePath(t"fixture/Base.tasty"), data),
       ClassfileDiscipline.claims(TreePath(t"readme.md"), data))
    . assert(_ == (true, false, false))

    test(m"the discipline claims nothing outside the jvm universe"):
      val (content, out) = compile(base, derived, api)
      val registry = Discipline.Registry(List(ClassfileDiscipline))
      val context = Discipline.Context(t"sjsir", classpath = List(out))

      registry.atomize(content, context).stdlib.map(_.discipline)
    . assert(_ == scala.List(t"opaque/1"))

    test(m"a registry listing the discipline first atomizes the classfiles"):
      val (content, out) = compile(base, derived, api)
      val registry = Discipline.Registry(List(ClassfileDiscipline))
      val context = Discipline.Context(t"jvm", classpath = List(out))

      registry.atomize(content, context).stdlib.map: atomization =>
        (atomization.discipline, atomization.atoms.stdlib.size > 0)
    . assert(_ == scala.List((t"classfile/1", true)))

  def jvmProfileTests(): Unit =
    import reliquary.*

    def compile(base: Text, derived: Text, api: Text, holder: Text = holder) =
      val out: Text = supervise:
        val directory: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())
        Files.createDirectories(Paths.get(directory.encode.s))
        Javac(Nil)(LocalClasspath())(sources(base, derived, api, holder), directory).complete()

        directory.encode

      val root = Paths.get(out.s).nn

      val content = Files.walk(root).nn.iterator.nn.asScala
        . to(scala.List)
        . filter { path => path.toString.endsWith(".class") }
        . sortBy(_.toString)
        . map: path =>
            val name = Text(root.relativize(path).nn.toString)
            (TreePath(name), Array.unsafeFrozen(Files.readAllBytes(path).nn))

      (List.from(content), out)

    def evidence(base: Text, derived: Text, api: Text, holder: Text = holder) =
      val (content, out) = compile(base, derived, api, holder)

      EcosystemProfile.Evidence
        (List(EcosystemProfile.Section(t"jvm", content, classpath = List(out))))

    def atomize(base: Text, derived: Text, api: Text, holder: Text = holder): Atomization =
      val (content, out) = compile(base, derived, api, holder)
      ClassfileDiscipline.atomize(content, Discipline.Context(t"jvm", classpath = List(out)))

    val before = evidence(base, derived, api)

    def violations(base2: Text, derived2: Text, api2: Text): scala.List[Text] =
      JvmProfile.check(before, evidence(base2, derived2, api2)).stdlib.map(_.detail)

    test(m"the profile certifies linkage and nothing else"):
      (JvmProfile.id, JvmProfile.certifies)
    . assert(_ == (t"jvm/1", Set(Discipline.Guarantee.Linkage)))

    test(m"an unchanged release violates no linkage predicate"):
      violations(base, derived, api)
    . assert(_.isEmpty)

    test(m"adding a concrete method violates no linkage predicate"):
      val added = t"public int added() { return 9; }\n  public int inherited"
      violations(edit(base, t"public int inherited", added), derived, api)
    . assert(_.isEmpty)

    test(m"removing a presented method is a linkage violation"):
      violations(edit(base, t"protected int guarded() { return 2; }", t""), derived, api)
    . assert: details =>
        details.exists(_.s.startsWith("fixture/Base#guarded:()I"))
          && details.exists(_.s.startsWith("fixture/Derived#guarded:()I"))

    test(m"narrowing accessibility is a linkage violation"):
      violations(edit(base, t"protected int guarded", t"private int guarded"), derived, api)
    . assert(_.nonEmpty)

    test(m"changing a method's return type is a linkage violation"):
      violations(edit(base, t"public int inherited() { return 1; }",
          t"public long inherited() { return 1; }"), derived, api)
    . assert(_.nonEmpty)

    test(m"a changed constant is reported apart from the linkage predicates"):
      val changed = edit(base, t"CONSTANT = 7", t"CONSTANT = 8")

      (violations(changed, derived, api),
       JvmProfile.constants(before, evidence(changed, derived, api)).stdlib)
    . assert(_ == (scala.List(),
        scala.List(t"fixture/Base.CONSTANT:I", t"fixture/Derived.CONSTANT:I")))

    // Appendix D.1's second bullet, made executable: a change can break recompilation while
    // leaving linkage untouched. Tightening a class's type-parameter bound rewrites its generic
    // `Signature` attribute and fails every consumer's next compile, but erasure is unchanged, so
    // every descriptor a compiled consumer resolves is byte-identical. The discipline grades it a
    // major; the profile — which reads the linkage-only fold — finds nothing to report. Both are
    // right, which is exactly why the two levels are recorded separately.
    test(m"a recompilation break with no linkage break is graded but not reported"):
      val bounded = edit(holder, t"class Holder<T>", t"class Holder<T extends Number>")

      (Grade.between(List(atomize(base, derived, api)), List(atomize(base, derived, api, bounded))),
       JvmProfile.check(before, evidence(base, derived, api, bounded)).stdlib)
    . assert(_ == (Grade.Major, scala.List()))

    // The audit (L128/L130) is what turns a finding into a verdict about the release.

    test(m"an unrecorded linkage break is rejected"):
      val registry = EcosystemProfile.Registry(List(JvmProfile))
      val declared = List(LiraManifest.Profile(t"jvm/1"))
      val after = evidence(edit(base, t"protected int guarded() { return 2; }", t""), derived, api)

      import errorDiagnostics.stackTracesDiagnostics
      capture[LiraError](EcosystemProfile.audit(registry, declared, before, after)).reason
    . assert(_ == LiraError.Reason.UnrecordedBreak(t"jvm/1", t"linkage"))

    test(m"a recorded linkage break is accepted"):
      val registry = EcosystemProfile.Registry(List(JvmProfile))

      val declared =
        List(LiraManifest.Profile(t"jvm/1", List(LiraManifest.Guarantee.Linkage)))

      val after = evidence(edit(base, t"protected int guarded() { return 2; }", t""), derived, api)

      EcosystemProfile.audit(registry, declared, before, after).unchecked.stdlib
    . assert(_ == scala.List())

    test(m"a declared profile with no implementation is reported, not rejected"):
      val registry = EcosystemProfile.Registry(List(JvmProfile))
      val declared = List(LiraManifest.Profile(t"unknown/1"))

      EcosystemProfile.audit(registry, declared, before, before).unchecked.stdlib
    . assert(_ == scala.List(t"unknown/1"))

    test(m"a violation at an uncertified level is the profile's defect, L128"):
      val broken = new EcosystemProfile:
        def id: Text = t"broken/1"
        def certifies: Set[Discipline.Guarantee] = Set(Discipline.Guarantee.Linkage)

        def check(previous: EcosystemProfile.Evidence, next: EcosystemProfile.Evidence)
        :   List[EcosystemProfile.Violation] raises DisciplineError =
          List(EcosystemProfile.Violation(Discipline.Guarantee.Recompilation, t"out of scope"))

      val registry = EcosystemProfile.Registry(List(broken))
      val declared = List(LiraManifest.Profile(t"broken/1"))

      import errorDiagnostics.stackTracesDiagnostics
      capture[LiraError](EcosystemProfile.audit(registry, declared, before, before)).reason
    . assert(_ == LiraError.Reason.ProfileViolated(t"broken/1", t"out of scope"))

    test(m"the toolchain predicate reports releases with no toolchain record"):
      def data(text: Text): Data = Array.unsafeFrozen(text.s.getBytes("UTF-8").nn)

      def release(module: Text, toolchain: List[LiraManifest.Tool]): LiraManifest =
        LiraManifest(
          module    = module,
          lineage   = List(LiraHash(LiraHash.Domain.Snapshot, data(module))),
          toolchain = toolchain,
          api       = List(),
          section   = List(),
          payload   = LiraManifest.Payload(t"brotli", 0L,
              LiraHash(LiraHash.Domain.Blob, data(module))))

      val tooled = release(t"alpha", List(LiraManifest.Tool(t"scala", t"3.9.0")))
      val bare = release(t"beta", List())

      JvmProfile.coherence(List(tooled, bare)).stdlib.map(_.s.takeWhile(_ != ' '))
    . assert(_ == scala.List("beta"))

    test(m"changed constants surface through the audit's advisory channel"):
      val registry = EcosystemProfile.Registry(List(JvmProfile))
      val declared = List(LiraManifest.Profile(t"jvm/1"))
      val after = evidence(edit(base, t"CONSTANT = 7", t"CONSTANT = 8"), derived, api)

      EcosystemProfile.audit(registry, declared, before, after).advisories.stdlib
    . assert: advisories =>
        advisories.length == 2
          && advisories.forall(_.s.contains("changed value"))

    // --- jsig/1 and host contracts ------------------------------------------------------------

    test(m"jsig claims signature files and classfiles in both its worlds"):
      val data = Array.freeze(Array[Byte](0))

      (JsigDiscipline.claims(TreePath(t"java.base/java/lang/Object.sig"), data),
       JsigDiscipline.claims(TreePath(t"android/view/View.class"), data),
       JsigDiscipline.claims(TreePath(t"readme.md"), data),
       JsigDiscipline.domain.covers(t"host"),
       JsigDiscipline.domain.covers(t"jvm"),
       JsigDiscipline.domain.covers(t"sjsir"))
    . assert(_ == (true, true, false, true, true, false))

    test(m"a supertype outside the claimed content is a boundary, not an error"):
      val (content, _) = compile(base, derived, api)
      val derivedOnly = List.from(content.filter { pair => pair(0).text.s.contains("Derived") })

      // The classpath is empty, so `Base` is unresolvable: `classfile/1` must fail here, and
      // `jsig/1` must not — the presented set simply lacks what the boundary hides.
      val atoms = JsigDiscipline.atomize(derivedOnly, Discipline.Context(t"host")).atoms

      (atoms.stdlib.exists(_.key == t"fixture/Derived"),
       atoms.stdlib.exists(_.key.s.startsWith("fixture/Derived#")),
       atoms.stdlib.exists(_.key.s.startsWith("fixture/Base")))
    . assert(_ == (true, true, false))

    test(m"host contract sequences derive versions, lineages and tags"):
      val (v1, _) = compile(base, derived, api)

      val (v2, _) = compile(edit(base, t"public int inherited",
          t"public int added() { return 9; }\n  public int inherited"), derived, api)

      val (v3, _) = compile(edit(base, t"protected int guarded() { return 2; }", t""),
          derived, api)

      val releases = List(
        HostRelease(t"jdk-17", v1),
        HostRelease(t"jdk-18", v2),
        HostRelease(t"jdk-19", v3))

      val liras = HostContracts.assemble(t"fixture-host", releases,
        List(LiraManifest.Tool(t"jsig-harvest", t"0.1")),
        allowMajor = { tag => tag == t"jdk-19" })

      val parsed = liras.stdlib.map { (tag, bytes) => (tag, Lira.read(bytes)) }
      parsed.foreach { (_, lira) => Verification.install(lira) }

      val versions = parsed.map: (_, lira) =>
        lira.manifest.version.let { v => s"${v.major}.${v.minor}.${v.patch}" }.or("")

      (versions,
       parsed.map { (_, lira) => lira.manifest.lineage.stdlib.size },
       parsed.flatMap { (_, lira) => lira.manifest.tag.stdlib },
       parsed.forall { (_, lira) => lira.manifest.hostContract })
    . assert(_ == (scala.List("0.1.0", "0.2.0", "0.3.0"), scala.List(1, 2, 1),
        scala.List(t"jdk-17", t"jdk-18", t"jdk-19"), true))

    test(m"an unsanctioned major refuses the sequence, L110"):
      val (v1, _) = compile(base, derived, api)

      val (v2, _) = compile(edit(base, t"protected int guarded() { return 2; }", t""),
          derived, api)

      import errorDiagnostics.stackTracesDiagnostics

      capture[LiraError]:
        HostContracts.assemble(t"fixture-host",
          List(HostRelease(t"a", v1), HostRelease(t"b", v2)),
          List(LiraManifest.Tool(t"jsig-harvest", t"0.1")))
      . reason
    . assert(_ == LiraError.Reason.UngradedSuccessor)

    test(m"ct.sym harvests a verifiable, tagged jdk contract"):
      CtSym.location().lay(true): path =>
        val releases = CtSym.releases(path)
        val earliest = releases.stdlib.head
        val surface = CtSym.surface(path, earliest, prefix = t"java.base/java/lang/")
        val tag = Text(s"jdk-$earliest")

        val liras = HostContracts.assemble(t"jdk", List(HostRelease(tag, surface)),
          List(LiraManifest.Tool(t"jsig-harvest", t"0.1")))

        val lira = Lira.read(liras.stdlib.head(1))
        Verification.install(lira)

        releases.stdlib.size >= 2
          && surface.stdlib.size > 20
          && lira.manifest.tag.stdlib == scala.List(tag)
          && lira.manifest.hostContract
    . assert(_ == true)

    // --- used-set extraction ------------------------------------------------------------------

    def encode(text: Text): Data = Array.unsafeFrozen(text.s.getBytes("UTF-8").nn)
    def blob(data: Data): Data = LiraHash(LiraHash.Domain.Blob, data)

    // Package-private: the source compiles in the `Holder.java` fixture slot, where a public
    // class of another name could not.
    val consumerOld: Text =
      t"""|package fixture;
          |class Consumer {
          |  public int use(Base b) { return b.inherited(); }
          |}
          |""".s.stripMargin.tt

    def consumerContent(baseSource: Text, consumer: Text): List[(TreePath, Data)] =
      val (content, _) = compile(baseSource, derived, api, consumer)
      List.from(content.filter { pair => pair(0).text.s.contains("Consumer") })

    test(m"references spell membership keys and exclude the content's own classes"):
      val refs = UsedSets.references(consumerContent(base, consumerOld))

      (refs.stdlib.contains(t"fixture/Base#inherited:()I"),
       refs.stdlib.contains(t"java/lang/Object"),
       refs.stdlib.exists(_.s.startsWith("fixture/Consumer")))
    . assert(_ == (true, true, false))

    test(m"resolution splits a contract's atoms from foreign references"):
      val (surface, _) = compile(base, derived, api)
      val listing = JsigDiscipline.atomize(surface, Discipline.Context(t"host"))
      val (matched, unmatched) = UsedSets.resolve(
          UsedSets.references(consumerContent(base, consumerOld)), listing)

      (matched.stdlib.nonEmpty,
       unmatched.stdlib.contains(t"java/lang/Object"),
       unmatched.stdlib.exists(_.s.startsWith("fixture/Base")))
    . assert(_ == (true, true, false))

    test(m"the uses blob round-trips its resolved hashes"):
      val (surface, _) = compile(base, derived, api)
      val listing = JsigDiscipline.atomize(surface, Discipline.Context(t"host"))
      val content = consumerContent(base, consumerOld)
      val (usesBlob, _) = UsedSets.uses(t"fixture-host", content, listing)
      val (matched, _) = UsedSets.resolve(UsedSets.references(content), listing)

      UsesBlob.decode(usesBlob)(1).stdlib.map { hash => LiraHash.text(hash) }.toSet
      == matched.stdlib.map { hash => LiraHash.text(hash) }.toSet
    . assert(identity)

    test(m"a computed used-set decides host satisfaction by spanning"):
      val added = edit(base, t"public int inherited",
          t"public int added() { return 9; }\n  public int inherited")

      val consumerNew = consumerOld.s.replace("b.inherited()", "b.added()").nn.tt

      val (v1, _) = compile(base, derived, api)
      val (v2, _) = compile(added, derived, api)

      val contracts = HostContracts.assemble(t"fixture-host",
        List(HostRelease(t"v1", v1), HostRelease(t"v2", v2)),
        List(LiraManifest.Tool(t"jsig-harvest", t"0.1")))

      val v1Manifest = Lira.read(contracts.stdlib.head(1)).manifest
      val v2Manifest = Lira.read(contracts.stdlib.last(1)).manifest

      val v1Listing = JsigDiscipline.atomize(v1, Discipline.Context(t"host"))
      val v2Listing = JsigDiscipline.atomize(v2, Discipline.Context(t"host"))

      // Both consumers compiled against, and resolved against, the v2 surface; the question is
      // whether the *older* contract release satisfies each, and only their used-sets differ.
      val snap2 = v2Manifest.lineage.stdlib.last
      val markerOld = blob(encode(t"uses-old"))
      val markerNew = blob(encode(t"uses-new"))

      def library(marker: Data): LiraManifest =
        LiraManifest(
          module  = t"consumer",
          lineage = List(LiraHash(LiraHash.Domain.Snapshot, encode(t"consumer"))),
          api     = List(),
          section = List(Section(t"jvm", tree = blob(encode(t"tree")),
              requires = List(LiraManifest.Requires(t"fixture-host", snap2, uses = marker)))),
          payload = LiraManifest.Payload(t"brotli", 0L, blob(encode(t"consumer"))))

      def usedSet(consumer: Text): scala.collection.immutable.Set[Text] =
        val (matched, _) = UsedSets.resolve(
            UsedSets.references(consumerContent(added, consumer)), v2Listing)

        matched.stdlib.map { hash => LiraHash.text(hash) }.toSet

      val oldUses = usedSet(consumerOld)
      val newUses = usedSet(consumerNew)

      val contractAtoms = { (module: Text) =>
        if module == t"fixture-host"
        then v1Listing.atoms.stdlib.map { atom => LiraHash.text(atom.valueHash) }.toSet
        else Unset
      }

      def lookup(marker: Data, set: scala.collection.immutable.Set[Text]) =
        { (data: Data) => if Blob.compare(data, marker) == 0 then set else Unset }

      // The old-surface consumer spans back to v1 by set inclusion; the one that calls the
      // v2-only method provably does not.
      val spans =
        Buildpath(List(library(markerOld)))
        . validate(t"jvm", contracts = List(v1Manifest), atoms = contractAtoms,
            used = lookup(markerOld, oldUses))
        . stdlib.isEmpty

      import errorDiagnostics.stackTracesDiagnostics

      val refused =
        capture[LiraError]:
          Buildpath(List(library(markerNew)))
          . validate(t"jvm", contracts = List(v1Manifest), atoms = contractAtoms,
              used = lookup(markerNew, newUses))
        . reason

      (spans, refused)
    . assert(_ == (true, LiraError.Reason.UnsatisfiedRequirement(t"fixture-host")))

    test(m"fixture references resolve against a harvested jdk surface"):
      CtSym.location().lay(true): path =>
        val release = CtSym.releases(path).stdlib.head
        val surface = CtSym.surface(path, release, prefix = t"java.base/java/lang/")
        val listing = JsigDiscipline.atomize(surface, Discipline.Context(t"host"))

        val (matched, unmatched) = UsedSets.resolve(
            UsedSets.references(consumerContent(base, consumerOld)), listing)

        matched.stdlib.nonEmpty && !unmatched.stdlib.contains(t"java/lang/Object")
    . assert(_ == true)
