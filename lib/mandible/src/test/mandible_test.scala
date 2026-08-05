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

  // Fixture variants are made by editing the source text, so each test states exactly the one
  // change whose grade it is asserting.
  def edit(source: Text, from: Text, to: Text): Text = source.s.replace(from.s, to.s).nn.tt

  def sources(base: Text, derived: Text, api: Text): Map[Text, Text] =
    Map(t"fixture/Base.java" -> base, t"fixture/Derived.java" -> derived, t"fixture/Api.java" -> api)

  def run(): Unit =
    classfileDisciplineTests()
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
    def compile(base: Text, derived: Text, api: Text) =
      // The compilation runs inside `supervise`, but the classfile bytes are read outside it:
      // `Data` is a frozen-array capability, and returning one out of the supervisor's scope
      // freshens its capture set past what the enclosing method can admit.
      val out: Text = supervise:
        val directory: soundness.Path on Linux = unsafely(temporaryDirectory / Uuid())
        Files.createDirectories(Paths.get(directory.encode.s))
        Javac(Nil)(LocalClasspath())(sources(base, derived, api), directory).complete()

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

    def atomize(base: Text, derived: Text, api: Text): Atomization =
      val (content, out) = compile(base, derived, api)
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
