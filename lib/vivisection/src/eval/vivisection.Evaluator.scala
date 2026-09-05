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
package vivisection

import scala.caps

import anthology.*
import anticipation.*
import contingency.*
import gossamer.*
import hellenism.*
import parasite.*
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*

object Evaluator:
  // Names each synthetic evaluation class distinctly, so a session never redefines a top-level
  // name (which `anthology`'s retained symbol table forbids).
  private val counter: Atomic[Int] = Atomic(0)

  // Injects compiled classfiles into a running debuggee. JDWP has no define-class command, so the
  // bytes are shipped as a `byte[]` allocated and filled over the wire, then handed to the target
  // class loader's protected `defineClass` — JDWP invocation ignores access control. Every
  // classfile of a compile is defined (a Scala source emits helper and lambda classes alongside
  // the one named here); linking is lazy, so order does not matter.
  class Injector private[vivisection]
    ( connection: Jdwp.Connection, thread: ThreadId, loader: ClassLoaderId )
    ( using Tactic[Debugger.Error] ):

    // Discovered once and reused for every classfile of an evaluation.
    private lazy val byteArrayType: ReferenceTypeId =
      loaded(t"[B")

    private lazy val classLoaderClass: ReferenceTypeId =
      loaded(t"Ljava/lang/ClassLoader;")

    private lazy val defineClass: MethodId =
      val signature = t"(Ljava/lang/String;[BII)Ljava/lang/Class;"

      val found = connection.methods(classLoaderClass).seek: method =>
        method.name == t"defineClass" && method.signature == signature

      found.let(_.method).or(Jdwp.Ref.empty)

    private def loaded(signature: Text): ReferenceTypeId =
      connection.allClasses().seek(_.signature == signature).let(_.cls)
        .or(Jdwp.Ref.empty)

    // Defines one classfile in the debuggee and returns the reference type of the loaded class,
    // recovered from the `java.lang.Class` object `defineClass` hands back (which is why no lookup
    // by name in `allClasses` is needed — the freshly-defined class may not appear there yet).
    def define(name: Text, bytecode: Data): Optional[ReferenceTypeId] =
      val length = bytecode.length
      val array = connection.newArray(byteArrayType, length)

      val values =
        List((0 until length).map { index => Jdwp.Value.OfByte(bytecode.readUnchecked(index)) }*)

      connection.setArrayValues(array, 0, values)
      val nameString = connection.createString(name)
      val nameArg = Jdwp.Value.Reference(Jdwp.Tag.StringTag, Jdwp.Ref(nameString.long))
      val arrayArg = Jdwp.Value.Reference(Jdwp.Tag.ArrayTag, array)
      val arguments = List(nameArg, arrayArg, Jdwp.Value.OfInt(0), Jdwp.Value.OfInt(length))
      val loaderId = Jdwp.Ref(loader.long)

      val defined =
        connection.invokeMethod(loaderId, thread, classLoaderClass, defineClass, arguments)

      defined.result match
        case Jdwp.Value.Reference(_, id) if !id.empty => connection.reflectedType(id)
        case _                                        => Unset

// Evaluates Scala expressions at a suspended frame: it mirrors the frame's visible locals as the
// parameters of a freshly-named synthetic class, compiles it against the debuggee's classpath in
// the warm session lent to it, injects the classfiles, instantiates the class with the locals'
// live values, and invokes its `run` method in the debuggee. The result is returned as text (its
// `String.valueOf` form), which keeps a single decode path across every result type; the typed,
// structured rendering arrives with the rendering phase.
class Evaluator private[vivisection]
  ( halt: Halt, session: Scalac.Session^, classpath: LocalClasspath, purview: Purview )
  ( using Monitor, Tactic[Async.Error], Tactic[Debugger.Error], Tactic[Compiler.Error] )
extends caps.ExclusiveCapability:

  private val connection = halt.connection
  private val thread = halt.thread

  // The frame's declared package, recovered from its class signature (`Lpkg/Name;` → `pkg`). The
  // synthetic class is generated there so the expression sees the same imports and visibility the
  // programmer had; an empty package for a top-level class.
  private def packageOf(signature: Text): Text =
    val body = signature.s.substring(1, signature.s.length - 1).nn
    val slash = body.lastIndexOf('/')
    if slash < 0 then t"" else body.substring(0, slash).nn.replace('/', '.').nn.tt

  def apply(expression: Text): Variable.Snapshot =
    execute(t"java.lang.String.valueOf(($expression): scala.Any)", t"java.lang.String") match
      case Jdwp.Value.Reference(Jdwp.Tag.StringTag, id) if !id.empty =>
        Variable.Snapshot.Str(id, connection.stringValue(Jdwp.Ref(id.long)))

      case other =>
        Variable.Snapshot.Primitive(other)

  // Evaluates `expression` at the variable's declared (or, failing that, erased) type and writes
  // the result back into the named variable, routed by its provenance — a frontend's variable
  // edit. Typing the synthetic `run` at the variable's own type makes the compiler enforce
  // assignability, and returns a primitive as a primitive, which the slot or field write
  // requires.
  def assign(binding: Text, expression: Text): Unit =
    variables().seek(_.name == binding).let: variable =>
      val kind: Text = variable.static.or(variable.erased)
      halt.assign(variable, execute(t"($expression)", kind))

  // The synthetic source `execute` compiles, staged but not compiled: the frame's visible locals
  // at their recovered static types, wrapped around `body` as the `run()` body at `resultType`,
  // plus the pieces the compile-inject-invoke tail needs — the class's simple name, its package,
  // and the locals with their live values (the constructor arguments).
  private def stage(frame: FrameId, location: Jdwp.Location, body: Text, resultType: Text)
  :   (Text, Text, Text, List[(Text, Text, Jdwp.Value)]) =

    val table = connection.variableTable(location.cls, location.method)

    // The locals live at the stop, in declaration order: name, Scala type, and current value.
    val locals: List[(Text, Text, Jdwp.Value)] =
      table.lay(List[(Text, Text, Jdwp.Value)]()): table =>
        val live = table.slots.filter: slot =>
          val index = location.index
          slot.name != t"this" && slot.index <= index && index < slot.index + slot.length

        val requests: List[(Int, Jdwp.Tag)] =
          live.map: slot => (slot.slot, Variable.tag(slot.signature))

        val values = connection.slotValues(thread, frame, requests)
        live.zip(values).map: (slot, value) => (slot.name, slot.signature, value)

    val className = t"vivisection$$eval$$${Evaluator.counter.ere(_ + 1)}"
    val ownerSignature = connection.signature(location.cls)
    val pkg = packageOf(ownerSignature)

    // Prefer each binding's declared static type (opaque and generic types recovered from TASTy)
    // over its erased runtime type, so `.inspect` selects the instance the programmer's own code
    // would. Only a method's value parameters are resolved this way for now; other locals keep
    // the erased type.
    val owner = Variable.demangle(ownerSignature)
    val method = methodName(location.cls, location.method)
    val statics = purview.parameters(owner, method)

    val typed: List[(Text, Text)] = locals.map: (name, signature, _) =>
      // The recovered type is bound to a typed local before it is read: an `Optional` read
      // directly inside a collection lambda trips the compiler's `wildApprox` assertion.
      val static: Optional[Text] = statics.at(name)
      (name, static.or(Variable.demangle(signature)))

    (render(className, pkg, typed, resultType, body), className, pkg, locals)

  // The synthetic source a completion host should typecheck, and the offset at which the console
  // fragment begins within it: the same class `execute` compiles, with the fragment as the `run`
  // body at `scala.Any`. No compiler runs here — the host highlights the source at a caret past
  // the returned offset with a typechecking `Highlight` over the debuggee's classpath, then
  // rebases the resulting replacement span by the same offset. Completion on a local therefore
  // resolves against its declared type, extension methods and givens included.
  def completion(fragment: Text): Optional[(Text, Int)] =
    halt.topFrame.let: (frame, location) =>
      val (source, _, _, _) = stage(frame, location, fragment, t"scala.Any")
      (source, source.length - 1 - fragment.length)

  // Compiles a synthetic class whose `run(): resultType = body` closes over the frame's locals,
  // injects it, and invokes it, returning the invoke's raw result.
  private def execute(body: Text, resultType: Text): Jdwp.Value =
    halt.topFrame.lay(Jdwp.Value.Void): (frame, location) =>
      val (source, className, pkg, locals) = stage(frame, location, body, resultType)

      val process = session.compile(Map(t"$className.scala" -> source))
      process.complete()

      val loader = classLoaderFor(location.cls)
      val injector = Evaluator.Injector(connection, thread, loader)
      val qualified = if pkg == t"" then className else t"$pkg.$className"

      // Define every classfile. `defineClass` loads without linking, so the entry class is then
      // forced through `Class.forName(name, true, loader)` to prepare it, after which its methods
      // can be read.
      //
      // The stdlib view hands the values over unwidened: traversing the native `Map` widens
      // each `Data` value to a read-only `Array[Byte]^{value.rd}`, which the injector's `Data`
      // parameter (a pure `Array[Byte]^{}`) will not accept.
      process.classfiles.stdlib.foreach: (path, bytecode) =>
        if path.encode.s.endsWith(".class") then injector.define(classNameOf(path.encode), bytecode)

      val cls = prepared(qualified, loader)
      val constructor = connection.methods(cls).seek(_.name == t"<init>").let(_.method)
      val run = connection.methods(cls).seek(_.name == t"run").let(_.method)
      val arguments: List[Jdwp.Value] = locals.map: (_, _, value) => value

      constructor.let: ctor =>
        run.let: runMethod =>
          connection.newInstance(cls, thread, ctor, arguments).result match
            case Jdwp.Value.Reference(_, id) =>
              connection.invokeMethod(id, thread, cls, runMethod, List()).result

            case _ =>
              Jdwp.Value.Void

      . or(Jdwp.Value.Void)

  // The elaborated call sites of the frame's method — the pieces the compiler inserted at each
  // call, inferred type arguments and synthesized `using` arguments — read from the debuggee's
  // own TASTy, so they reflect what actually runs. A hover surface's raw material.
  def elaborations(): List[prophesy.Elaboration] =
    halt.topFrame.lay(List()): (_, location) =>
      val owner = Variable.demangle(connection.signature(location.cls))
      purview.elaborations(owner, methodName(location.cls, location.method))

  // The frame's visible variables, each enriched with its declared static type where one can be
  // recovered from TASTy — the `stenography` rendering the user sees as `Variable.static`. A
  // binding whose static type could not be recovered (a non-parameter local, for now) keeps
  // `Unset` there and its erased runtime type in `Variable.erased`.
  def variables(): List[Variable] =
    val bindings = halt.variables()

    halt.topFrame.lay(bindings): (_, location) =>
      val owner = Variable.demangle(connection.signature(location.cls))
      val statics = purview.rendered(owner, methodName(location.cls, location.method))

      bindings.map: variable =>
        // The recovered type is bound to a typed local before it is read: an `Optional` read
        // directly inside a collection lambda trips the compiler's `wildApprox` assertion.
        val static: Optional[Text] = statics.at(variable.name)
        static.lay(variable): static => variable.copy(static = static)

  // Renders a visible binding through its `Inspectable` instance, resolved and invoked in the
  // debuggee. Because the synthetic class types the binding at its declared type, the summon is a
  // *static* one — the instance the programmer's own code would pick — so a type's own notation is
  // used rather than a generic `toString`. `Inspectable` is a `Typeclass.Pure`, so a resolved
  // instance is verified side-effect-free; only the derived `toString`/`Showable` fallbacks (which
  // mark their output) are unverified.
  def inspect(binding: Text): Text =
    apply(t"($binding).inspect") match
      case Variable.Snapshot.Str(_, text) => text
      case other                          => other.inspect

  private def loadedType(signature: Text): ReferenceTypeId =
    connection.allClasses().seek(_.signature == signature).let(_.cls)
      .or(Jdwp.Ref.empty)

  private def methodId(cls: ReferenceTypeId, name: Text, signature: Text): MethodId =
    val found = connection.methods(cls).seek: info =>
      info.name == name && info.signature == signature

    found.let(_.method).or(Jdwp.Ref.empty)

  // Prepares the freshly-defined entry class and returns its reference type, by forcing linkage
  // through `Class.forName(name, initialize = true, loader)` and reflecting the resulting `Class`
  // object back to its reference type. `defineClass` alone leaves the class loaded but unprepared,
  // so its method table cannot yet be read.
  private def prepared(qualified: Text, loader: ClassLoaderId): ReferenceTypeId =
    val classClass = loadedType(t"Ljava/lang/Class;")
    val descriptor = t"(Ljava/lang/String;ZLjava/lang/ClassLoader;)Ljava/lang/Class;"
    val forName = methodId(classClass, t"forName", descriptor)
    val nameString = connection.createString(qualified)
    val nameArg = Jdwp.Value.Reference(Jdwp.Tag.StringTag, Jdwp.Ref(nameString.long))
    val loaderArg = Jdwp.Value.Reference(Jdwp.Tag.ObjectTag, Jdwp.Ref(loader.long))
    val arguments = List(nameArg, Jdwp.Value.OfBoolean(true), loaderArg)

    connection.invokeStatic(classClass, thread, forName, arguments).result match
      case Jdwp.Value.Reference(_, id) if !id.empty => connection.reflectedType(id)
      case _                                        => Jdwp.Ref.empty

  // The class loader that defined the frame's class, falling back to the system loader for a
  // bootstrap-loaded frame (whose loader is the null reference).
  private def classLoaderFor(cls: ReferenceTypeId): ClassLoaderId =
    val loader = connection.classLoader(cls)

    if !loader.empty then loader else
      val classLoaderClass = loadedType(t"Ljava/lang/ClassLoader;")
      val method = methodId(classLoaderClass, t"getSystemClassLoader", t"()Ljava/lang/ClassLoader;")

      connection.invokeStatic(classLoaderClass, thread, method, List()).result match
        case Jdwp.Value.Reference(_, id) => Jdwp.Ref(id.long)
        case _                           => Jdwp.Ref.empty

  // `pkg/Name.class` (or `Name.class`) → the binary class name `pkg.Name`.
  private def classNameOf(path: Text): Text =
    val raw = path.s
    val base = if raw.endsWith(".class") then raw.substring(0, raw.length - 6).nn else raw
    val trimmed = if base.startsWith("/") then base.substring(1).nn else base
    trimmed.replace('/', '.').nn.tt

  // The name of the method at a location, read back from its class's method table.
  private def methodName(cls: ReferenceTypeId, method: MethodId): Text =
    connection.methods(cls).seek(_.method == method).let(_.name).or(t"")

  // The synthetic compilation unit: a class whose constructor parameters mirror the frame's
  // locals (each already typed at its declared or erased type) and whose `run` evaluates the
  // given body at the given result type — `String.valueOf` text for a plain evaluation, or a
  // variable's own type for an assignment.
  private def render
    ( name: Text, pkg: Text, params: List[(Text, Text)], resultType: Text, body: Text )
  :   Text =

    val parameters = params.map { (field, kind) => t"$field: $kind" }.join(t", ").s

    // `spectacular` is imported so `.inspect` resolves for the rendering path; an evaluation which
    // does not use it simply leaves the import unused.
    val imports = "import spectacular.*\n\n"
    val header = if pkg == t"" then imports else s"package ${pkg.s}\n\n$imports"

    s"${header}class ${name.s}($parameters):\n  def run(): ${resultType.s} = ${body.s}\n".tt
