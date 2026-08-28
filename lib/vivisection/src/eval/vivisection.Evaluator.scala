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

import java.util.concurrent.atomic as juca

import scala.caps
import scala.collection.immutable as sci

import anthology.*
import anticipation.*
import contingency.*
import gossamer.*
import hellenism.*
import parasite.*
import proscenium.*
import spectacular.*
import vacuous.*

object Evaluator:
  // Names each synthetic evaluation class distinctly, so a session never redefines a top-level
  // name (which `anthology`'s retained symbol table forbids).
  private val counter: juca.AtomicInteger = juca.AtomicInteger(0)

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

      val found = connection.methods(classLoaderClass).stdlib.find: method =>
        method.name == t"defineClass" && method.signature == signature

      found.map(_.method).getOrElse(Jdwp.Ref.empty)

    private def loaded(signature: Text): ReferenceTypeId =
      connection.allClasses().stdlib.find(_.signature == signature).map(_.cls)
        .getOrElse(Jdwp.Ref.empty)

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
  ( halt: Halt, session: Scalac.Session^, classpath: LocalClasspath )
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
    halt.topFrame.lay(Variable.Snapshot.Null): (frame, location) =>
      val table = connection.variableTable(location.cls, location.method)

      // The locals live at the stop, in declaration order: name, Scala type, and current value.
      val locals: sci.List[(Text, Text, Jdwp.Value)] =
        table.lay(sci.List[(Text, Text, Jdwp.Value)]()): table =>
          val live = table.slots.stdlib.filter: slot =>
            val index = location.index
            slot.name != t"this" && slot.index <= index && index < slot.index + slot.length

          val requests = live.map: slot => (slot.slot, Variable.tag(slot.signature))
          val values = connection.slotValues(thread, frame, List(requests*)).stdlib
          live.zip(values).map: (slot, value) => (slot.name, slot.signature, value)

      val className = t"vivisection$$eval$$${Evaluator.counter.getAndIncrement()}"
      val pkg = packageOf(connection.signature(location.cls))
      val source = render(className, pkg, locals.map { (name, sig, _) => (name, sig) }, expression)

      val process = session.compile(Map(t"$className.scala" -> source))
      process.complete()

      val loader = classLoaderFor(location.cls)
      val injector = Evaluator.Injector(connection, thread, loader)
      val qualified = if pkg == t"" then className else t"$pkg.$className"

      // Define every classfile. `defineClass` loads without linking, so the entry class is then
      // forced through `Class.forName(name, true, loader)` to prepare it, after which its methods
      // can be read.
      process.classfiles.stdlib.foreach: (path, bytecode) =>
        if path.encode.s.endsWith(".class") then injector.define(classNameOf(path.encode), bytecode)

      val cls = prepared(qualified, loader)
      val constructor = connection.methods(cls).stdlib.find(_.name == t"<init>").map(_.method)
      val run = connection.methods(cls).stdlib.find(_.name == t"run").map(_.method)
      val arguments = List(locals.map { (_, _, value) => value }*)

      val result: Jdwp.Value = constructor match
        case scala.Some(ctor) => run match
          case scala.Some(runMethod) =>
            connection.newInstance(cls, thread, ctor, arguments).result match
              case Jdwp.Value.Reference(_, id) =>
                connection.invokeMethod(id, thread, cls, runMethod, List()).result

              case _ =>
                Jdwp.Value.Void

          case _ =>
            Jdwp.Value.Void

        case _ =>
          Jdwp.Value.Void

      result match
        case Jdwp.Value.Reference(Jdwp.Tag.StringTag, id) if !id.empty =>
          Variable.Snapshot.Str(id, connection.stringValue(Jdwp.Ref(id.long)))

        case other =>
          Variable.Snapshot.Primitive(other)

  // Renders a visible binding through its `Inspectable` instance, resolved and invoked in the
  // debuggee. Because the synthetic class types the binding at its declared type, the summon is a
  // *static* one — the instance the programmer's own code would pick — so a type's own notation is
  // used rather than a generic `toString`. `Inspectable` is a `Typeclass.Pure`, so a resolved
  // instance is verified side-effect-free; only the derived `toString`/`Showable` fallbacks (which
  // mark their output) are unverified. The declared type is the erased runtime type until TASTy
  // static typing (a later phase) supplies the source type, so an opaque type still renders through
  // its underlying representation's instance for now.
  def inspect(binding: Text): Text =
    apply(t"($binding).inspect") match
      case Variable.Snapshot.Str(_, text) => text
      case other                          => other.inspect

  private def loadedType(signature: Text): ReferenceTypeId =
    connection.allClasses().stdlib.find(_.signature == signature).map(_.cls)
      .getOrElse(Jdwp.Ref.empty)

  private def methodId(cls: ReferenceTypeId, name: Text, signature: Text): MethodId =
    val found = connection.methods(cls).stdlib.find: info =>
      info.name == name && info.signature == signature

    found.map(_.method).getOrElse(Jdwp.Ref.empty)

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

  // The synthetic compilation unit: a class whose constructor parameters mirror the frame's
  // locals and whose `run` renders the expression as text through `String.valueOf`, so every
  // result — primitive, string or object — comes back over one decode path.
  private def render(name: Text, pkg: Text, params: sci.List[(Text, Text)], expression: Text)
  :   Text =

    val rendered = params.map: (field, signature) =>
      s"${field.s}: ${Variable.demangle(signature).s}"

    val parameters = rendered.mkString(", ")

    // `spectacular` is imported so `.inspect` resolves for the rendering path; an evaluation which
    // does not use it simply leaves the import unused.
    val imports = "import spectacular.*\n\n"
    val header = if pkg == t"" then imports else s"package ${pkg.s}\n\n$imports"
    val run = s"java.lang.String.valueOf((${expression.s}): scala.Any)"

    s"${header}class ${name.s}($parameters):\n  def run(): java.lang.String = $run\n".tt
