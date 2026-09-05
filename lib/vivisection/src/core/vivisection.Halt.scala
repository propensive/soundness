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

import anticipation.*
import contingency.*
import denominative.*

// A line table's last covering entry, and the depth of the `$outer` chain walked below, are
// genuinely counted; both lists are short, and the line table is already traversed once to filter
// it.
import denominative.dysasymptotics.linearSize
import gossamer.*
import proscenium.*
import rudiments.*
import symbolism.*
import vacuous.*

object Halt:
  // How many leading elements of an array are fetched eagerly for its snapshot, and how many are
  // materialised on expansion.
  private[vivisection] val prefixLength: Int = 10
  private[vivisection] val expansionLength: Int = 256

  // The per-composite record of whether a handler has asked the dispatcher to leave the VM
  // suspended. Shared by every handler run for one composite, because JDWP suspends once per
  // composite and the dispatcher settles that suspension exactly once.
  private[vivisection] class Retention():
    private val flag: Atomic[Boolean] = Atomic(false)

    private[vivisection] def set(): Unit = flag() = true
    private[vivisection] def retained: Boolean = flag()

  // Why the thread stopped: the event which minted this halt. `Stopped` covers breakpoints, steps
  // and method entry and exit, where the handler already knows which request it registered; the
  // other causes carry payloads the stop's location alone cannot — the exception in flight, or
  // the field being touched (and, on modification, the value about to be written). The `target`
  // is absent when a static field is touched.
  enum Cause:
    case Stopped

    case Thrown(exception: ObjectId, catchLocation: Optional[Jdwp.Location])

    case Access(cls: ReferenceTypeId, field: FieldId, target: Optional[ObjectId])

    case Modification(cls: ReferenceTypeId, field: FieldId, target: Optional[ObjectId],
        incoming: Jdwp.Value)

  // A summary of the exception in flight at an exception stop: its class, the detail message it
  // was constructed with (absent when null), and whether the throw will be caught.
  case class ExceptionInfo(className: Text, message: Optional[Text], caught: Boolean)

  // One logical position a physical frame stands for: for inlined code, the file and line where
  // the code was written; for the frame itself, its own source and its real line. `source` is
  // the file's recorded basename and `path` its fuller path, when known.
  case class Position
    ( name:    Text,
      source:  Optional[Text],
      path:    Optional[Text],
      line:    Int,
      inlined: Boolean,
      cls:     Optional[Text] = Unset )

// The capability lent to an event handler while its thread stands suspended: a view over the
// stopped thread's frames and their logical variables. Sealed (`ExclusiveCapability`), so it
// cannot outlive the stop it describes — once the dispatcher resumes the thread, every
// identifier this halt handed out may be stale.
class Halt private[vivisection]
  ( private[vivisection] val connection: Jdwp.Connection,
    val thread:   ThreadId,
    val location: Jdwp.Location,
    val cause:    Halt.Cause,
    retention:    Halt.Retention )
  // The tactic the dispatcher supplied when it minted this halt is a `using` parameter, so it is
  // already the given the connection commands below resolve: a handler runs on the dispatcher
  // task, so the recoverable-error channel it uses must be one which unwinds *that* stack, not the
  // registration site's.
  (using Tactic[Debugger.Error])
extends caps.ExclusiveCapability:

  // Asks the dispatcher not to auto-resume after the handlers for this stop have run. The caller
  // then owns the suspension, and resumes (or not) through the session.
  def remain(): Unit = retention.set()

  def frames(): List[(FrameId, Jdwp.Location)] =
    connection.frames(thread, 0, connection.frameCount(thread))

  // The logical positions a frame stands for, innermost first: one per inline origin recovered
  // from the class's SMAP, then the physical frame itself at its real (call-site) line. A frame
  // with no SMAP, one stopped at a real line, or a VM without the source-debug-extension
  // capability yields exactly the single raw position `describe` reports.
  def positions(location: Jdwp.Location): List[Halt.Position] =
    val (name, line) = describe(location)
    val smap = connection.smap(location.cls)
    val source = safely(connection.sourceFile(location.cls))
    val path = smap.let(_.path)

    smap.let(_.expand(line)).lay(List(Halt.Position(name, source, path, line, false))):
      expansion =>
        val inlined = expansion.inlined.map: origin =>
          // The `ScalaClass` stratum records binary-ish names with `$u002E` escapes; failing
          // that, the origin's file names the frame. The raw name rides along, for a caller
          // holding the classpath to resolve the definition through TASTy.
          val cls = origin.cls.lay(origin.file): cls => cls.s.replace("$u002E", ".").nn.tt
          Halt.Position(cls, origin.file, origin.path, origin.line, true, origin.cls)

        val real = Halt.Position(name, source, path, expansion.line.or(line), false)
        inlined + List(real)

  // A human-readable position for a frame — `pkg.Cls.method` and its source line, resolved from
  // the class's method and line tables; line 0 where no line information exists.
  def describe(location: Jdwp.Location): (Text, Int) =
    val cls = Variable.demangle(connection.signature(location.cls))
    val method = connection.methods(location.cls).seek(_.method == location.method)
    val name = method.let { info => t"$cls.${info.name}" }.or(cls)

    val line = safely(connection.lineTable(location.cls, location.method)) match
      case table: Jdwp.LineTable =>
        table.lines.filter(_.index <= location.index).last.let(_.line).or(0)

      case _ =>
        0

    (name, line)

  // Describes the exception in flight, when this stop reports one. The message is read directly
  // from `Throwable`'s `detailMessage` field — walking up the type hierarchy to the class which
  // declares it — rather than by invoking `getMessage`, so no debuggee code runs and none of the
  // invoke hazards apply.
  def exceptionInfo(): Optional[Halt.ExceptionInfo] = cause match
    case Halt.Cause.Thrown(exception, catchLocation) =>
      val (_, cls) = connection.referenceType(exception)
      val className = Variable.demangle(connection.signature(cls))

      def declaring(cls0: ReferenceTypeId): Optional[FieldId] =
        if cls0.empty then Unset else
          connection.fields(cls0).seek(_.name == t"detailMessage").let(_.field).or:
            declaring(connection.superclass(cls0))

      val message: Optional[Text] = declaring(cls).lay(Unset):
        fieldId =>
          connection.fieldValues(exception, List(fieldId)).prim match
            case Jdwp.Value.Reference(Jdwp.Tag.StringTag, id) if !id.empty =>
              connection.stringValue(Jdwp.Ref(id.long))

            case _ =>
              Unset

      Halt.ExceptionInfo(className, message, catchLocation != Unset)

    case _ =>
      Unset

  // The stopped frame's `this`, absent in a static or native frame.
  def thisObject(frame: FrameId): Optional[ObjectId] =
    connection.thisObject(thread, frame) match
      case Jdwp.Value.Reference(_, id) => if id.empty then Unset else id
      case _                           => Unset

  // The frame where the thread stopped — the innermost, and the default subject of variable
  // recovery and evaluation.
  private[vivisection] def topFrame: Optional[(FrameId, Jdwp.Location)] =
    connection.frames(thread, 0, 1).prim

  // The logical variables visible at the top frame — where the thread stopped.
  def variables(): List[Variable] =
    topFrame.lay(List[Variable]()): (frame, location0) => variables(frame, location0)

  // The logical variables visible at a frame: the live local slots, then the state captured on
  // `this` and its `$outer` chain — un-flattened, unboxed and lazy-read before anything
  // downstream sees a value. A frame compiled without a variable table degrades to the captured
  // and field state alone.
  def variables(frame: FrameId, location0: Jdwp.Location): List[Variable] =
    val table = connection.variableTable(location0.cls, location0.method)

    val locals = table.lay(List[Variable]()): table =>
      val live = table.slots.filter: slot =>
        val index = location0.index
        slot.name != t"this" && slot.index <= index && index < slot.index + slot.length

      val requests: List[(Int, Jdwp.Tag)] =
        live.map: slot => (slot.slot, Variable.tag(slot.signature))

      val values = connection.slotValues(thread, frame, requests)

      live.zip(values).map: (slot, value) =>
        variable(slot.name, slot.signature, value, Variable.Provenance.Local(slot.slot))

    val captures = thisObject(frame).lay(List[Variable]())(capturesOf(_, List()))

    locals + captures

  // Expands a snapshot into its next level: an object's instance fields, or an array's elements.
  def children(snapshot0: Variable.Snapshot): List[Variable] =
    snapshot0 match
      case Variable.Snapshot.Obj(id, cls) =>
        val (_, cls0) = connection.referenceType(id)
        val fields = instanceFields(cls0)
        val values = connection.fieldValues(id, fields.map(_.field))

        fields.zip(values).map: (field, value) =>
          variable(field.name, field.signature, value,
              Variable.Provenance.Field(cls, id, field.field))

      case Variable.Snapshot.Arr(id, component, length, _) =>
        val limit = if length < Halt.expansionLength then length else Halt.expansionLength

        val values =
          if limit == 0 then List[Jdwp.Value]() else connection.arrayValues(id, 0, limit)

        // `indexed` numbers from `Prim`, so the element's own index is the zero-based `n0`.
        values.indexed.map: (value, index) =>
          Variable(index.n0.toString.tt, Unset, Variable.tagName(component), snapshot(value),
              Variable.Provenance.Element(id, index.n0), true, Variable.State.Forced)

      case _ =>
        List()

  // Discards the given frame and every frame above it, leaving the thread suspended at the call
  // which created it, so resuming re-executes the call — a frontend's frame restart. Requires
  // the VM's `canPopFrames` capability and no native frame among those popped.
  def pop(frame: FrameId): Unit = connection.popFrames(thread, frame)

  // Writes a new value into a variable at the stopped frame.
  def assign(variable: Variable, value: Jdwp.Value): Unit =
    topFrame.let: (frame, _) => assign(frame, variable, value)

  // Writes a new value into a variable, routed by its provenance: a local slot in the given
  // frame, a captured or member field on its holding object, an array element, or — through a
  // ref cell — the cell's `elem`, so assigning a captured `var` behaves as the source suggests.
  // The value must be of the variable's erased type; the identifiers a provenance carries are
  // only valid while this halt's thread stands suspended, which is exactly when assignment is
  // meaningful.
  def assign(frame: FrameId, variable: Variable, value: Jdwp.Value): Unit =
    variable.provenance match
      case Variable.Provenance.Local(slot) =>
        connection.setSlotValues(thread, frame, List((slot, value)))

      case Variable.Provenance.Captured(_, holder, field) =>
        connection.setFieldValues(holder, List((field, value)))

      case Variable.Provenance.Field(_, target, field) =>
        connection.setFieldValues(target, List((field, value)))

      case Variable.Provenance.Element(array, index) =>
        connection.setArrayValues(array, index, List(value))

      case Variable.Provenance.Cell(cell, elem, _) =>
        connection.setFieldValues(cell, List((elem, value)))

  private def instanceFields(cls: ReferenceTypeId): List[Jdwp.FieldInfo] =
    connection.fields(cls).filter: field => (field.modifiers & 0x8) == 0

  // Builds one logical variable from its storage form: unboxes a ref cell, then snapshots.
  private def variable
    ( name: Text, signature: Text, value: Jdwp.Value, provenance: Variable.Provenance )
  :   Variable =

    val (value0, provenance0, mutable) = unboxed(value, provenance)

    Variable(name, Unset, Variable.demangle(signature), snapshot(value0), provenance0, mutable,
        Variable.State.Forced)

  // A captured `var` is boxed in a `scala.runtime.*Ref` cell: the logical value is the cell's
  // `elem` field, and the cell's presence is exactly what marks the variable mutable.
  private def unboxed(value: Jdwp.Value, provenance: Variable.Provenance)
  :   (Jdwp.Value, Variable.Provenance, Boolean) =

    value match
      case Jdwp.Value.Reference(_, id) if !id.empty =>
        val (_, cls) = connection.referenceType(id)
        val signature = connection.signature(cls)

        if signature.s.startsWith("Lscala/runtime/") && signature.s.endsWith("Ref;") then
          val elem = connection.fields(cls).seek(_.name == t"elem")

          elem.let: field =>
            connection.fieldValues(id, List(field.field)).prim.let: value0 =>
              (value0, Variable.Provenance.Cell(id, field.field, provenance), true)
          . or((value, provenance, false))

        else
          (value, provenance, false)

      case _ =>
        (value, provenance, false)

  // Decodes a wire value into the debugger-side view a renderer can show. Strings and a bounded
  // prefix of each array are fetched eagerly; any other object stays a summary for `children`.
  private def snapshot(value: Jdwp.Value): Variable.Snapshot =
    value match
      case Jdwp.Value.Reference(tag, id) =>
        if id.empty then Variable.Snapshot.Null else tag match
          case Jdwp.Tag.StringTag =>
            Variable.Snapshot.Str(id, connection.stringValue(Jdwp.Ref(id.long)))

          case Jdwp.Tag.ArrayTag =>
            val (_, cls) = connection.referenceType(id)
            val signature = connection.signature(cls)
            val component = Variable.tag(signature.s.substring(1).nn.tt)
            val length = connection.arrayLength(id)
            val count = if length < Halt.prefixLength then length else Halt.prefixLength

            val prefix =
              if count == 0 then List[Variable.Snapshot]()
              else connection.arrayValues(id, 0, count).map(snapshot(_))

            Variable.Snapshot.Arr(id, component, length, prefix)

          case _ =>
            val (_, cls) = connection.referenceType(id)
            Variable.Snapshot.Obj(id, Variable.demangle(connection.signature(cls)))

      case other =>
        Variable.Snapshot.Primitive(other)

  // Un-flattens the state captured on `this`: each synthetic `name$N` field recovers `name`, the
  // `$outer` chain is walked so an enclosing scope's captures surface too, and ordinary member
  // fields appear against their owner. A lazy val's backing field is read but *never* forced: a
  // null cell means it is unevaluated, and stays that way.
  private def capturesOf(obj: ObjectId, path: List[Text]): List[Variable] =
    if path.size > 8 then List() else
      val (_, cls) = connection.referenceType(obj)
      val owner = Variable.demangle(connection.signature(cls))
      val fields = instanceFields(cls)
      val values = connection.fieldValues(obj, fields.map(_.field))

      fields.zip(values).flatMap: (field, value) =>
        val name = Variable.fieldName(field.name)

        // Each recovered name is bound to a typed local before it is read: an `Optional` read
        // directly inside a collection lambda trips the compiler's `wildApprox` assertion.
        val lazily: Optional[Text] = Variable.lazyField(name)
        val capture: Optional[Text] = Variable.captured(name)

        if name == t"$$outer" then value match
          case Jdwp.Value.Reference(_, outer) if !outer.empty =>
            capturesOf(outer, path + List(name))

          case _ =>
            List()

        else if lazily.present then
          val base = lazily.or(name)

          val unforced = value match
            case Jdwp.Value.Reference(_, id) => id.empty
            case _                           => false

          val state = if unforced then Variable.State.Unforced else Variable.State.Forced
          val snap = if unforced then Unset else snapshot(value)

          List(Variable(base, Unset, Variable.demangle(field.signature), snap,
              Variable.Provenance.Field(owner, obj, field.field), false, state))

        else if capture.present then
          val base = capture.or(name)

          List(variable(base, field.signature, value,
              Variable.Provenance.Captured(path + List(name), obj, field.field)))

        else
          List(variable(name, field.signature, value,
              Variable.Provenance.Field(owner, obj, field.field)))
