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

import ambience.{System, WorkingDirectory}
import anthology.*
import anticipation.*
import rudiments.each
import coaxial.*
import contingency.*
import fulminate.*
import gigantism.*
import gossamer.*
import guillotine.*
import hieroglyph.*
import jacinta.*
import parasite.*
import proscenium.*
import turbulence.*
import urticose.*
import vacuous.*

// The Debug Adapter Protocol's message model — the subset this adapter speaks. Requests arrive
// as `{seq, type: "request", command, arguments}`; the adapter answers with
// `{seq, type: "response", request_seq, success, command, body}` and raises
// `{seq, type: "event", event, body}` on its own initiative. Argument types are decodable
// (they arrive), body types encodable (they leave); an `Unset` member is simply absent on the
// wire. Field names mirror the specification's exactly, including its `camelCase` and the
// reserved-word `type`.
object Dap:
  import dynamicJsonAccess.enabled

  // Serves the protocol over the ambient standard streams until the input is exhausted — the
  // canonical stdio transport a frontend launches. All outgoing traffic flows through a single
  // writer task, so responses and events never interleave, and each request is handled in
  // arrival order on this thread. The observer sees every message's raw text, both directions.
  def listen(observer: Text => Unit = { _ => () })
    ( using online:     Online,
            monitor:    Monitor,
            probate:    Probate,
            backend:    Socket.Backend,
            options:    Every[Socket.Option.Tcp],
            system:     System,
            asyncError: Tactic[Async.Error],
            working:    WorkingDirectory,
            stdio:      Stdio^,
            loggable:   (Socket.Event is Loggable)^,
            exec:       (Exec.Event is Loggable)^,
            compile:    (CompileEvent is Loggable)^,
            note:       Diagnostics )
  :   Unit =

    import strategies.throwUnsafely
    import charEncoders.utf8Encoder

    val outgoing: Relay[Json] = Relay()

    val writer: Task[Unit] = async:
      outgoing.lazyList.each: json =>
        val body: Text = json.encode
        observer(body)
        stdio.write(DapTransport.frame(body))
        stdio.out.flush()

    val session = DapSession(outgoing.put(_))

    try
      DapTransport.pump(stdio.in.source[Data], observer): message =>
        safely(message.read[Json]).let(session.handle(_))
    finally
      session.close()
      outgoing.stop()
      writer.cancel()

  object Envelope:
    // Pure and throwing: each internal summon mints its own throwing tactic; a decode failure
    // surfaces as `Json.Error` handled at the transport. Threading a caller's tactic through
    // the capture-polymorphic derivation is rejected by separation checking. The other codec
    // anchors below follow the same shape.
    given decodable: Envelope is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  // Any DAP message, read only for the members that decide how it is handled. All optional, so
  // every message decodes.
  case class Envelope
    ( seq:     Optional[Int]  = Unset,
      `type`:  Optional[Text] = Unset,
      command: Optional[Text] = Unset,
      event:   Optional[Text] = Unset )

  def envelope(json: Json): Envelope =
    import strategies.throwUnsafely
    try json.as[Envelope] catch case _: Exception => Envelope()

  // Message constructors. The `seq` is supplied by the session, which owns the outgoing
  // counter. Throwing tactics: attaching a field to a freshly-built object cannot fail.
  def response(seq: Int, request: Envelope, body: Optional[Json]): Json =
    import strategies.throwUnsafely

    val base =
      Json.make
        ( seq         = seq.in[Json],
          request_seq = request.seq.or(0).in[Json],
          success     = true.in[Json],
          command     = request.command.or(t"").in[Json] )

    val typed = base.updateDynamic("type")(t"response".in[Json])

    body.lay(typed): body => typed.updateDynamic("body")(body)

  def failure(seq: Int, request: Envelope, message: Text): Json =
    import strategies.throwUnsafely

    val base =
      Json.make
        ( seq         = seq.in[Json],
          request_seq = request.seq.or(0).in[Json],
          success     = false.in[Json],
          command     = request.command.or(t"").in[Json],
          message     = message.in[Json] )

    base.updateDynamic("type")(t"response".in[Json])

  def event(seq: Int, name: Text, body: Optional[Json]): Json =
    import strategies.throwUnsafely
    val base = Json.make(seq = seq.in[Json], event = name.in[Json])
    val typed = base.updateDynamic("type")(t"event".in[Json])

    body.lay(typed): body => typed.updateDynamic("body")(body)

  object ExceptionFilter:
    given encodable: ExceptionFilter is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class ExceptionFilter(filter: Text, label: Text, default: Boolean = false)

  val exceptionFilters: List[ExceptionFilter] =
    List
      ( ExceptionFilter(t"uncaught", t"Uncaught exceptions", true),
        ExceptionFilter(t"caught", t"Caught exceptions") )

  object Capabilities:
    given encodable: Capabilities is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  // What this adapter advertises in its `initialize` response; absent flags read as false.
  case class Capabilities
    ( supportsConfigurationDoneRequest: Boolean = true,
      supportsFunctionBreakpoints:      Boolean = true,
      supportsDataBreakpoints:          Boolean = true,
      supportsSetVariable:              Boolean = true,
      supportsSetExpression:            Boolean = true,
      supportsExceptionInfoRequest:     Boolean = true,
      supportsRestartFrame:             Boolean = true,
      supportsCompletionsRequest:       Boolean = true,
      completionTriggerCharacters:      List[Text] = List(t"."),
      exceptionBreakpointFilters:       List[ExceptionFilter] = Dap.exceptionFilters )

  object LaunchArguments:
    given decodable: LaunchArguments is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  // `launch` arguments are adapter-defined by the specification; this adapter asks for a main
  // class and a classpath, mirroring `Debuggee`.
  case class LaunchArguments(mainClass: Text, classpath: Text)

  object AttachArguments:
    given decodable: AttachArguments is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class AttachArguments(port: Int, hostName: Optional[Text] = Unset)

  object Source:
    given decodable: Source is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

    given encodable: Source is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class Source(name: Optional[Text] = Unset, path: Optional[Text] = Unset)

  object SourceBreakpointSpec:
    given decodable: SourceBreakpointSpec is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class SourceBreakpointSpec(line: Int)

  object SetBreakpointsArguments:
    given decodable: SetBreakpointsArguments is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class SetBreakpointsArguments(source: Source, breakpoints: List[SourceBreakpointSpec] = Nil)

  object SetExceptionBreakpointsArguments:
    given decodable: SetExceptionBreakpointsArguments is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class SetExceptionBreakpointsArguments(filters: List[Text] = Nil)

  object FunctionBreakpointSpec:
    given decodable: FunctionBreakpointSpec is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class FunctionBreakpointSpec(name: Text)

  object SetFunctionBreakpointsArguments:
    given decodable: SetFunctionBreakpointsArguments is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class SetFunctionBreakpointsArguments(breakpoints: List[FunctionBreakpointSpec] = Nil)

  object DataBreakpointInfoArguments:
    given decodable: DataBreakpointInfoArguments is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class DataBreakpointInfoArguments(name: Text, variablesReference: Optional[Int] = Unset)

  object DataBreakpointSpec:
    given decodable: DataBreakpointSpec is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class DataBreakpointSpec(dataId: Text)

  object SetDataBreakpointsArguments:
    given decodable: SetDataBreakpointsArguments is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class SetDataBreakpointsArguments(breakpoints: List[DataBreakpointSpec] = Nil)

  object ThreadArguments:
    given decodable: ThreadArguments is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class ThreadArguments(threadId: Int)

  object StackTraceArguments:
    given decodable: StackTraceArguments is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class StackTraceArguments
    ( threadId: Int, startFrame: Optional[Int] = Unset, levels: Optional[Int] = Unset )

  object FrameArguments:
    given decodable: FrameArguments is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class FrameArguments(frameId: Int)

  object VariablesArguments:
    given decodable: VariablesArguments is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class VariablesArguments(variablesReference: Int)

  object SetVariableArguments:
    given decodable: SetVariableArguments is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class SetVariableArguments(variablesReference: Int, name: Text, value: Text)

  object EvaluateArguments:
    given decodable: EvaluateArguments is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class EvaluateArguments
    ( expression: Text, frameId: Optional[Int] = Unset, context: Optional[Text] = Unset )

  object CompletionsArguments:
    given decodable: CompletionsArguments is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  // `column` is a 1-based character position within `text`, per the specification's default
  // units; `line` is accepted but unused, since this adapter completes single-line console
  // input.
  case class CompletionsArguments
    ( text: Text, column: Int, frameId: Optional[Int] = Unset, line: Optional[Int] = Unset )

  object SetExpressionArguments:
    given decodable: SetExpressionArguments is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class SetExpressionArguments(expression: Text, value: Text, frameId: Optional[Int] = Unset)

  object Breakpoint:
    given encodable: Breakpoint is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  // Response bodies and the structures they carry.
  case class Breakpoint
    ( verified: Boolean,
      id:       Optional[Int]  = Unset,
      line:     Optional[Int]  = Unset,
      message:  Optional[Text] = Unset )

  object BreakpointsBody:
    given encodable: BreakpointsBody is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class BreakpointsBody(breakpoints: List[Breakpoint])

  object DataBreakpointInfoBody:
    given encodable: DataBreakpointInfoBody is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class DataBreakpointInfoBody
    ( dataId: Optional[Text], description: Text, accessTypes: List[Text] = Nil )

  object ThreadInfo:
    given encodable: ThreadInfo is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class ThreadInfo(id: Int, name: Text)

  object ThreadsBody:
    given encodable: ThreadsBody is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class ThreadsBody(threads: List[ThreadInfo])

  object StackFrame:
    given encodable: StackFrame is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class StackFrame
    ( id:               Int,
      name:             Text,
      line:             Int,
      column:           Int = 0,
      source:           Optional[Source] = Unset,
      presentationHint: Optional[Text] = Unset )

  object StackTraceBody:
    given encodable: StackTraceBody is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class StackTraceBody(stackFrames: List[StackFrame], totalFrames: Optional[Int] = Unset)

  object Scope:
    given encodable: Scope is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class Scope(name: Text, variablesReference: Int, expensive: Boolean = false)

  object ScopesBody:
    given encodable: ScopesBody is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class ScopesBody(scopes: List[Scope])

  object VariableInfo:
    given encodable: VariableInfo is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class VariableInfo
    ( name:               Text,
      value:              Text,
      variablesReference: Int = 0,
      `type`:             Optional[Text] = Unset )

  object VariablesBody:
    given encodable: VariablesBody is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class VariablesBody(variables: List[VariableInfo])

  object SetVariableBody:
    given encodable: SetVariableBody is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class SetVariableBody(value: Text)

  object EvaluateBody:
    given encodable: EvaluateBody is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class EvaluateBody(result: Text, variablesReference: Int = 0)

  object CompletionItem:
    given encodable: CompletionItem is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  // `start` and `length` delimit the region of the request's `text` the completion replaces, in
  // the same 1-based units as the request's `column`; when omitted, the client inserts `label`
  // at the requested column.
  case class CompletionItem
    ( label:  Text,
      `type`: Optional[Text] = Unset,
      start:  Optional[Int]  = Unset,
      length: Optional[Int]  = Unset )

  object CompletionsBody:
    given encodable: CompletionsBody is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class CompletionsBody(targets: List[CompletionItem])

  object ExceptionInfoBody:
    given encodable: ExceptionInfoBody is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class ExceptionInfoBody
    ( exceptionId: Text, breakMode: Text, description: Optional[Text] = Unset )

  object ContinueBody:
    given encodable: ContinueBody is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class ContinueBody(allThreadsContinued: Boolean = true)

  object StoppedBody:
    given encodable: StoppedBody is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  // Event bodies.
  case class StoppedBody
    ( reason:            Text,
      threadId:          Optional[Int] = Unset,
      allThreadsStopped: Boolean = true,
      hitBreakpointIds:  List[Int] = Nil )

  object ContinuedBody:
    given encodable: ContinuedBody is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class ContinuedBody(threadId: Int, allThreadsContinued: Boolean = true)

  object OutputBody:
    given encodable: OutputBody is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class OutputBody(output: Text, category: Optional[Text] = Unset)

  object ExitedBody:
    given encodable: ExitedBody is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class ExitedBody(exitCode: Int)

  object BreakpointEventBody:
    given encodable: BreakpointEventBody is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class BreakpointEventBody(reason: Text, breakpoint: Breakpoint)
