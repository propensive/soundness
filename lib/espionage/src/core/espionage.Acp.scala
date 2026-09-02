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
package espionage

import java.io as ji
import java.util.concurrent as juc

import scala.caps

import ambience.*
import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import eucalyptus.*
import fulminate.*
import gossamer.*
import guillotine.*
import hieroglyph.*
import jacinta.*
import obligatory.*
import parasite.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*
import Acp.*

// The client half of the Agent Client Protocol (https://agentclientprotocol.com/), version 1: the
// role an editor plays, here for programmatic use. A program spawns an agent as a subprocess (or
// speaks to one behind a stream pair), negotiates capabilities, opens sessions, sends prompts,
// and receives the agent's streamed updates and callback requests — permissions, filesystem
// access, terminals — through handlers it registers before connecting.
object Acp:
  // The protocol version this library implements: a single integer, incremented by the
  // specification only for breaking changes.
  val version: Int = 1

  // The content-block vocabulary is MCP's, which ACP adopts verbatim; it lives in synesthesia's
  // `content` component — free of that library's HTTP stack — and is re-exported here so the ACP
  // vocabulary is complete under `Acp`.
  export synesthesia.Content.*

  object Envelope:
    // Pure and throwing, like the other derivation anchors; see `InitializeResult.decodable`.
    given encodable: Envelope is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

    given decodable: Envelope is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  // Any JSON-RPC message, read only for the two members that decide how it is handled: the method
  // it names — which a response does not — and the id it correlates on, which a notification does
  // not. Both are optional, so every message decodes.
  case class Envelope(method: Optional[Text] = Unset, id: Optional[Json] = Unset)

  // Initialization

  case class FsCapabilities
    ( readTextFile:  Optional[Boolean] = Unset,
      writeTextFile: Optional[Boolean] = Unset )

  case class ClientCapabilities
    ( fs:       Optional[FsCapabilities] = Unset,
      terminal: Optional[Boolean]        = Unset )

  case class PromptCapabilities
    ( image:           Optional[Boolean] = Unset,
      audio:           Optional[Boolean] = Unset,
      embeddedContext: Optional[Boolean] = Unset )

  case class McpCapabilities(http: Optional[Boolean] = Unset, sse: Optional[Boolean] = Unset)

  case class AgentCapabilities
    ( loadSession:        Optional[Boolean]            = Unset,
      promptCapabilities: Optional[PromptCapabilities] = Unset,
      mcpCapabilities:    Optional[McpCapabilities]    = Unset )

  case class AuthMethod(id: Text, name: Text, description: Optional[Text] = Unset)

  object InitializeResult:
    // Pure and throwing: each internal summon mints its own throwing tactic; a decode failure
    // surfaces as `Json.Error` handled at the transport. Threading a caller's tactic through the
    // capture-polymorphic derivation is rejected by separation checking. The other decoder
    // anchors below follow the same shape.
    given decodable: InitializeResult is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class InitializeResult
    ( protocolVersion:   Int,
      agentCapabilities: Optional[AgentCapabilities] = Unset,
      authMethods:       List[AuthMethod]            = Nil,
      _meta:             Optional[Json]              = Unset )

  // Sessions

  case class EnvVariable(name: Text, value: Text)

  // How the agent should reach a Model Context Protocol server the client wants connected to a
  // session: the stdio transport's launch specification.
  case class McpServer
    ( name:    Text,
      command: Text,
      args:    List[Text]        = Nil,
      env:     List[EnvVariable] = Nil )

  case class SessionMode(id: Text, name: Text, description: Optional[Text] = Unset)

  case class SessionModeState(currentModeId: Text, availableModes: List[SessionMode] = Nil)

  object NewSessionResult:
    given decodable: NewSessionResult is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class NewSessionResult
    ( sessionId: Text,
      modes:     Optional[SessionModeState] = Unset,
      _meta:     Optional[Json]             = Unset )

  // Prompt turns

  object StopReason:
    given encodable: StopReason is Json.Encodable = Json.Encodable(() => Morphology.Str):
      case StopReason.EndTurn         => t"end_turn".in[Json]
      case StopReason.MaxTokens       => t"max_tokens".in[Json]
      case StopReason.MaxTurnRequests => t"max_turn_requests".in[Json]
      case StopReason.Refusal         => t"refusal".in[Json]
      case StopReason.Cancelled       => t"cancelled".in[Json]

    given decodable: StopReason is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the decode cannot thread a
      // caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Str): json =>
          json.as[Text] match
            case t"end_turn"          => StopReason.EndTurn
            case t"max_tokens"        => StopReason.MaxTokens
            case t"max_turn_requests" => StopReason.MaxTurnRequests
            case t"refusal"           => StopReason.Refusal
            case t"cancelled"         => StopReason.Cancelled
            case _                    => abort(Json.Error(Json.Error.Reason.OutOfRange))

  // Why a prompt turn ended: the result of `session/prompt`.
  enum StopReason:
    case EndTurn, MaxTokens, MaxTurnRequests, Refusal, Cancelled

  object PromptResult:
    given decodable: PromptResult is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  case class PromptResult(stopReason: StopReason, _meta: Optional[Json] = Unset)

  // Tool calls

  object ToolKind:
    given encodable: ToolKind is Json.Encodable = Json.Encodable(() => Morphology.Str):
      case ToolKind.Read    => t"read".in[Json]
      case ToolKind.Edit    => t"edit".in[Json]
      case ToolKind.Delete  => t"delete".in[Json]
      case ToolKind.Move    => t"move".in[Json]
      case ToolKind.Search  => t"search".in[Json]
      case ToolKind.Execute => t"execute".in[Json]
      case ToolKind.Think   => t"think".in[Json]
      case ToolKind.Fetch   => t"fetch".in[Json]
      case ToolKind.Other   => t"other".in[Json]

    given decodable: ToolKind is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the decode cannot thread a
      // caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Str): json =>
          json.as[Text] match
            case t"read"    => ToolKind.Read
            case t"edit"    => ToolKind.Edit
            case t"delete"  => ToolKind.Delete
            case t"move"    => ToolKind.Move
            case t"search"  => ToolKind.Search
            case t"execute" => ToolKind.Execute
            case t"think"   => ToolKind.Think
            case t"fetch"   => ToolKind.Fetch
            case _          => ToolKind.Other

  enum ToolKind:
    case Read, Edit, Delete, Move, Search, Execute, Think, Fetch, Other

  object ToolCallStatus:
    given encodable: ToolCallStatus is Json.Encodable = Json.Encodable(() => Morphology.Str):
      case ToolCallStatus.Pending    => t"pending".in[Json]
      case ToolCallStatus.InProgress => t"in_progress".in[Json]
      case ToolCallStatus.Completed  => t"completed".in[Json]
      case ToolCallStatus.Failed     => t"failed".in[Json]
      case ToolCallStatus.Cancelled  => t"cancelled".in[Json]

    given decodable: ToolCallStatus is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the decode cannot thread a
      // caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Str): json =>
          json.as[Text] match
            case t"pending"     => ToolCallStatus.Pending
            case t"in_progress" => ToolCallStatus.InProgress
            case t"completed"   => ToolCallStatus.Completed
            case t"failed"      => ToolCallStatus.Failed
            case t"cancelled"   => ToolCallStatus.Cancelled
            case _              => abort(Json.Error(Json.Error.Reason.OutOfRange))

  enum ToolCallStatus:
    case Pending, InProgress, Completed, Failed, Cancelled

  case class ToolCallLocation(path: Text, line: Optional[Int] = Unset)

  object ToolCallContent:
    import dynamicJsonAccess.enabled

    private val typeTag = Json.discriminatedUnion[ToolCallContent](t"type")

    given encodable: ToolCallContent is Json.Encodable = Json.Encodable(() => Morphology.Any):
      case content: ToolContent  => typeTag.rewrite(t"content",  content.in[Json])
      case content: ToolDiff     => typeTag.rewrite(t"diff",     content.in[Json])
      case content: ToolTerminal => typeTag.rewrite(t"terminal", content.in[Json])

    given decodable: ToolCallContent is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the decode cannot thread a
      // caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Any): json =>
          json.`type`.as[Text] match
            case "content"  => json.as[ToolContent]
            case "diff"     => json.as[ToolDiff]
            case "terminal" => json.as[ToolTerminal]
            case _          => abort(Json.Error(Json.Error.Reason.OutOfRange))

  // What a tool call produced, as the agent reports it: ordinary content, a structured file
  // diff, or a reference to a terminal the agent runs a command in.
  sealed trait ToolCallContent

  object ToolContent:
    given decodable: ToolContent is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

    given encodable: ToolContent is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class ToolContent(content: ContentBlock) extends ToolCallContent

  object ToolDiff:
    given decodable: ToolDiff is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

    given encodable: ToolDiff is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class ToolDiff(path: Text, oldText: Optional[Text] = Unset, newText: Text)
  extends ToolCallContent

  object ToolTerminal:
    given decodable: ToolTerminal is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

    given encodable: ToolTerminal is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class ToolTerminal(terminalId: Text) extends ToolCallContent

  object ToolCall:
    given decodable: ToolCall is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  // A tool call as first reported (the `tool_call` update): only the id and title are required.
  case class ToolCall
    ( toolCallId: Text,
      title:      Text,
      kind:       Optional[ToolKind]       = Unset,
      status:     Optional[ToolCallStatus] = Unset,
      content:    List[ToolCallContent]    = Nil,
      locations:  List[ToolCallLocation]   = Nil,
      rawInput:   Optional[Json]           = Unset,
      rawOutput:  Optional[Json]           = Unset )
  extends SessionUpdate

  object ToolCallUpdate:
    given decodable: ToolCallUpdate is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

  // A later report about an existing tool call (the `tool_call_update` update): every member but
  // the id is optional, and a present member replaces what was previously reported. The same
  // shape carries the tool call a permission request concerns.
  case class ToolCallUpdate
    ( toolCallId: Text,
      title:      Optional[Text]                   = Unset,
      kind:       Optional[ToolKind]               = Unset,
      status:     Optional[ToolCallStatus]         = Unset,
      content:    Optional[List[ToolCallContent]]  = Unset,
      locations:  Optional[List[ToolCallLocation]] = Unset,
      rawInput:   Optional[Json]                   = Unset,
      rawOutput:  Optional[Json]                   = Unset )
  extends SessionUpdate

  // Plans

  object PlanPriority:
    given encodable: PlanPriority is Json.Encodable = Json.Encodable(() => Morphology.Str):
      case PlanPriority.High   => t"high".in[Json]
      case PlanPriority.Medium => t"medium".in[Json]
      case PlanPriority.Low    => t"low".in[Json]

    given decodable: PlanPriority is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the decode cannot thread a
      // caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Str): json =>
          json.as[Text] match
            case t"high"   => PlanPriority.High
            case t"medium" => PlanPriority.Medium
            case t"low"    => PlanPriority.Low
            case _         => abort(Json.Error(Json.Error.Reason.OutOfRange))

  enum PlanPriority:
    case High, Medium, Low

  object PlanStatus:
    given encodable: PlanStatus is Json.Encodable = Json.Encodable(() => Morphology.Str):
      case PlanStatus.Pending    => t"pending".in[Json]
      case PlanStatus.InProgress => t"in_progress".in[Json]
      case PlanStatus.Completed  => t"completed".in[Json]

    given decodable: PlanStatus is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the decode cannot thread a
      // caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Str): json =>
          json.as[Text] match
            case t"pending"     => PlanStatus.Pending
            case t"in_progress" => PlanStatus.InProgress
            case t"completed"   => PlanStatus.Completed
            case _              => abort(Json.Error(Json.Error.Reason.OutOfRange))

  enum PlanStatus:
    case Pending, InProgress, Completed

  case class PlanEntry(content: Text, priority: PlanPriority, status: PlanStatus)

  case class AvailableCommand(name: Text, description: Text, input: Optional[Json] = Unset)

  // Session updates

  object SessionUpdate:
    import dynamicJsonAccess.enabled

    private val typeTag = Json.discriminatedUnion[SessionUpdate](t"sessionUpdate")

    given encodable: SessionUpdate is Json.Encodable = Json.Encodable(() => Morphology.Any):
      case update: UserMessageChunk  => typeTag.rewrite(t"user_message_chunk", update.in[Json])
      case update: AgentMessageChunk => typeTag.rewrite(t"agent_message_chunk", update.in[Json])
      case update: AgentThoughtChunk => typeTag.rewrite(t"agent_thought_chunk", update.in[Json])
      case update: ToolCall          => typeTag.rewrite(t"tool_call", update.in[Json])
      case update: ToolCallUpdate    => typeTag.rewrite(t"tool_call_update", update.in[Json])
      case update: Plan              => typeTag.rewrite(t"plan", update.in[Json])

      case update: AvailableCommandsUpdate =>
        typeTag.rewrite(t"available_commands_update", update.in[Json])

      case update: CurrentModeUpdate => typeTag.rewrite(t"current_mode_update", update.in[Json])

    given decodable: SessionUpdate is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the decode cannot thread a
      // caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Any): json =>
          json.sessionUpdate.as[Text] match
            case "user_message_chunk"        => json.as[UserMessageChunk]
            case "agent_message_chunk"       => json.as[AgentMessageChunk]
            case "agent_thought_chunk"       => json.as[AgentThoughtChunk]
            case "tool_call"                 => json.as[ToolCall]
            case "tool_call_update"          => json.as[ToolCallUpdate]
            case "plan"                      => json.as[Plan]
            case "available_commands_update" => json.as[AvailableCommandsUpdate]
            case "current_mode_update"       => json.as[CurrentModeUpdate]
            case _                           => abort(Json.Error(Json.Error.Reason.OutOfRange))

  // One `session/update` notification: everything an agent reports about a turn as it runs,
  // discriminated on the wire by its `sessionUpdate` member.
  sealed trait SessionUpdate

  object UserMessageChunk:
    given decodable: UserMessageChunk is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

    given encodable: UserMessageChunk is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class UserMessageChunk(content: ContentBlock) extends SessionUpdate

  object AgentMessageChunk:
    given decodable: AgentMessageChunk is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

    given encodable: AgentMessageChunk is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class AgentMessageChunk(content: ContentBlock) extends SessionUpdate

  object AgentThoughtChunk:
    given decodable: AgentThoughtChunk is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

    given encodable: AgentThoughtChunk is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class AgentThoughtChunk(content: ContentBlock) extends SessionUpdate

  object Plan:
    given decodable: Plan is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

    given encodable: Plan is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class Plan(entries: List[PlanEntry] = Nil) extends SessionUpdate

  object AvailableCommandsUpdate:
    given decodable: AvailableCommandsUpdate is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

    given encodable: AvailableCommandsUpdate is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class AvailableCommandsUpdate(availableCommands: List[AvailableCommand] = Nil)
  extends SessionUpdate

  object CurrentModeUpdate:
    given decodable: CurrentModeUpdate is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

    given encodable: CurrentModeUpdate is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class CurrentModeUpdate(currentModeId: Text) extends SessionUpdate

  // Permissions

  object PermissionOptionKind:
    given encodable: PermissionOptionKind is Json.Encodable = Json.Encodable(() => Morphology.Str):
      case PermissionOptionKind.AllowOnce    => t"allow_once".in[Json]
      case PermissionOptionKind.AllowAlways  => t"allow_always".in[Json]
      case PermissionOptionKind.RejectOnce   => t"reject_once".in[Json]
      case PermissionOptionKind.RejectAlways => t"reject_always".in[Json]

    given decodable: PermissionOptionKind is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the decode cannot thread a
      // caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Str): json =>
          json.as[Text] match
            case t"allow_once"    => PermissionOptionKind.AllowOnce
            case t"allow_always"  => PermissionOptionKind.AllowAlways
            case t"reject_once"   => PermissionOptionKind.RejectOnce
            case t"reject_always" => PermissionOptionKind.RejectAlways
            case _                => abort(Json.Error(Json.Error.Reason.OutOfRange))

  enum PermissionOptionKind:
    case AllowOnce, AllowAlways, RejectOnce, RejectAlways

  object PermissionOption:
    given decodable: PermissionOption is Json.Decodable =
      import strategies.throwUnsafely
      caps.unsafe.unsafeAssumePure(Json.DecodableDerivation.derived)

    given encodable: PermissionOption is Json.Encodable =
      caps.unsafe.unsafeAssumePure(Json.EncodableDerivation.derived)

  case class PermissionOption(optionId: Text, name: Text, kind: PermissionOptionKind)

  object RequestPermissionOutcome:
    import dynamicJsonAccess.enabled

    private val typeTag = Json.discriminatedUnion[RequestPermissionOutcome](t"outcome")

    given encodable: RequestPermissionOutcome is Json.Encodable =
      Json.Encodable(() => Morphology.Any):
        case Selected(optionId) =>
          typeTag.rewrite(t"selected", Map(t"optionId" -> optionId.in[Json]).in[Json])

        case Cancelled =>
          typeTag.rewrite(t"cancelled", Map[Text, Json]().in[Json])

    given decodable: RequestPermissionOutcome is Json.Decodable =
      // Pure and throwing, like the derivation anchors: the decode cannot thread a
      // caller's tactic under separation checking.
      import strategies.throwUnsafely

      caps.unsafe.unsafeAssumePure:
        Json.Decodable(Morphology.Any): json =>
          json.outcome.as[Text] match
            case "selected"  => Selected(json.optionId.as[Text])
            case "cancelled" => Cancelled
            case _           => abort(Json.Error(Json.Error.Reason.OutOfRange))

  // The user's answer to a permission request: one of the offered options, or `Cancelled` — which
  // is also the mandatory answer to every request still pending when its turn is cancelled.
  sealed trait RequestPermissionOutcome
  case class Selected(optionId: Text) extends RequestPermissionOutcome
  case object Cancelled extends RequestPermissionOutcome

  case class RequestPermissionResult(outcome: RequestPermissionOutcome)

  // Filesystem and terminal results

  case class ReadTextFileResult(content: Text)
  case class CreateTerminalResult(terminalId: Text)

  case class TerminalExitStatus(exitCode: Optional[Int] = Unset, signal: Optional[Text] = Unset)

  case class TerminalOutputResult
    ( output:     Text,
      truncated:  Boolean,
      exitStatus: Optional[TerminalExitStatus] = Unset )

  // The bundle of terminal operations a client offers an agent, registered whole: the protocol
  // advertises terminal support as a single capability, so the five methods are all-or-nothing.
  trait Terminals:
    def create
      ( sessionId:       Text,
        command:         Text,
        args:            List[Text],
        env:             List[EnvVariable],
        cwd:             Optional[Text],
        outputByteLimit: Optional[Long] )
    :   Text

    def output(sessionId: Text, terminalId: Text): TerminalOutputResult
    def waitForExit(sessionId: Text, terminalId: Text): TerminalExitStatus
    def kill(sessionId: Text, terminalId: Text): Unit
    def release(sessionId: Text, terminalId: Text): Unit

  // Handler context shapes. Every handler is lent the session id it concerns, its tagged
  // payloads, and an error emitter; each shape is a single context-parameter clause, and results
  // are pure, so nothing lent to a handler can leave it — by return value, closure or task.

  type Turned1[payload, result] =
    (sessionId: Text aka "sessionId", payload: payload, emit: Emit[Acp.Error]^) ?=> result

  type Turned2[payload1, payload2, result] =
    ( sessionId: Text aka "sessionId",
      payload1:  payload1,
      payload2:  payload2,
      emit:      Emit[Acp.Error]^ )
    ?=> result

  type Turned3[payload1, payload2, payload3, result] =
    ( sessionId: Text aka "sessionId",
      payload1:  payload1,
      payload2:  payload2,
      payload3:  payload3,
      emit:      Emit[Acp.Error]^ )
    ?=> result

  // The handler signature of each feature, named for its registration combinator.

  type UpdateHandler = Turned1[SessionUpdate aka "update", Unit]

  type PermissionHandler =
    Turned2[ToolCallUpdate aka "toolCall", List[PermissionOption] aka "options",
        RequestPermissionOutcome]

  type ReadFileHandler =
    Turned3[Text aka "path", Optional[Int] aka "line", Optional[Int] aka "limit", Text]

  type WriteFileHandler = Turned2[Text aka "path", Text aka "content", Unit]

  // Contextual accessors, reading a handler's context by its tag; `connection` reads the live
  // connection `connect` lends to its block.

  transparent inline def connection(using connection: Acp.Connection): connection.type =
    connection

  inline def sessionId(using sessionId: Text aka "sessionId"): Text = sessionId()
  inline def update(using update: SessionUpdate aka "update"): SessionUpdate = update()

  inline def toolCall(using toolCall: ToolCallUpdate aka "toolCall"): ToolCallUpdate =
    toolCall()

  inline def options(using options: List[PermissionOption] aka "options")
  :   List[PermissionOption] =

    options()

  inline def path(using path: Text aka "path"): Text = path()
  inline def line(using line: Optional[Int] aka "line"): Optional[Int] = line()
  inline def limit(using limit: Optional[Int] aka "limit"): Optional[Int] = limit()
  inline def content(using content: Text aka "content"): Text = content()

  // Registration combinators, each writing a boxed handler into the lent registry. Inline, so the
  // registry capability flows from the use site rather than a fresh root minted at the method
  // boundary.

  transparent inline def updated(handler: UpdateHandler)(using registry: Acp.Registry^): Unit =
    registry.updated0 = Acp.Registry.Slot[UpdateHandler](handler)

  transparent inline def permission(handler: PermissionHandler)(using registry: Acp.Registry^)
  :   Unit =

    registry.permission0 = Acp.Registry.Slot[PermissionHandler](handler)

  transparent inline def readFile(handler: ReadFileHandler)(using registry: Acp.Registry^)
  :   Unit =

    registry.readFile0 = Acp.Registry.Slot[ReadFileHandler](handler)

  transparent inline def writeFile(handler: WriteFileHandler)(using registry: Acp.Registry^)
  :   Unit =

    registry.writeFile0 = Acp.Registry.Slot[WriteFileHandler](handler)

  transparent inline def terminals(handler: Terminals)(using registry: Acp.Registry^): Unit =
    registry.terminal0 = Acp.Registry.Slot[Terminals](handler)

  // An escape hatch: adjusts the derived capabilities, applied last, at initialization.
  transparent inline def capabilities(adjust: ClientCapabilities => ClientCapabilities)
    ( using registry: Acp.Registry^ )
  :   Unit =

    registry.adjust0 = adjust

  // Connects to an ACP agent for the duration of a lambda. The first block registers the
  // client's handlers on the lent registry; once it returns, the registry is consumed and
  // frozen, the client's capabilities are derived from what was registered, and the exchange
  // opens: a writer task drains outgoing messages onto the agent's input, a reader task routes
  // what comes back, and `initialize` is exchanged — aborting with `Incompatible` if the agent
  // answers with a protocol version other than this library's. The second block then holds a
  // live, initialized connection; when it returns, the exchange is torn down and a subprocess
  // agent is killed, so nothing can outlive the agent it speaks to. Everything stateful —
  // registry, service, dispatchers, connection — is local to this call: nothing
  // capability-carrying is ever stored in an application-lifetime object. `observer`, if given,
  // sees every message in both directions as it crosses the transport.
  //
  // `capture` lets the connection block capture the monitor, which a block that spawns a task —
  // to prompt and cancel concurrently, say — needs; exegesis's `Lsp.proxy` takes the same shape.
  def connect[result, capture^](agent: Agent, observer: Observer = Observer.Silent)
    ( register: (registry: Acp.Registry^) ?=> Unit )
    ( lambda: (connection: Acp.Connection) ?->{capture} result )
    ( using Monitor^{capture}, Probate, Diagnostics, WorkingDirectory, Tactic[Acp.Error] )
  :   result =

    import strategies.throwUnsafely

    val registry: Acp.Registry^ = Acp.Registry()
    register(using registry)

    val state: State = State()
    val service: Service^ = Service(registry, state)

    def open(sink: (Intake[Data] over Credit)^, read: (Text => Unit) => Unit): result =
      Exchange.exchange(service, state, observer, sink, read): connection =>
        connection.initialize(service.capabilities)
        lambda(using connection)

    agent match
      case Agent.Streams(input, output) =>
        val streamable = summon[ji.InputStream is Streamable by Data over Credit]
        val sink = summon[ji.OutputStream is Sink by Data over Credit].intake(output)

        try open(sink, Transport.pump(streamable.stream(input), observer)(_)) finally sink.finish()

      case Agent.Process(command) =>
        // Launched with a silent logger and a throwing tactic: a failure to launch has no
        // caller's `Tactic` to be raised through here, and an exec log has no channel to reach —
        // an ACP agent owns standard output.
        import logging.silentLogging

        val job = command.fork[Exit]()

        // The child is killed rather than waited for: an agent that never notices its input
        // closing — or that outlives an exchange that ended early — would otherwise outlive the
        // session it was lent to.
        try open(job.intake, Transport.pump(job.stdout(), observer)(_)) finally job.abort()

  // The method a message names, or `Unset` if it names none — which marks it a response.
  private[espionage] def method(json: Json): Optional[Text] = envelope(json).method

  // The id a message correlates on, for a message of any kind.
  private[espionage] def identifier(json: Json): Optional[Json] = envelope(json).id

  private[espionage] def envelope(json: Json): Envelope =
    import strategies.throwUnsafely
    try json.as[Envelope] catch case _: Exception => Envelope()

  private[espionage] def requestId(json: Json): Optional[Json] =
    // Throwing rather than `safely`: the request decodable cannot thread the boundary tactic
    // under separation checking, and an unreadable id is simply absent.
    import strategies.throwUnsafely
    try json.as[JsonRpc.Request].id catch case _: Exception => Unset

  // An observer of the raw traffic crossing the transport, for a client that exposes a log of
  // the messages it exchanges. Each message is reported as the text that was read from — or
  // framed onto — the wire, before parsing, so a malformed message is observed too.
  object Observer:
    // The default: a client that does not expose its traffic pays nothing for the hook.
    object Silent extends Observer:
      def received(message: Text): Unit = ()
      def sent(message: Text): Unit = ()

    given silent: Observer = Silent

  trait Observer:
    def received(message: Text): Unit
    def sent(message: Text): Unit

  object Agent:
    // An agent launched as a subprocess: how a client normally starts one.
    def apply(command: guillotine.Command): Agent = Agent.Process(command)

    // An agent already running behind a pair of streams. Widened to `Agent`, which is what
    // `connect` is indexed by.
    def streams(input: ji.InputStream, output: ji.OutputStream): Agent =
      Agent.Streams(input, output)

  // An ACP agent this process can speak to: the far end of the exchange `connect` opens.
  //
  // A plain value, holding no capability: the channel is minted when the connection opens and
  // disposed of when it ends, and a stream or an intake — single-owner, and dead once the agent
  // is gone — has no business being carried in a description of where to find an agent.
  enum Agent:
    // Standard input and output of a subprocess carry the protocol.
    case Process(command: guillotine.Command)

    // An agent already running behind a pair of byte streams: one in this process, one behind a
    // socket, or a fixture in a test.
    case Streams(input: ji.InputStream, output: ji.OutputStream)

  // AcpError → Acp.Error
  object Error:
    // Each reason carries its JSON-RPC wire code (`code`), alongside the sequential `number`
    // used for the SN-148 diagnostic. A fault raised in a handler is converted into an error
    // response whose `error.code` is the reason's wire code.
    enum Reason(val number: Int, val code: Int) extends Clarification:
      case Parse          extends Reason(1, -32700)
      case InvalidRequest extends Reason(2, -32600)
      case MethodNotFound extends Reason(3, -32601)
      case InvalidParams  extends Reason(4, -32602)
      case Internal       extends Reason(5, -32603)
      case AuthRequired   extends Reason(6, -32000)
      case Incompatible   extends Reason(7, 0)

    // The inverse of `code`: recovers the reason from an error response's wire code, for a
    // client reading a fault an agent sent it. A code outside the standard set is `Unset` here,
    // and reported as `Internal` by the caller. (`Incompatible` is local — a protocol-version
    // mismatch discovered at initialization — and its zero is not a wire code.)
    def reason(code: Int): Optional[Reason] =
      var found: Optional[Reason] = Unset
      var index: Int = 0

      while index < Reason.values.length do
        val reason = Reason.values(index)
        if reason.code == code && reason.code != 0 then found = reason
        index += 1

      found

    given communicable: Reason is Communicable =
      case Reason.Parse          => m"the message could not be parsed as JSON"
      case Reason.InvalidRequest => m"the message was not a valid JSON-RPC request"
      case Reason.MethodNotFound => m"the method name was not recognised"
      case Reason.InvalidParams  => m"the parameters were not valid for the requested method"
      case Reason.Internal       => m"an internal error occurred"
      case Reason.AuthRequired   => m"the agent requires authentication"

      case Reason.Incompatible =>
        m"the agent does not support the protocol version this client implements"

  case class Error(reason: Acp.Error.Reason, details: Optional[Text] = Unset)(using Diagnostics)
  extends fulminate.Error(148, reason.number)(m"the ACP operation failed because $reason"):
    // The message sent to the agent in the error response: the given details, or the reason's
    // standard description.
    def response: Text = details.or(reason.communicate.text)

  // AcpRegistry → Acp.Registry
  object Registry:
    // A context-function value adapts to any non-context-function expected type by being
    // applied, so a handler cannot inhabit an `Optional[...]` union directly: the box holds it
    // intact.
    case class Slot[handler](value: handler)

  // The target of the registration combinators, lent to the block given to `Acp.connect` for its
  // duration, then consumed by the service it configures: registration after the exchange begins
  // is impossible by construction. An exclusive capability, so it cannot escape the block; its
  // slots are public untracked vars — the combinators are inline, assigning from their expansion
  // sites. The slots are an erased rim (`AnyRef | Null`): a union mentioning a context-function
  // type freshens its capture sets at every adaptation, so the typed boundary is the combinator
  // (whose parameter is the pure handler type — a handler closing over the registry is rejected
  // there) and the service's invocation helpers, which restore the type by cast.
  class Registry private[espionage] () extends caps.ExclusiveCapability:

    @scala.caps.unsafe.untrackedCaptures
    var updated0: AnyRef | Null = null

    @scala.caps.unsafe.untrackedCaptures
    var permission0: AnyRef | Null = null

    @scala.caps.unsafe.untrackedCaptures
    var readFile0: AnyRef | Null = null

    @scala.caps.unsafe.untrackedCaptures
    var writeFile0: AnyRef | Null = null

    @scala.caps.unsafe.untrackedCaptures
    var terminal0: AnyRef | Null = null

    @scala.caps.unsafe.untrackedCaptures
    var adjust0: Optional[ClientCapabilities => ClientCapabilities] = Unset

    // The capabilities the client advertises at initialization, derived from what was
    // registered, so the declaration can never disagree with the implementation; `adjust0`, if
    // registered, is applied last.
    private[espionage] def capabilities: ClientCapabilities =
      def flag(slot: AnyRef | Null): Optional[Boolean] = if slot == null then Unset else true

      val fs: Optional[FsCapabilities] =
        if readFile0 == null && writeFile0 == null then Unset
        else FsCapabilities(flag(readFile0), flag(writeFile0))

      val derived = ClientCapabilities(fs, flag(terminal0))
      adjust0.lay(derived)(_(derived))

  // AcpConnection → Acp.Connection
  // The client's half of an ACP exchange: the handle a program holds on a running agent. It is a
  // capability, lent by `Acp.connect` for the duration of a lambda and disposed of afterwards,
  // so it cannot outlive the agent it speaks to.
  //
  // Outbound messages are put on the inherited `JsonRpc` channel, which the exchange's writer
  // drains onto the transport; inbound messages are read by the exchange's reader and routed. A
  // request blocks the caller until its response arrives, but never blocks the reader, so
  // several requests may be in flight at once — a prompt turn stays open while its updates
  // stream — and may be answered out of order.
  class Connection private[espionage] (state: State)(using Monitor, Diagnostics)
  extends JsonRpc, caps.ExclusiveCapability:
    type Origin = AcpAgent

    import strategies.throwUnsafely

    // One proxy module per sub-interface, both sharing this instance's outgoing channel. The
    // connection is confined by its own type and each proxy is a member of it, so sealing the
    // reference the generated modules hold is sound; the macro cannot take a capability-typed
    // splice.
    private val channel: JsonRpc = caps.unsafe.unsafeAssumePure(this)

    val lifecycle: AcpAgentLifecycle =
      caps.unsafe.unsafeAssumePure(channel.proxy[AcpAgentLifecycle])

    val sessions: AcpAgentSession = caps.unsafe.unsafeAssumePure(channel.proxy[AcpAgentSession])

    // What the agent reported at initialization: pure data, recorded by `initialize`.
    @scala.caps.unsafe.untrackedCaptures
    private var initialized0: Optional[InitializeResult] = Unset

    // A fault the agent reports as an error response arrives as a `JsonRpc.Error` carrying the
    // wire code, which is exactly the vocabulary of `Acp.Error.Reason`; a code outside the
    // standard set is reported as `Internal`, with the agent's own message as the detail.
    private def ask[result](block: => result)(using Tactic[Acp.Error]): result =
      try block catch case error: JsonRpc.Error =>
        abort
         ( Acp.Error
            ( error.code.let(Acp.Error.reason(_)).or(Acp.Error.Reason.Internal),
              error.detail ) )

    // The raw seam: sends a message exactly as given, without minting an id or awaiting an
    // answer, for the methods this library does not model.
    def send(message: Json): Unit = put(message)

    // Negotiates the protocol version and capabilities. The client sends the latest version it
    // supports; an agent that cannot speak it answers with its own latest, which this client —
    // implementing exactly one — must then reject.
    def initialize(capabilities: ClientCapabilities = ClientCapabilities())
      ( using Tactic[Acp.Error] )
    :   InitializeResult =

      val result = ask(lifecycle.initialize(Acp.version, capabilities))
      if result.protocolVersion != Acp.version then abort(Acp.Error(Error.Reason.Incompatible))
      initialized0 = result
      result

    def authenticate(methodId: Text)(using Tactic[Acp.Error]): Unit =
      ask(lifecycle.authenticate(methodId)) yet ()

    def newSession(cwd: Text, mcpServers: List[McpServer] = Nil)(using Tactic[Acp.Error])
    :   NewSessionResult =

      ask(sessions.`session/new`(cwd, mcpServers))

    // Resumes an existing session: the agent replays the whole conversation through
    // `session/update` notifications — routed to the registered update handler — before this
    // returns.
    def loadSession(sessionId: Text, cwd: Text, mcpServers: List[McpServer] = Nil)
      ( using Tactic[Acp.Error] )
    :   Unit =

      ask(sessions.`session/load`(sessionId, cwd, mcpServers)) yet ()

    // One prompt turn: blocks until the agent reports why the turn ended, while its updates
    // stream concurrently to the registered update handler. Opening a new turn resets the
    // session's cancellation, so a client may prompt again after cancelling.
    def prompt(sessionId: Text, content: List[ContentBlock])(using Tactic[Acp.Error])
    :   StopReason =

      state.begin(sessionId)
      ask(sessions.`session/prompt`(sessionId, content)).stopReason

    // The plain-text form of `prompt`.
    def prompt(sessionId: Text, text: Text)(using Tactic[Acp.Error]): StopReason =
      prompt(sessionId, List(TextContent(text)))

    // Cancels the session's current turn. A notification, not a request: the turn's outcome is
    // the in-flight `session/prompt` response, which the agent must complete with the
    // `Cancelled` stop reason. Permission requests pending when this is called — and any that
    // arrive before the next turn opens — are answered `Cancelled`, as the protocol requires.
    def cancel(sessionId: Text): Unit =
      state.cancel(sessionId)
      sessions.`session/cancel`(sessionId)

    def setMode(sessionId: Text, modeId: Text)(using Tactic[Acp.Error]): Unit =
      ask(sessions.`session/set_mode`(sessionId, modeId)) yet ()

    // What the agent reported at initialization, for a caller that has initialized.
    def agentCapabilities: Optional[AgentCapabilities] = initialized0.let(_.agentCapabilities)
    def authMethods: List[AuthMethod] = initialized0.lay(Nil)(_.authMethods)

  // AcpState → Acp.State
  // The per-connection state shared between the connection (which cancels turns and opens new
  // ones) and the service (which must answer permission requests for a cancelled turn with the
  // `Cancelled` outcome). Pure, concurrent data: it is written by the caller's thread and read by
  // the reader's dispatch tasks.
  private[espionage] class State:
    private val flags: juc.ConcurrentHashMap[Text, java.lang.Boolean] = juc.ConcurrentHashMap()

    private[espionage] def cancel(sessionId: Text): Unit =
      flags.put(sessionId, java.lang.Boolean.TRUE)

    // Opening a turn clears the session's cancellation, so a client may prompt again after
    // cancelling.
    private[espionage] def begin(sessionId: Text): Unit = flags.remove(sessionId)

    private[espionage] def cancelled(sessionId: Text): Boolean = flags.get(sessionId) != null

  // AcpService → Acp.Service
  // One live ACP client service: the bridge between the macro-generated JSON-RPC dispatchers and
  // the registered handlers. It is created per `Acp.connect` invocation and lives only in its
  // frame; it is an exclusive capability, so it cannot be stored in an application-lifetime
  // object.
  private[espionage] object Service:
    // The fault recorded by the current dispatch's emitter, if any; collected (and cleared) after
    // each dispatch and turned into an error response. Unlike `LspSession`, whose single-threaded
    // dispatch loop lets one slot serve every dispatch, agent requests here are dispatched on
    // spawned tasks — a permission handler may block on user input while updates keep streaming —
    // so concurrent dispatches must not cross their faults: the slot is per-thread, and `conclude`
    // runs on the thread that dispatched. The cell is a separate pure-data object rather than the
    // service itself, so the emitter lent to a handler captures only the cell.
    private[espionage] class Cell:
      private val fault0: ThreadLocal[Acp.Error | Null] = ThreadLocal()

      private[espionage] def record(error: Acp.Error): Unit = fault0.set(error)

      private[espionage] def collect(): Optional[Acp.Error] = fault0.get() match
        case null             => Unset
        case error: Acp.Error => fault0.set(null) yet error

    // Building the service consumes the registry: nothing can register once the exchange begins.
    private[espionage] def apply(consume registry: Registry^, state: State): Service^ =
      new Service(registry, state)

  private[espionage] class Service private (handlers: Registry^, state: State)
  extends AcpClient, caps.ExclusiveCapability:
    // Confined to this class rather than the file: `Acp.Error`'s constructor needs a
    // `Diagnostics`, and an ambient one would compete with the connection's own.
    import errorDiagnostics.stackTracesDiagnostics

    private val fault0: Service.Cell = Service.Cell()

    // The capabilities to advertise at initialization, derived from what was registered.
    private[espionage] val capabilities: ClientCapabilities = handlers.capabilities

    private[espionage] def fault(): Optional[Acp.Error] = fault0.collect()

    // The fault-aware conclusion of one dispatch: a fault recorded by the handler pre-empts its
    // result — for a request it becomes the error response, echoing the request's id; for a
    // notification, which may not be answered, it is discarded (this direction has no channel to
    // report it on).
    private[espionage] def conclude(json: Json, response: Optional[Json]): Optional[Json] =
      fault().lay(response): fault =>
        Acp.requestId(json).let(JsonRpc.failure(fault.reason.code, fault.response, _))

    private def emitter(): Emit[Acp.Error] =
      val cell = fault0

      Emit[Acp.Error]: fault => cell.record(fault)

    // Records a `MethodNotFound` fault for a request whose capability was never advertised. A
    // conformant agent checks the capability before calling, but a misbehaving one must receive an
    // answer rather than hang awaiting one.
    private def unregistered[result](default: result): result =
      fault0.record(Acp.Error(Acp.Error.Reason.MethodNotFound))
      default

    // Invocation helpers: one per handler shape. Each looks up the handler (`null` means
    // unregistered), and applies it with its tagged payloads. The registry's slots are an erased
    // rim, so each helper restores the handler's type with a single, annotated cast; the
    // annotation also stops the context-function value being applied at the definition.

    private def turned1[payload, result](default: result)(handler: AnyRef | Null)
      ( sessionId: Text, payload: payload )
    :   result =

      if handler == null then default else
        val invoke: Turned1[payload, result] =
          handler.asInstanceOf[Acp.Registry.Slot[Turned1[payload, result]]].value

        invoke(using sessionId.aka["sessionId"], payload, emitter())

    private def turned2[payload1, payload2, result](default: result)(handler: AnyRef | Null)
      ( sessionId: Text, payload1: payload1, payload2: payload2 )
    :   result =

      if handler == null then default else
        val invoke: Turned2[payload1, payload2, result] =
          handler.asInstanceOf[Acp.Registry.Slot[Turned2[payload1, payload2, result]]].value

        invoke(using sessionId.aka["sessionId"], payload1, payload2, emitter())

    private def turned3[payload1, payload2, payload3, result](default: result)
      ( handler: AnyRef | Null )
      ( sessionId: Text, payload1: payload1, payload2: payload2, payload3: payload3 )
    :   result =

      if handler == null then default else
        val invoke: Turned3[payload1, payload2, payload3, result] =
          handler.asInstanceOf[Acp.Registry.Slot[Turned3[payload1, payload2, payload3, result]]]
          . value

        invoke(using sessionId.aka["sessionId"], payload1, payload2, payload3, emitter())

    // The terminal bundle, restored from its slot; `null` if terminals were never registered.
    private def terminals: Terminals | Null =
      if handlers.terminal0 == null then null
      else handlers.terminal0.asInstanceOf[Acp.Registry.Slot[Terminals]].value

    // Session

    def `session/update`(sessionId: Text, update: SessionUpdate): Unit =
      turned1[SessionUpdate aka "update", Unit](())(handlers.updated0)
       ( sessionId, update.aka["update"] )

    // The protocol requires a permission request pending when its turn is cancelled to be
    // answered `Cancelled`. A request arriving after cancellation is answered without troubling
    // the handler; one already blocking in the handler when the cancellation arrives is answered
    // `Cancelled` when the handler returns, its selection discarded. (The response is sent when
    // the handler completes, not at the instant of cancellation: an answer that cannot take
    // effect arriving late is harmless, and it spares the dispatch a race against its own
    // handler.)
    def `session/request_permission`
      ( sessionId: Text, toolCall: ToolCallUpdate, options: List[PermissionOption] )
    :   RequestPermissionResult =

      if state.cancelled(sessionId) then RequestPermissionResult(Cancelled) else
        val outcome: RequestPermissionOutcome =
          turned2
           [ ToolCallUpdate aka "toolCall",
             List[PermissionOption] aka "options",
             RequestPermissionOutcome ]
           ( Cancelled )
           ( handlers.permission0 )
           ( sessionId, toolCall.aka["toolCall"], options.aka["options"] )

        RequestPermissionResult(if state.cancelled(sessionId) then Cancelled else outcome)

    // Filesystem

    def `fs/read_text_file`
      ( sessionId: Text, path: Text, line: Optional[Int], limit: Optional[Int] )
    :   ReadTextFileResult =

      if handlers.readFile0 == null then unregistered(ReadTextFileResult(t"")) else
        val content: Text =
          turned3[Text aka "path", Optional[Int] aka "line", Optional[Int] aka "limit", Text]
           ( t"" )
           ( handlers.readFile0 )
           ( sessionId, path.aka["path"], line.aka["line"], limit.aka["limit"] )

        ReadTextFileResult(content)

    def `fs/write_text_file`(sessionId: Text, path: Text, content: Text): Json =
      if handlers.writeFile0 == null then unregistered(()) else
        turned2[Text aka "path", Text aka "content", Unit]
         ( () )
         ( handlers.writeFile0 )
         ( sessionId, path.aka["path"], content.aka["content"] )

      Json.ast(Json.Ast(Json.JsonNull))

    // Terminals

    def `terminal/create`
      ( sessionId:       Text,
        command:         Text,
        args:            Optional[List[Text]],
        env:             Optional[List[EnvVariable]],
        cwd:             Optional[Text],
        outputByteLimit: Optional[Long] )
    :   CreateTerminalResult =

      terminals match
        case null => unregistered(CreateTerminalResult(t""))

        case terminals: Terminals =>
          CreateTerminalResult
           ( terminals.create
              ( sessionId, command, args.or(Nil), env.or(Nil), cwd, outputByteLimit ) )

    def `terminal/output`(sessionId: Text, terminalId: Text): TerminalOutputResult =
      terminals match
        case null                 => unregistered(TerminalOutputResult(t"", false))
        case terminals: Terminals => terminals.output(sessionId, terminalId)

    def `terminal/wait_for_exit`(sessionId: Text, terminalId: Text): TerminalExitStatus =
      terminals match
        case null                 => unregistered(TerminalExitStatus())
        case terminals: Terminals => terminals.waitForExit(sessionId, terminalId)

    def `terminal/kill`(sessionId: Text, terminalId: Text): Json =
      terminals match
        case null                 => unregistered(())
        case terminals: Terminals => terminals.kill(sessionId, terminalId)

      Json.ast(Json.Ast(Json.JsonNull))

    def `terminal/release`(sessionId: Text, terminalId: Text): Json =
      terminals match
        case null                 => unregistered(())
        case terminals: Terminals => terminals.release(sessionId, terminalId)

      Json.ast(Json.Ast(Json.JsonNull))

  // AcpTransport → Acp.Transport
  // The Agent Client Protocol's stdio framing: newline-delimited JSON, one UTF-8 message per line,
  // with no embedded newlines — deliberately simpler than LSP's `Content-Length` headers. Splitting
  // happens on the raw `Lf` byte before any text decoding, so multi-byte UTF-8 content passes
  // through unharmed. The framing is symmetrical, so it lives here and each end of an exchange
  // supplies its own channel.
  private[espionage] object Transport:
    // A message with its terminator. Encoded compactly at the write site: an indented encoding
    // would embed the newlines the framing forbids.
    def frame(body: Text): Data =
      import charEncoders.utf8Encoder
      t"$body\n".in[Data]

    // Reads framed messages from a channel until it is exhausted, handing each to `receive`. A
    // blank line — including the remnant of a `\r\n` terminator — carries no message and is
    // skipped rather than reported. The observer sees the message as it arrived — before parsing,
    // so a malformed message is observed too, and without the framing, so both directions read
    // alike in a log.
    def pump(consume source: (Stream[Data] over Credit)^, observer: Acp.Observer^)
      ( receive: Text => Unit )
    :   Unit =

      import strategies.throwUnsafely

      source.chunks.frames[Linefeed].each: frame =>
        val message: Text = frame.utf8

        if message.length > 0 && message != t"\r" then
          observer.received(message)
          receive(message)

  // AcpExchange → Acp.Exchange
  // Establishes an exchange with an ACP agent: opens the channel to it, starts a writer draining
  // the connection's outgoing messages onto it and a reader routing what comes back, lends the
  // connection, and tears everything down afterwards.
  //
  // The reader is its own task, which is what lets a caller block on a response: a request awaits
  // its promise while the reader goes on reading, so a prompt turn stays open while its updates
  // stream, and several requests may be in flight at once, answered out of order.
  private[espionage] object Exchange:
    // A free function, not a method: the reader is supplied as a partly-applied `pump`, whose
    // closure captures the observer, and a method of the exchange would have that same observer in
    // its own prefix — an overlap separation checking rejects.
    // `capture` flows through from `Acp.connect`: the lent block may capture the monitor (to
    // spawn tasks of its own), so the lambda — and the monitor it overlaps with — must both be
    // typed to admit it.
    private[espionage] def exchange[result, capture^]
      ( service:  Service^,
        state:    State,
        observer: Acp.Observer,
        sink:     (Intake[Data] over Credit)^,
        read:     (Text => Unit) => Unit )
      ( lambda: Acp.Connection ->{capture} result )
      ( using Monitor^{capture}, Probate, Diagnostics )
    :   result =

      import strategies.throwUnsafely
      import Json.jsonEncodableInText

      // Sealed: the connection captures this exchange's monitor and diagnostics, and an honest
      // `Acp.Connection^` would hide them from the writer and reader that serve it. It is a local
      // of this method, lent to `lambda` and dead once `lambda` returns.
      val connection: Acp.Connection = caps.unsafe.unsafeAssumePure(Acp.Connection(state))

      // The service is confined by its own type and the dispatch closures are locals of this
      // method, so sealing the reference the generated dispatchers hold is sound; the macro cannot
      // take a capability-typed splice. One dispatcher per served interface, so each generated
      // class stays within the JVM constant-pool limit.
      val serving: AcpClient = caps.unsafe.unsafeAssumePure(service)

      val sessionDispatch: Json => Optional[Json] =
        caps.unsafe.unsafeAssumeSeparate(JsonRpc.serve[AcpClientSession](serving))

      val fsDispatch: Json => Optional[Json] =
        caps.unsafe.unsafeAssumeSeparate(JsonRpc.serve[AcpClientFs](serving))

      val terminalDispatch: Json => Optional[Json] =
        caps.unsafe.unsafeAssumeSeparate(JsonRpc.serve[AcpClientTerminal](serving))

      val sessionMethods: List[Text] = JsonRpc.methods[AcpClientSession]
      val fsMethods: List[Text] = JsonRpc.methods[AcpClientFs]
      val terminalMethods: List[Text] = JsonRpc.methods[AcpClientTerminal]

      // A single writer, so writes never interleave. The encoding is compact — the framing
      // forbids embedded newlines. The observer sees the encoded body, not the terminator.
      val writer: Task[Unit] = async:
        // `.stdlib.iterator`: drained element by element, without memoizing the live chain.
        connection.outgoing.stdlib.iterator.each: json =>
          val body: Text = json.encode
          observer.sent(body)
          sink.put(Transport.frame(body))
          sink.flush()

      // Runs one dispatch and sends its conclusion. Faults become error responses; a message the
      // dispatcher cannot decode is answered rather than dropped, so the agent never hangs
      // awaiting an answer.
      def serve(dispatch: Json => Optional[Json])(json: Json): Unit =
        val id: Optional[Json] = Acp.requestId(json)

        val response: Optional[Json] =
          try dispatch(json) catch
            case error: Json.Error => JsonRpc.failure(-32602, t"Invalid params", id)
            case error: Exception  => JsonRpc.failure(-32603, t"Internal error", id)

        service.conclude(json, response).let(connection.put)

      // The channel is read by a partly-applied `pump`, rather than by handing the stream over:
      // the stream is single-owner, so it is minted and consumed within the reader task, which
      // also keeps this thread off the channel's first refill — a blocking read that would
      // otherwise happen before the writer that unblocks it has started.
      //
      // Routing differs by message kind. A response is handed to any dispatcher for correlation. A
      // notification — `session/update`, whose chunk ordering is semantically significant — is
      // dispatched synchronously on the reader task. A request (`session/request_permission`,
      // `fs/*`, `terminal/*`) is dispatched on a spawned task: a permission handler may block on a
      // decision for minutes, and running it here would stall both update streaming and response
      // correlation. (This is the deliberate divergence from LSP's single-loop server, whose
      // reverse direction is notifications-only for exactly that reason.)
      val reader: Task[Unit] = async:
        read: message =>
          safely(message.as[Json]).let: json =>
            Acp.method(json).lay(sessionDispatch(json) yet ()): method =>
              if sessionMethods.has(method) then
                if method == t"session/update" then serve(sessionDispatch)(json)
                else async(serve(sessionDispatch)(json)) yet ()
              else if fsMethods.has(method) then
                async(serve(fsDispatch)(json)) yet ()
              else if terminalMethods.has(method) then
                async(serve(terminalDispatch)(json)) yet ()
              else
                // A method this client does not model: a request is answered, so the agent never
                // hangs, and a notification is ignored, as the protocol allows.
                Acp.identifier(json).let: id =>
                  connection.put(JsonRpc.failure(-32601, t"Method not found", id))

      try lambda(connection) finally
        reader.cancel()
        writer.cancel()

// The client→agent request/notification surface, split so that each interface can be compiled
// into its own JSON-RPC proxy class. The method names are the wire methods; return type `Unit`
// marks a notification.

trait AcpAgentLifecycle:
  @rpc
  def initialize(protocolVersion: Int, clientCapabilities: Acp.ClientCapabilities)
  :   Acp.InitializeResult

  @rpc
  def authenticate(methodId: Text): Json

trait AcpAgentSession:
  @rpc
  def `session/new`(cwd: Text, mcpServers: List[Acp.McpServer]): Acp.NewSessionResult

  @rpc
  def `session/load`(sessionId: Text, cwd: Text, mcpServers: List[Acp.McpServer]): Json

  @rpc
  def `session/prompt`(sessionId: Text, prompt: List[Acp.ContentBlock]): Acp.PromptResult

  @rpc
  def `session/cancel`(sessionId: Text): Unit

  @rpc
  def `session/set_mode`(sessionId: Text, modeId: Text): Json

// The whole agent surface: what `Acp.Connection`'s generated proxies call.
trait AcpAgent extends AcpAgentLifecycle, AcpAgentSession

// The agent→client surface this client serves, split (as above) into one generated dispatcher
// class per interface. `session/update` is the protocol's only notification in this direction;
// everything else is a request the agent blocks on.

trait AcpClientSession:
  @rpc
  def `session/update`(sessionId: Text, update: Acp.SessionUpdate): Unit

  @rpc
  def `session/request_permission`
    ( sessionId: Text,
      toolCall:  Acp.ToolCallUpdate,
      options:   List[Acp.PermissionOption] )
  :   Acp.RequestPermissionResult

trait AcpClientFs:
  @rpc
  def `fs/read_text_file`
    ( sessionId: Text, path: Text, line: Optional[Int], limit: Optional[Int] )
  :   Acp.ReadTextFileResult

  @rpc
  def `fs/write_text_file`(sessionId: Text, path: Text, content: Text): Json

trait AcpClientTerminal:
  @rpc
  def `terminal/create`
    ( sessionId:       Text,
      command:         Text,
      args:            Optional[List[Text]],
      env:             Optional[List[Acp.EnvVariable]],
      cwd:             Optional[Text],
      outputByteLimit: Optional[Long] )
  :   Acp.CreateTerminalResult

  @rpc
  def `terminal/output`(sessionId: Text, terminalId: Text): Acp.TerminalOutputResult

  @rpc
  def `terminal/wait_for_exit`(sessionId: Text, terminalId: Text): Acp.TerminalExitStatus

  @rpc
  def `terminal/kill`(sessionId: Text, terminalId: Text): Json

  @rpc
  def `terminal/release`(sessionId: Text, terminalId: Text): Json

// The whole client surface: what `Service` implements and the exchange's reader dispatches.
trait AcpClient extends AcpClientSession, AcpClientFs, AcpClientTerminal
