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
┃    Soundness, version 0.53.0.                                                                    ┃
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
package synesthesia

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import distillate.*
import eucalyptus.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import inimitable.*
import jacinta.*
import obligatory.*
import parasite.*
import prepositional.*
import proscenium.*
import revolution.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

import scala.annotation.*
import scala.quoted.*

import errorDiagnostics.stackTraces

object Mcp:
  val version = t"2025-11-25"
  type Cursor = Text

  def send[interface <: McpServer](id: Text, server: interface, mcpInterface: Interface)
    ( dispatch: Json => Optional[Json] )
    ( using request: Http.Request )
    ( using Monitor, Codicil, Online )
  : Http.Response =
      import jsonPrinters.minimal
      import charEncoders.utf8
      import charDecoders.utf8
      import textSanitizers.skip

      given mcpSessionId: ("mcpSessionId" is Directive of Text) = identity(_)
      given mcpProtocolVersion: ("mcpProtocolVersion" is Directive of Text) = identity(_)
      given lastEventId: ("lastEventId" is Directive of Text) = identity(_)

      recover:
        case error: ParseError =>
          Http.Response(Http.Ok):
            JsonRpc.error(-32700, t"Parse error: ${error.message}".show).json
          . mcpSessionId = id

        case error: JsonError =>
          Http.Response(Http.Ok):
            JsonRpc.error(-32600, t"Invalid request: ${error.message}".show).json
          . mcpSessionId = id

      . within:
          try
            request.method match
              case Http.Options =>
                Http.Response
                  ( Http.NoContent,
                    mcpProtocolVersion = version,
                    mcpSessionId       = id )
                  (  )

              case Http.Delete =>
                Http.Response
                  ( Http.Accepted,
                    mcpProtocolVersion = version,
                    mcpSessionId       = id )
                  (  )

              case Http.Get =>
                Http.Response
                  ( Http.Ok,
                    connection         = t"keep-alive",
                    cacheControl       = t"no-cache",
                    mcpProtocolVersion = version,
                    mcpSessionId       = id )
                  ( mcpInterface.stream )

              case Http.Post =>
                val input = request.body().read[Json]
                dispatch(input).let: json =>
                  import jsonPrinters.indented
                  Http.Response
                    ( Http.Ok,
                      mcpProtocolVersion = version,
                      mcpSessionId       = id )
                    ( json )

                . or:
                    Http.Response
                      ( Http.Accepted,
                        mcpProtocolVersion = version,
                        mcpSessionId       = id )
                      (  )
              case method =>
                ???
          catch
            case error: Throwable =>
              Http.Response(Http.Ok):
                JsonRpc.error(-32603, t"Internal error: ${error.toString}".show).json

  case class TaskAugmented(task: Optional[TaskMetadata] = Unset)
  case class Error(code: Int, message: Text, data: Optional[Json] = Unset)

  object TextInt:
    given encodable: TextInt is Encodable in Json = _.id.absolve match
      case text: Text => text.json
      case int: Int   => int.json

    given decodable: Tactic[JsonError] => TextInt is Decodable in Json =
      json => TextInt(safely(json.as[Int]).or(json.as[Text]))

  case class TextInt(id: Text | Int)

  val ParseError = -32700
  val InvalidRequest = -32600
  val MethodNotFound = -32601
  val InvalidParams = -32602
  val InternalError = -32603
  val UrlElicitationRequired = -32042

  case class TaskMetadata(ttl: Optional[Int] = Unset)
  case class RelatedTaskMetadata(taskId: Text)
  case class Icon
    ( src:      Text,
      mimeType: Optional[Text]       = Unset,
      sizes:    Optional[List[Text]] = Unset,
      theme:    Optional[Text]       = Unset )

  case class BaseMetadata(name: Text, title: Optional[Text] = Unset)

  case class Implementation
    ( name:        Text,
      title:       Optional[Text]       = Unset,
      icons:       Optional[List[Icon]] = Unset,
      version:     Text,
      description: Optional[Text]       = Unset,
      websiteUrl:  Optional[Text]       = Unset)

  case class ClientInfo
    ( name:        Text,
      title:       Text,
      version:     Text,
      description: Text,
      icons:       Optional[List[Icon]] = Unset)

  case class ServerInfo
    ( name:        Text,
      title:       Text,
      version:     Text,
      description: Text,
      icons:       Optional[List[Icon]] = Unset,
      websiteUrl:  HttpUrl )

  case class Initialize
    ( protocolVersion: Text,
      capabilities:    ServerCapabilities,
      serverInfo:      Implementation,
      instructions:    Optional[Text] = Unset )

  case class ListChanged(listChanged: Optional[Boolean] = Unset)
  case class Resources(subscribe: Optional[Boolean] = true, listChanged: Optional[Boolean] = true)

  case class Sampling(context: Optional[Json] = Unset, tools: Optional[Json] = Unset)
  case class Elicitation(form: Optional[Json] = Unset, url: Optional[Json] = Unset)
  case class RequestsElicitation(create: Optional[Json] = Unset)
  case class Call(call: Optional[Json] = Unset)
  case class RequestsSampling(createMessage: Optional[Json] = Unset)

  case class Tasks
    ( list:     Optional[Json]     = Unset,
      cancel:   Optional[Json]     = Unset,
      requests: Optional[Requests] = Unset )

  case class ListTasks(nextCursor: Optional[Cursor] = Unset, tasks: List[Task] = Nil)

  case class Requests
    ( sampling:    Optional[RequestsSampling]    = Unset,
      elicitation: Optional[RequestsElicitation] = Unset,
      tools:       Optional[Call]                = Unset )

  case class ServerCapabilities
    ( experimental: Optional[Map[Text, Json]] = Unset,
      logging:      Optional[Json]            = Unset,
      completions:  Optional[Json]            = Unset,
      prompts:      Optional[ListChanged]     = ListChanged(true),
      resources:    Optional[Resources]       = Resources(),
      tools:        ListChanged               = ListChanged(true),
      tasks:        Optional[Tasks]           = Unset )

  case class ClientCapabilities
    ( experimental: Optional[Map[Text, Json]] = Unset,
      roots:        Optional[ListChanged]     = Unset,
      sampling:     Optional[Sampling]        = Unset,
      elicitation:  Optional[Elicitation]     = Unset,
      tasks:        Optional[Tasks]           = Unset )

  object Contents:
    given encodable: Contents is Encodable in Json = _.contents match
      case text: TextResourceContents => text.json
      case blob: BlobResourceContents => blob.json

    given decodable: Tactic[JsonError] => Contents is Decodable in Json = json =>
      Contents(safely(json.as[TextResourceContents]).or(json.as[BlobResourceContents]))

  case class Contents(contents: TextResourceContents | BlobResourceContents)
  case class Context(arguments: Optional[Map[Text, Text]] = Unset)


  case class ListResources(nextCursor: Optional[Cursor] = Unset, resources: List[Resource] = Nil)
  case class ListResourceTemplates(nextCursor: Optional[Cursor] = Unset, resourceTemplates: List[ResourceTemplate] = Nil)
  case class ReadResource(contents: List[Contents] = Nil)

  case class Resource
    ( name:        Text,
      uri:         Text,
      title:       Optional[Text]       = Unset,
      description: Optional[Text]       = Unset,
      icons:       Optional[List[Icon]] = Unset,
      mimeType:    Optional[Text]       = Unset,
      annotations: Annotations          = Annotations(),
      size:        Optional[Long]       = Unset,
      _meta:       Optional[Json]       = Unset )

  case class ResourceTemplate
    ( name:        Text,
      title:       Optional[Text]       = Unset,
      icons:       Optional[List[Icon]] = Unset,
      uriTemplate: Text,
      description: Optional[Text]       = Unset,
      mimeType:    Optional[Text]       = Unset,
      annotations: Annotations          = Annotations(),
      _meta:       Optional[Json]       = Unset )

  case class ResourceContents(uri: Text, mimeType: Optional[Text] = Unset, _meta: Optional[Json] = Unset)
  case class TextResourceContents(uri: Text, mimeType: Optional[Text] = Unset, text: Text, _meta: Optional[Json] = Unset)
  case class BlobResourceContents(uri: Text, mimeType: Optional[Text] = Unset, blob: Text, _meta: Optional[Json] = Unset)
  case class ListPrompts(nextCursor: Optional[Cursor] = Unset, prompts: List[Prompt] = Nil)

  case class Annotations
    ( audience:     Optional[List[Role]] =  Unset,
      priority:     Optional[Double]     = Unset,
      lastModified: Optional[Text]       = Unset )

  case class Complete(completion: Completion)
  case class Completion(values: List[Text] = Nil, total: Optional[Int] = Unset, hasMore: Optional[Boolean] = Unset)

  object Role:
    given encodable: Role is Encodable in Json =
      case Role.User      => t"user".json
      case Role.Assistant => t"assistant".json

    given decodable: Tactic[JsonError] => Role is Decodable in Json = _.as[Text] match
      case t"user"      => Role.User
      case t"assistant" => Role.Assistant
      case _            => abort(JsonError(JsonError.Reason.OutOfRange))

  enum Role:
    case User, Assistant

  object TaskSupport:
    given encodable: TaskSupport is Encodable in Json =
      case TaskSupport.Forbidden => t"forbidden".json
      case TaskSupport.Optional  => t"optional".json
      case TaskSupport.Required  => t"required".json

    given decodable: Tactic[JsonError] => TaskSupport is Decodable in Json = _.as[Text] match
      case t"forbidden" => TaskSupport.Forbidden
      case t"optional"  => TaskSupport.Optional
      case t"required"  => TaskSupport.Required
      case _            => abort(JsonError(JsonError.Reason.OutOfRange))

  enum TaskSupport:
    case Forbidden, Optional, Required

  object LoggingLevel:
    given encodable: LoggingLevel is Encodable in Json = _.toString.tt.lower.json

    given decodable: Tactic[JsonError] => LoggingLevel is Decodable in Json = _.as[Text] match
      case t"debug"     => LoggingLevel.Debug
      case t"info"      => LoggingLevel.Info
      case t"notice"    => LoggingLevel.Notice
      case t"warning"   => LoggingLevel.Warning
      case t"error"     => LoggingLevel.Error
      case t"critical"  => LoggingLevel.Critical
      case t"alert"     => LoggingLevel.Alert
      case t"emergency" => LoggingLevel.Emergency
      case _            => abort(JsonError(JsonError.Reason.OutOfRange))

  enum LoggingLevel:
    case Debug, Info, Notice, Warning, Error, Critical, Alert, Emergency

  case class LoggingMessage(level: LoggingLevel, logger: Optional[Text] = Unset, data: Json)

  object TaskStatus:
    given encodable: TaskStatus is Encodable in Json =
      case TaskStatus.Working       => t"working".json
      case TaskStatus.InputRequired => t"input_required".json
      case TaskStatus.Completed     => t"completed".json
      case TaskStatus.Failed        => t"failed".json
      case TaskStatus.Cancelled     => t"cancelled".json

    given decodable: Tactic[JsonError] => TaskStatus is Decodable in Json = _.as[Text] match
      case t"working"        => TaskStatus.Working
      case t"input_required" => TaskStatus.InputRequired
      case t"completed"      => TaskStatus.Completed
      case t"failed"         => TaskStatus.Failed
      case t"cancelled"      => TaskStatus.Cancelled
      case _                 => abort(JsonError(JsonError.Reason.OutOfRange))

  enum TaskStatus:
    case Working, InputRequired, Completed, Failed, Cancelled

  case class PromptArgument
    ( name:        Text,
      title:       Optional[Text]    = Unset,
      description: Optional[Text]    = Unset,
      required:    Optional[Boolean] = Unset )

  case class Prompt
    ( name:        Text,
      title:       Optional[Text]                 = Unset,
      icons:       Optional[List[Icon]]           = Unset,
      description: Optional[Text]                 = Unset,
      arguments:   Optional[List[PromptArgument]] = Unset,
      _meta:       Optional[Json]                 = Unset )
  case class Argument(name: Text, value: Text)
  object Reference:
    import dynamicJsonAccess.enabled

    given encodable: Reference is Encodable in Json =
      case ref: PromptReference            => unsafely(ref.json.`type` = "ref/prompt")
      case ref: ResourceTemplateReference  => unsafely(ref.json.`type` = "ref/resource")

    given decodable: Tactic[JsonError] => Reference is Decodable in Json = json =>
      json.`type`.as[Text] match
        case "ref/prompt"   => json.as[PromptReference]
        case "ref/resource" => json.as[ResourceTemplateReference]
        case _              => abort(JsonError(JsonError.Reason.OutOfRange))

  sealed trait Reference
  case class PromptReference(name: Text, title: Optional[Text] = Unset) extends Reference
  case class ResourceTemplateReference(uri: Text) extends Reference

  case class PromptMessage(role: Role, content: ContentBlock)

  case class GetPrompt(description: Optional[Text] = Unset, messages: List[PromptMessage] = Nil)

  case class CallTool
    ( content:           List[ContentBlock] = Nil,
      structuredContent: Optional[Json]     = Unset,
      isError:           Optional[Boolean]  = Unset )

  case class ListTools(nextCursor: Optional[Cursor] = Unset, tools: List[Tool] = Nil)
  case class ListRoots(roots: List[Root] = Nil)

  case class Root(uri: Text, name: Optional[Text] = Unset, _meta: Optional[Json] = Unset)

  case class Tool
    ( name:         Text,
      inputSchema:  JsonSchema,
      outputSchema: Optional[JsonSchema]       = Unset,
      title:        Optional[Text]            = Unset,
      icons:        Optional[List[Icon]]      = Unset,
      description:  Optional[Text]            = Unset,
      execution:    Optional[ToolExecution]   = Unset,
      annotations:  Optional[ToolAnnotations] = Unset,
      _meta:        Optional[Json]            = Unset )

  case class ToolExecution(taskSupport: Optional[TaskSupport] = Unset)

  case class ToolAnnotations
    ( title:           Optional[Text]    = Unset,
      readOnlyHint:    Optional[Boolean] = Unset,
      destructiveHint: Optional[Boolean] = Unset,
      idempotentHint:  Optional[Boolean] = Unset,
      openWorldHint:   Optional[Boolean] = Unset )

  case class ToolChoice(mode: Optional[Mode] = Unset)

  object Mode:
    given encodable: Mode is Encodable in Json =
      case Mode.Auto     => t"auto".json
      case Mode.Required => t"required".json
      case Mode.None     => t"none".json

    given decodable: Tactic[JsonError] => Mode is Decodable in Json = _.as[Text] match
      case t"auto"     => Mode.Auto
      case t"required" => Mode.Required
      case t"none"     => Mode.None
      case _           => abort(JsonError(JsonError.Reason.OutOfRange))

  enum Mode:
    case Auto, Required, None

  case class SamplingMessage(role: Role, content: List[SamplingMessageContentBlock], _meta: Optional[Json] = Unset)

  object SamplingMessageContentBlock:
    import dynamicJsonAccess.enabled

    given encodable: SamplingMessageContentBlock is Encodable in Json =
      case content: TextContent       => unsafely(content.json.`type` = "text")
      case content: ImageContent      => unsafely(content.json.`type` = "image")
      case content: AudioContent      => unsafely(content.json.`type` = "audio")
      case content: ToolUseContent    => unsafely(content.json.`type` = "tool_use")
      case content: ToolResultContent => unsafely(content.json.`type` = "tool_result")

    given decodable: Tactic[JsonError] => SamplingMessageContentBlock is Decodable in Json = json =>
      json.`type`.as[Text] match
        case "text"        => json.as[TextContent]
        case "image"       => json.as[ImageContent]
        case "audio"       => json.as[AudioContent]
        case "tool_use"    => json.as[ToolUseContent]
        case "tool_result" => json.as[ToolResultContent]
        case _             => abort(JsonError(JsonError.Reason.OutOfRange))

  sealed trait SamplingMessageContentBlock

  case class ModelPreferences
    ( hints:                Optional[List[ModelHint]] = Unset,
      costPriority:         Optional[Double]          = Unset,
      speedPriority:        Optional[Double]          = Unset,
      intelligencePriority: Optional[Double]          = Unset)

  case class ModelHint(name: Optional[Text] = Unset)

  object ContentBlock:
    import dynamicJsonAccess.enabled

    given encodable: ContentBlock is Encodable in Json =
      case content: TextContent      => unsafely(content.json.`type` = "text")
      case content: ImageContent     => unsafely(content.json.`type` = "image")
      case content: AudioContent     => unsafely(content.json.`type` = "audio")
      case content: ResourceLink     => unsafely(content.json.`type` = "resource_link")
      case content: EmbeddedResource => unsafely(content.json.`type` = "resource")

    given decodable: Tactic[JsonError] => ContentBlock is Decodable in Json = json =>
      json.`type`.as[Text] match
        case "text"          => json.as[TextContent]
        case "image"         => json.as[ImageContent]
        case "audio"         => json.as[AudioContent]
        case "resource_link" => json.as[ResourceLink]
        case "resource"      => json.as[EmbeddedResource]
        case _               => abort(JsonError(JsonError.Reason.OutOfRange))

  sealed trait ContentBlock
  case class TextContent(text: Text, annotations: Optional[Annotations] = Unset)
  extends ContentBlock, SamplingMessageContentBlock

  case class ImageContent(data: Text, mimeType: Text, annotations: Optional[Annotations] = Unset)
  extends ContentBlock, SamplingMessageContentBlock

  case class AudioContent(data: Text, mimeType: Text, annotations: Optional[Annotations] = Unset)
  extends ContentBlock, SamplingMessageContentBlock

  case class ResourceLink
    ( name:        Text,
      uri:         Text,
      title:       Optional[Text]       = Unset,
      description: Optional[Text]       = Unset,
      icons:       Optional[List[Icon]] = Unset,
      mimeType:    Optional[Text]       = Unset,
      annotations: Annotations          = Annotations(),
      size:        Optional[Long]       = Unset,
      _meta:       Optional[Json]       = Unset )
  extends ContentBlock
  case class EmbeddedResource
    ( resource:    Contents,
      annotations: Optional[Annotations] = Unset,
      _meta:       Optional[Json]        = Unset )
  extends ContentBlock

  case class ToolUseContent(id: Text, name: Text, input: Json)
  extends SamplingMessageContentBlock

  case class ToolResultContent
    ( toolUseId:         Text,
      content:           List[ContentBlock]  = Nil,
      structuredContent: Optional[Json]      = Unset,
      isError:           Optional[Boolean]   = Unset )
  extends SamplingMessageContentBlock

  case class CreateMessage
    ( role:       Role,
      content:    List[SamplingMessageContentBlock],
      model:      Text,
      stopReason: Optional[Text] = Unset )

  case class Task
    ( taskId:        Text,
      status:        TaskStatus,
      statusMessage: Optional[Text] = Unset,
      createdAt:     Text,
      lastUpdatedAt: Text,
      ttl:           Optional[Int]  = Unset,
      pollInterval:  Optional[Int]  = Unset )

  case class CreateTaskResult(task: Task)

  object ElicitAction:
    given encodable: ElicitAction is Encodable in Json =
      case ElicitAction.Accept  => t"accept".json
      case ElicitAction.Decline => t"decline".json
      case ElicitAction.Cancel  => t"cancel".json

    given decodable: Tactic[JsonError] => ElicitAction is Decodable in Json = _.as[Text] match
      case t"accept"  => ElicitAction.Accept
      case t"decline" => ElicitAction.Decline
      case t"cancel"  => ElicitAction.Cancel
      case _          => abort(JsonError(JsonError.Reason.OutOfRange))

  enum ElicitAction:
    case Accept, Decline, Cancel

  case class ElicitResult(action: ElicitAction, content: Optional[Json] = Unset)

  sealed trait ElicitRequest

  case class ElicitRequestForm
    ( message:         Text,
      requestedSchema: Json,
      mode:            Optional[Text] = Unset )
  extends ElicitRequest

  case class ElicitRequestUrl
    ( mode:          Text,
      message:       Text,
      elicitationId: Text,
      url:           Text )
  extends ElicitRequest

  case class ElicitationComplete(elicitationId: Text)

  trait Api extends JsonRpc:

    type Origin = McpClient

    @rpc
    def ping(): Unit

    @rpc
    def initialize
      ( protocolVersion: Text,
        capabilities:    ClientCapabilities,
        clientInfo:      Implementation,
        _meta:           Optional[Json])
    : Initialize

    @rpc
    def `completion/complete`
      ( ref:      Reference,
        argument: Argument,
        context:  Optional[Context],
        _meta:    Optional[Json])
    : Complete

    @rpc
    def `logging/setLevel`(level: LoggingLevel, _meta: Optional[Json]): Unit

    @rpc
    def `prompts/get`(name: Text, arguments: Optional[Map[Text, Text]], _meta: Optional[Json])
    : GetPrompt

    @rpc
    def `prompts/list`(cursor: Optional[Cursor], _meta: Optional[Json]): ListPrompts

    @rpc
    def `resources/list`(cursor: Optional[Cursor], _meta: Optional[Json]): ListResources

    @rpc
    def `resources/templates/list`(cursor: Optional[Cursor], _meta: Optional[Json]): ListResourceTemplates

    @rpc
    def `resources/read`(uri: Text, _meta: Optional[Json]): ReadResource

    @rpc
    def `resources/subscribe`(uri: Text, _meta: Optional[Json]): Unit

    @rpc
    def `resources/unsubscribe`(uri: Text, _meta: Optional[Json]): Unit

    @rpc
    def `tools/call`(name: Text, arguments: Json, _meta: Optional[Json]): CallTool

    @rpc
    def `tools/list`(_meta: Optional[Json]): ListTools

    @rpc
    def `tasks/get`(taskId: Text, _meta: Optional[Json]): Task

    @rpc
    def `tasks/result`(taskId: Text, _meta: Optional[Json]): Map[Text, Json]

    @rpc
    def `tasks/list`(_meta: Optional[Json]): ListTasks

    @rpc
    def `tasks/cancel`(taskId: Text, _meta: Optional[Json]): Task

    @rpc
    def `notifications/cancelled`
      ( requestId: Optional[TextInt],
        reason:    Optional[Text],
        _meta:     Optional[Json] ): Unit

    @rpc
    def `notifications/progress`
      ( progressToken: TextInt,
        progress:      Double,
        total:         Optional[Double],
        message:       Optional[Text],
        _meta:         Optional[Json] )
    : Unit

    @rpc
    def `notifications/initialized`(_meta: Optional[Json]): Unit

    @rpc
    def `notifications/roots/list_changed`(_meta: Optional[Json]): Unit

    @rpc
    def `notifications/tasks/status`
      ( taskId:        Text,
        status:        TaskStatus,
        statusMessage: Optional[Text],
        createdAt:     Text,
        lastUpdatedAt: Text,
        ttl:           Optional[Int],
        pollInterval:  Optional[Int],
        _meta:         Optional[Json] )
    : Unit

    @rpc
    def `notifications/resources/list_changed`(_meta: Optional[Json]): Unit

    @rpc
    def `notifications/resources/updated`(uri: Text, _meta: Optional[Json]): Unit

    @rpc
    def `notifications/prompts/list_changed`(_meta: Optional[Json]): Unit

    @rpc
    def `notifications/tools/list_changed`(_meta: Optional[Json]): Unit

    @rpc
    def `notifications/message`(level: LoggingLevel, logger: Optional[Text], data: Json, _meta: Optional[Json]): Unit

  object Interface:

    private val cache: scm.HashMap[Text, Interface] = scm.HashMap()

    given streamable: Interface is Streamable by Sse = _.stream

    inline def apply(sessionId: Text, server: McpServer from McpClient)
      ( using spec: server.type is McpSpecification )
    : Interface =

        cache.establish(sessionId):
          new Interface(sessionId, server, spec)


  class Interface
    (     sessionId: Text,
      val server:    McpServer from McpClient,
      val spec:      server.type is McpSpecification )
  extends Api:

    protected var loggingLevel: LoggingLevel = LoggingLevel.Info

    def ping(): Unit = ()

    def initialize
      ( protocolVersion: Text,
        capabilities:    ClientCapabilities,
        clientInfo:      Implementation,
        _meta:           Optional[Json] )
    : Initialize =

        Initialize
          (version,
            ServerCapabilities(),
            Implementation(server.name, version = server.version.encode),
            server.description )

    def `completion/complete`
      ( ref:      Reference,
        argument: Argument,
        context:  Optional[Context],
        _meta:    Optional[Json] )
    : Complete =

        ???


    def `logging/setLevel`(level: LoggingLevel, _meta: Optional[Json]): Unit =
      loggingLevel = level

    def `prompts/get`(name: Text, arguments: Optional[Map[Text, Text]], _meta: Optional[Json])
    : GetPrompt =

        val messages = spec.invokePrompt(server, client, name, arguments.or(Map())).map:
          case Human(message) => PromptMessage(Role.User, TextContent(message))
          case Agent(message) => PromptMessage(Role.Assistant, TextContent(message))

        GetPrompt(Unset, messages)

    def `prompts/list`(cursor: Optional[Cursor], _meta: Optional[Json]): ListPrompts =
      ListPrompts(prompts = spec.prompts())

    def `resources/list`(cursor: Optional[Cursor], _meta: Optional[Json]): ListResources =
      ListResources(resources = spec.resources())

    def `resources/templates/list`(cursor: Optional[Cursor], _meta: Optional[Json]): ListResourceTemplates = ???

    def `resources/read`(uri: Text, _meta: Optional[Json]): ReadResource =
      ReadResource(List(spec.invokeResource(server, uri)))

    def `resources/subscribe`(uri: Text, _meta: Optional[Json]): Unit = ???

    def `resources/unsubscribe`(uri: Text, _meta: Optional[Json]): Unit = ???

    def `tools/call`(name: Text, arguments: Json, _meta: Optional[Json]): CallTool = unsafely:
      val result = spec.invokeTool(server, client, name, arguments)

      import jsonPrinters.minimal
      CallTool(content = List(TextContent(result.show)), structuredContent = result)

    def `tools/list`(_meta: Optional[Json]): ListTools =
      ListTools(tools = spec.tools())
      . tap: tools =>
        import jsonPrinters.indented

    def `tasks/get`(taskId: Text, _meta: Optional[Json]): Task = ???

    def `tasks/result`(taskId: Text, _meta: Optional[Json]): Map[Text, Json] = ???

    def `tasks/list`(_meta: Optional[Json]): ListTasks = ListTasks(tasks = Nil)

    def `notifications/cancelled`
      ( requestId: Optional[TextInt], reason: Optional[Text], _meta: Optional[Json] )
    : Unit =

        ()


    def `notifications/progress`
      ( progressToken: TextInt,
        progress:      Double,
        total:         Optional[Double],
        message:       Optional[Text],
        _meta:         Optional[Json] )
    : Unit =

        ()


    def `notifications/initialized`(_meta: Optional[Json]): Unit = ()

    def `notifications/resources/list_changed`(_meta: Optional[Json]): Unit = ???

    def `notifications/resources/updated`(uri: Text, _meta: Optional[Json]): Unit = ???

    def `tasks/cancel`(taskId: Text, _meta: Optional[Json]): Task = ???

    def `notifications/roots/list_changed`(_meta: Optional[Json]): Unit = ???

    def `notifications/tasks/status`
      ( taskId:        Text,
        status:        TaskStatus,
        statusMessage: Optional[Text],
        createdAt:     Text,
        lastUpdatedAt: Text,
        ttl:           Optional[Int],
        pollInterval:  Optional[Int],
        _meta:         Optional[Json] )
    : Unit = ???

    def `notifications/prompts/list_changed`(_meta: Optional[Json]): Unit = ???

    def `notifications/tools/list_changed`(_meta: Optional[Json]): Unit = ???

    def `notifications/message`(level: LoggingLevel, logger: Optional[Text], data: Json, _meta: Optional[Json]): Unit = ???
