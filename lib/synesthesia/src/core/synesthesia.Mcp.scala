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
      given lastEventId: ("lastEventId" is Directive of Text) = identity(_)

      println(s"Session ID: $id")

      recover:
        case error: ParseError =>
          println("parse error")
          Http.Response(Http.Ok):
            JsonRpc.error(-32700, t"Parse error: ${error.message}".show).json
          . mcpSessionId = id

        case error: JsonError =>
          println("json error")
          Http.Response(Http.Ok):
            JsonRpc.error(-32600, t"Invalid request: ${error.message}".show).json
          . mcpSessionId = id

      . within:
          try
            request.method match
              case Http.Options =>
                println("OPTIONS")
                Http.Response(Http.NoContent)()

              case Http.Delete =>
                println("DELETE")
                Http.Response(Http.Accepted)()

              case Http.Get =>
                println("GET")
                println(s"Recovery from ${request.headers.lastEventId.prim}")
                Http.Response(Http.Ok, connection = t"keep-alive", cacheControl = t"no-cache")
                  ( mcpInterface.stream )
                . mcpSessionId = id

              case Http.Post =>
                println("POST")
                val input = request.body().read[Json]
                dispatch(input).let: json =>
                  println("dispatch")
                  import jsonPrinters.indented
                  println(json.show)
                  Http.Response(Http.Ok)(json).mcpSessionId = id

                . or:
                    Http.Response(Http.Accepted)().mcpSessionId = id
              case method =>
                println(s"Received HTTP request with method $method")
                ???
          catch
            case error: Throwable =>
              println(error.getMessage)
              error.printStackTrace()
              Http.Response(Http.Ok):
                JsonRpc.error(-32603, t"Internal error: ${error.toString}".show).json

  case class TaskAugmentedRequestParams(task: Optional[TaskMetadata])
  case class Error(code: Int, message: Text, data: Optional[Json])

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

  case class TaskMetadata(ttl: Optional[Int])
  case class Icon(src: Text, mimeType: Text, sizes: List[Text], theme: Optional[Text])
  case class BaseMetadata(name: Text, title: Optional[Text])

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
      icons:       List[Icon] )

  case class ServerInfo
    ( name:        Text,
      title:       Text,
      version:     Text,
      description: Text,
      icons:       List[Icon],
      websiteUrl:  HttpUrl )

  case class Initialize
    ( protocolVersion: Text,
      capabilities:    ServerCapabilities,
      serverInfo:      Implementation,
      instructions:    Optional[Text] )

  case class ListChanged(listChanged: Optional[Boolean])
  case class Resources(subscribe: Optional[Boolean], listChanged: Optional[Boolean])

  case class Sampling(context: Optional[Json], tools: Optional[Json])
  case class Elicitation(form: Optional[Json], url: Optional[Json])
  case class RequestsElicitation(create: Optional[Json])
  case class Call(call: Optional[Json])
  case class RequestsSampling(createMessage: Optional[Json])

  case class Tasks
    ( list:     Optional[Json]     = Unset,
      cancel:   Optional[Json]     = Unset,
      requests: Optional[Requests] = Unset )

  case class ListTasks(tasks: List[Task])

  case class Requests
    ( sampling:    Optional[RequestsSampling]    = Unset,
      elicitation: Optional[RequestsElicitation] = Unset,
      tools:       Optional[Call]                = Unset )

  case class ServerCapabilities
    ( experimental: Optional[Map[Text, Json]] = Unset,
      logging:      Optional[Json]            = Unset,
      completions:  Optional[Json]            = Unset,
      prompts:      Optional[ListChanged]     = Unset,
      resources:    Optional[Resources]       = Unset,
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
  case class Context(arguments: Optional[Map[Text, Text]])


  case class ListResources(resources: List[Resource])
  case class ListResourceTemplates(resourceTemplates: List[ResourceTemplate])
  case class ReadResource(contents: List[Contents])
  case class Resource
    ( name:        Text,
      title:       Optional[Text] = Unset,
      icons:       List[Icon]     = Nil,
      description: Optional[Text] = Unset,
      mimeType:    Optional[Text] = Unset,
      annotations: Annotations,
      size:        Long )

  case class ResourceTemplate
    ( name:        Text,
      title:       Optional[Text],
      icons:       List[Icon],
      uriTemplate: Text,
      description: Optional[Text],
      mimeType:    Optional[Text],
      annotations: Annotations )

  case class ResourceContents(uri: Text, mimeType: Optional[Text])
  case class TextResourceContents(uri: Text, mimeType: Optional[Text], text: Text)
  case class BlobResourceContents(uri: Text, mimeType: Optional[Text], blob: Text)
  case class ListPrompts(cursor: Optional[Cursor], prompts: List[Prompt])

  case class Annotations
    ( audience:     Optional[List[Role]],
      priority:     Optional[Int],
      lastModified: Optional[Text] )

  case class Complete(completion: Completion)
  case class Completion(values: List[Text], total: Optional[Int], hasMore: Optional[Boolean])

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

  object TaskStatus:
    given encodable: TaskStatus is Encodable in Json = _.toString.tt.lower.json

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
      title:       Optional[Text],
      icons:       List[Icon],
      description: Optional[Text] )

  case class Prompt
    ( name:        Text,
      title:       Optional[Text],
      icons:       List[Icon],
      description: Optional[Text],
      arguments:   List[PromptArgument] )
  case class Argument(name: Text, value: Text)
  case class Reference(name: Text, title: Optional[Text], `type`: Text, uri: Optional[Text])

  case class CallTool
    ( content:           List[ContentBlock] = Nil,
      structuredContent: Optional[Json]     = Unset,
      isError:           Optional[Boolean]  = Unset )

  case class ListTools(tools: List[Tool])
  case class ListRoots(tools: List[Root])

  case class Root(uri: Text, name: Optional[Text])

  case class Tool
    ( name:         Text,
      inputSchema:  JsonSchema,
      outputSchema: Optional[JsonSchema],
      title:        Optional[Text]            = Unset,
      icons:        Optional[List[Icon]]      = Unset,
      description:  Optional[Text]            = Unset,
      execution:    Optional[ToolExecution]   = Unset,
      annotations:  Optional[ToolAnnotations] = Unset )

  case class ToolExecution(taskSupport: Optional[TaskSupport])

  case class ToolAnnotations
    ( title:           Optional[Text],
      readOnlyHint:    Optional[Boolean],
      destructiveHint: Optional[Boolean],
      idempotentHint:  Optional[Boolean],
      openWorldHint:   Optional[Boolean] )

  case class ToolChoice(mode: Mode)

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

  case class SamplingMessage(role: Role, content: List[SamplingMessageContentBlock])

  case class SamplingMessageContentBlock(text: Text) // FIXME

  case class ModelPreferences
    ( hints:                Optional[List[ModelHint]] = Unset,
      costPriority:         Optional[Double]          = Unset,
      speedPriority:        Optional[Double]          = Unset,
      intelligencePriority: Optional[Double]          = Unset)

  case class ModelHint(name: Optional[Text])

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
        //case "resource"      => json.as[EmbeddedResource]
        case _               => abort(JsonError(JsonError.Reason.OutOfRange))

  sealed trait ContentBlock
  case class TextContent(text: Text, annotations: Optional[Annotations] = Unset)
  extends ContentBlock

  case class ImageContent(data: Text, mimeType: Text, annotations: Optional[Annotations] = Unset)
  extends ContentBlock

  case class AudioContent(data: Text, mimeType: Text, annotations: Optional[Annotations] = Unset)
  extends ContentBlock

  case class ResourceLink(uri: Text) extends ContentBlock
  case class EmbeddedResource(resource: Resource) extends ContentBlock

  case class CreateMessage
    ( role:       Role,
      content:    List[SamplingMessageContentBlock],
      model:      Text,
      stopReason: Optional[Text] )

  case class Task
    ( taskId:        Text,
      status:        TaskStatus,
      statusMessage: Optional[Text],
      createdAt:     Text,
      lastUpdatedAt: Text,
      ttl:           Int,
      pollInterval:  Optional[Int] )

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
    def `prompts/get`(name: Text, arguments: Optional[Map[Text, Text]], _meta: Optional[Json]): Unit

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
      ( request: Optional[TextInt],
        reason:  Optional[Text],
        _meta:   Optional[Json] ): Unit

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
        ttl:           Int,
        pollInterval:  Optional[Int],
        _meta:         Optional[Json] )
    : Unit

    @rpc
    def `notifications/resources/list_changed`(_meta: Optional[Json]): Unit

    @rpc
    def `notifications/resources/updated`(uri: Text, _meta: Optional[Json]): Unit

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
  extends Mcp.Api:

    println(s"Initializing MCP Interface (${this.toString})")

    protected var loggingLevel: LoggingLevel = LoggingLevel.Info

    def ping(): Unit =
      println("Received ping")

    def initialize
      ( protocolVersion: Text,
        capabilities:    ClientCapabilities,
        clientInfo:      Implementation,
        _meta:           Optional[Json] )
    : Mcp.Initialize =

        Mcp.Initialize
          ( "2025-11-25",
            ServerCapabilities(),
            Implementation(server.name, version = server.version.encode),
            server.description )
        . tap(println(_))

    def `completion/complete`
      ( ref:      Reference,
        argument: Argument,
        context:  Optional[Context],
        _meta:    Optional[Json] )
    : Complete =

        ???


    def `logging/setLevel`(level: LoggingLevel, _meta: Optional[Json]): Unit =
      loggingLevel = level

    def `prompts/get`(name: Text, arguments: Optional[Map[Text, Text]], _meta: Optional[Json]): Unit =
      ???

    def `prompts/list`(cursor: Optional[Cursor], _meta: Optional[Json]): ListPrompts =
      ListPrompts(Unset, server.prompts)

    def `resources/list`(cursor: Optional[Cursor], _meta: Optional[Json]): ListResources = ListResources(Nil)

    def `resources/templates/list`(cursor: Optional[Cursor], _meta: Optional[Json]): ListResourceTemplates = ???

    def `resources/read`(uri: Text, _meta: Optional[Json]): ReadResource = ???

    def `resources/subscribe`(uri: Text, _meta: Optional[Json]): Unit = ???

    def `resources/unsubscribe`(uri: Text, _meta: Optional[Json]): Unit = ???

    def `tools/call`(name: Text, arguments: Json, _meta: Optional[Json]): CallTool = unsafely:
      val result = spec.invoke(server, client, name, arguments)

      import jsonPrinters.minimal
      CallTool(content = List(TextContent(result.show)), structuredContent = result)

    def `tools/list`(_meta: Optional[Json]): ListTools =
      ListTools(spec.tools())
      . tap: tools =>
        import jsonPrinters.indented
        println(tools.json.show)

    def `tasks/get`(taskId: Text, _meta: Optional[Json]): Task = ???

    def `tasks/result`(taskId: Text, _meta: Optional[Json]): Map[Text, Json] = ???

    def `tasks/list`(_meta: Optional[Json]): ListTasks = ListTasks(Nil)

    def `notifications/cancelled`
      ( request: Optional[TextInt], reason: Optional[Text], _meta: Optional[Json] )
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


    def `notifications/initialized`(_meta: Optional[Json]): Unit =
      println("Notifications initialized")

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
        ttl:           Int,
        pollInterval:  Optional[Int],
        _meta:         Optional[Json] )
    : Unit = ???
