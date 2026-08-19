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

import scala.caps
import scala.collection.mutable as scm

import soundness.*

import Json.jsonEncodableInText
import errorDiagnostics.stackTracesDiagnostics
import probates.awaitProbate
import strategies.throwUnsafely
import threading.virtualThreading
import workingDirectories.defaultWorkingDirectory

// Kept as a top-level object (its own class) rather than nested in `Tests` so the ACP codecs the
// dispatchers inline do not add to the `Tests` class, which would otherwise exceed the JVM
// per-class size limit. The service and dispatchers are built once and sealed pure: a live
// capability may not inhabit an object field, but this fixture drives dispatch synchronously and
// retains nothing beyond the tests.
object TestClient:
  import Acp.*

  // What the handlers saw, for the assertions.
  val updates: scm.ArrayBuffer[Text] = scm.ArrayBuffer()

  val state: Acp.State = Acp.State()

  private val fixture: AnyRef =
    val registry: Acp.Registry^ = Acp.Registry()
    given registry0: (Acp.Registry^{registry}) = registry

    updated:
      update match
        case AgentMessageChunk(TextContent(text, _)) =>
          updates.synchronized(updates.append(t"${sessionId}:${text}"))

        case other =>
          updates.synchronized(updates.append(t"${sessionId}:other"))

    permission:
      options.filter(_.kind == PermissionOptionKind.AllowOnce).prim
      . lay(Cancelled): option => Selected(option.optionId)

    readFile:
      t"contents of ${path}"

    Acp.Service(registry, state).asInstanceOf[AnyRef]

  private def service: Acp.Service = fixture.asInstanceOf[Acp.Service]

  private val dispatch0: AnyRef =
    val serving: AcpClient = fixture.asInstanceOf[Acp.Service]
    val session: Json => Optional[Json] =
      caps.unsafe.unsafeAssumeSeparate(JsonRpc.serve[AcpClientSession](serving))

    val fs: Json => Optional[Json] =
      caps.unsafe.unsafeAssumeSeparate(JsonRpc.serve[AcpClientFs](serving))

    val terminal: Json => Optional[Json] =
      caps.unsafe.unsafeAssumeSeparate(JsonRpc.serve[AcpClientTerminal](serving))
    val sessionMethods = JsonRpc.methods[AcpClientSession]
    val fsMethods = JsonRpc.methods[AcpClientFs]

    val route: Json => Optional[Json] = json =>
      Acp.method(json).lay(session(json)): method =>
        if sessionMethods.has(method) then session(json)
        else if fsMethods.has(method) then fs(json)
        else terminal(json)

    caps.unsafe.unsafeAssumeSeparate(route).asInstanceOf[AnyRef]

  def dispatch(json: Json): Optional[Json] =
    dispatch0.asInstanceOf[Json => Optional[Json]](json)

  // Dispatch plus the service's fault-aware conclusion, as the exchange performs it.
  def roundtrip(json: Json): Optional[Json] = service.conclude(json, dispatch(json))

  def capabilities: Acp.ClientCapabilities = service.capabilities

// A scripted in-process agent joined to a real `Acp.connect` exchange by a pair of pipes: an
// exchange that crosses the framing and the codecs in both directions, exactly as it would
// between a client and an agent it launched. The script answers `initialize` and `session/new`,
// then for each `session/prompt` streams updates, requests a permission, and finally reports the
// turn's stop reason — unless the turn is cancelled first, in which case the permission request
// is made only after `session/cancel` arrives, exercising the mandatory `cancelled` answer.
object AgentFixture:
  import Acp.*
  import dynamicJsonAccess.enabled

  // What the fake agent observed, for the assertions.
  val received: scm.ArrayBuffer[Text] = scm.ArrayBuffer()

  private def record(note: Text): Unit = received.synchronized(received.append(note))

  private def result(id: Json, result: Json): Json =
    Map(t"jsonrpc" -> t"2.0".in[Json], t"id" -> id, t"result" -> result).in[Json]

  private def request(id: Text, method: Text, params: Json): Json =
    Map
     ( t"jsonrpc" -> t"2.0".in[Json],
       t"id"      -> id.in[Json],
       t"method"  -> method.in[Json],
       t"params"  -> params )
    . in[Json]

  private def notification(method: Text, params: Json): Json =
    Map(t"jsonrpc" -> t"2.0".in[Json], t"method" -> method.in[Json], t"params" -> params).in[Json]

  private def update(sessionId: Text, update: SessionUpdate): Json =
    notification
     ( t"session/update",
       Map(t"sessionId" -> sessionId.in[Json], t"update" -> update.in[Json]).in[Json] )

  private def permission(id: Text, sessionId: Text): Json =
    // Literal JSON rather than encoded records: the inline encodable for the opaque `List`
    // cannot be summoned here, and a fixture's wire text reads best as wire text anyway.
    val options: Json =
      t"""[{"optionId":"allow","name":"Allow","kind":"allow_once"},
          {"optionId":"deny","name":"Deny","kind":"reject_once"}]"""
      . as[Json]

    request
     ( id,
       t"session/request_permission",
       Map
        ( t"sessionId" -> sessionId.in[Json],
          t"toolCall"  -> t"""{"toolCallId":"call1"}""".as[Json],
          t"options"   -> options )
       . in[Json] )

  // Runs the scripted agent over the given streams until its input is exhausted.
  def serve(input: ji.InputStream, output: ji.OutputStream): Unit =
    val print: ji.PrintStream = ji.PrintStream(output, true)

    def send(json: Json): Unit = print.synchronized:
      print.print(json.encode.s + "\n")
      print.flush()

    // The prompt request currently awaiting its answer: the turn stays open until the
    // permission sequence completes, and its response must echo this request's id.
    var promptRequest: Optional[Json] = Unset

    val streamable = summon[ji.InputStream is Streamable by Data over Credit]

    streamable.stream(input).toProgression.stdlib.iterator.frames[Linefeed].each: frame =>
      val message: Text = frame.utf8

      if message.length > 0 then
        val json: Json = message.as[Json]
        val method: Text = try json.method.as[Text] catch case _: Exception => t""

        method match
          case t"initialize" =>
            record(t"initialize")

            send:
              result
               ( json.id,
                 Map
                  ( t"protocolVersion"   -> Acp.version.in[Json],
                    t"agentCapabilities" -> Map(t"loadSession" -> false.in[Json]).in[Json] )
                 . in[Json] )

          case t"session/new" =>
            record(t"session/new:${json.params.cwd.as[Text]}")
            send(result(json.id, Map(t"sessionId" -> t"sess1".in[Json]).in[Json]))

          case t"session/prompt" =>
            val sessionId: Text = json.params.sessionId.as[Text]
            val text: Text =
              try json.params.prompt.as[List[ContentBlock]] match
                case TextContent(text, _) :: _ => text
                case _                         => t""
              catch case _: Exception => t""

            record(t"session/prompt:$sessionId:$text")

            promptRequest = json

            if text == t"cancel me" then
              // Hold the turn open: the permission request is sent only once the client's
              // cancellation arrives.
              ()
            else
              send(update(sessionId, AgentMessageChunk(TextContent(t"Hello, "))))
              send(update(sessionId, AgentMessageChunk(TextContent(t"world!"))))
              send(update(sessionId, ToolCall(t"call1", t"Reading a file")))
              send(permission(t"perm1", sessionId))

          case t"session/cancel" =>
            val sessionId: Text = json.params.sessionId.as[Text]
            record(t"session/cancel:$sessionId")
            send(permission(t"perm2", sessionId))

          case t"" =>
            // A response: a permission answer, correlated by id.
            val id: Text = try json.id.as[Text] catch case _: Exception => t""
            val outcome: Text =
              try json.result.outcome.outcome.as[Text] catch case _: Exception => t"?"

            record(t"answer:$id:$outcome")

            def conclude(stopReason: Text): Unit =
              promptRequest.let: prompt =>
                send:
                  result
                   ( Acp.requestId(prompt).or(json.id),
                     Map(t"stopReason" -> stopReason.in[Json]).in[Json] )

              promptRequest = Unset

            if id == t"perm1" then conclude(t"end_turn")
            else if id == t"perm2" then conclude(t"cancelled")

          case other =>
            record(t"unexpected:$other")

// The `session/prompt` ids the fixture correlates on: the fake agent answers `perm1`'s prompt by
// re-reading the request that carried it, so the two prompts are distinguished by their text.
object Tests extends Suite(m"Espionage Tests"):
  def run(): Unit =
    import Acp.*

    suite(m"String enum codecs"):
      test(m"StopReason encodes as its wire string"):
        StopReason.MaxTurnRequests.in[Json].encode
      . assert(_ == t"\"max_turn_requests\"")

      test(m"StopReason decodes from its wire string"):
        t"\"refusal\"".as[Json].as[StopReason]
      . assert(_ == StopReason.Refusal)

      test(m"ToolKind decodes an unknown kind as Other"):
        t"\"_custom\"".as[Json].as[ToolKind]
      . assert(_ == ToolKind.Other)

      test(m"PermissionOptionKind round-trips"):
        val kinds = List
         ( PermissionOptionKind.AllowOnce, PermissionOptionKind.AllowAlways,
           PermissionOptionKind.RejectOnce, PermissionOptionKind.RejectAlways )

        kinds.map: kind =>
          kind.in[Json].encode.as[Json].as[PermissionOptionKind]
      . assert:
          _ == List
                ( PermissionOptionKind.AllowOnce, PermissionOptionKind.AllowAlways,
                  PermissionOptionKind.RejectOnce, PermissionOptionKind.RejectAlways )

    suite(m"Session update codecs"):
      test(m"agent_message_chunk decodes"):
        val json = t"""{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"hi"}}"""
        json.as[Json].as[SessionUpdate]
      . assert(_ == AgentMessageChunk(TextContent(t"hi")))

      test(m"user_message_chunk decodes"):
        val json = t"""{"sessionUpdate":"user_message_chunk","content":{"type":"text","text":"hi"}}"""
        json.as[Json].as[SessionUpdate]
      . assert(_ == UserMessageChunk(TextContent(t"hi")))

      test(m"agent_thought_chunk decodes"):
        val json = t"""{"sessionUpdate":"agent_thought_chunk","content":{"type":"text","text":"hm"}}"""
        json.as[Json].as[SessionUpdate]
      . assert(_ == AgentThoughtChunk(TextContent(t"hm")))

      test(m"tool_call decodes with kind, status and a diff"):
        val json =
          t"""{"sessionUpdate":"tool_call","toolCallId":"c1","title":"Edit","kind":"edit",
              "status":"in_progress","content":[{"type":"diff","path":"/a.txt","newText":"x"}]}"""

        json.as[Json].as[SessionUpdate]
      . assert:
          _ == ToolCall
                ( t"c1",
                  t"Edit",
                  ToolKind.Edit,
                  ToolCallStatus.InProgress,
                  List(ToolDiff(t"/a.txt", Unset, t"x")) )

      test(m"tool_call_update decodes with only an id and status"):
        val json = t"""{"sessionUpdate":"tool_call_update","toolCallId":"c1","status":"completed"}"""
        json.as[Json].as[SessionUpdate]
      . assert(_ == ToolCallUpdate(t"c1", status = ToolCallStatus.Completed))

      test(m"plan decodes"):
        val json =
          t"""{"sessionUpdate":"plan","entries":[{"content":"step","priority":"high","status":"pending"}]}"""

        json.as[Json].as[SessionUpdate]
      . assert(_ == Plan(List(PlanEntry(t"step", PlanPriority.High, PlanStatus.Pending))))

      test(m"available_commands_update decodes"):
        val json =
          t"""{"sessionUpdate":"available_commands_update","availableCommands":[{"name":"web","description":"Search"}]}"""

        json.as[Json].as[SessionUpdate]
      . assert(_ == AvailableCommandsUpdate(List(AvailableCommand(t"web", t"Search"))))

      test(m"current_mode_update decodes"):
        val json = t"""{"sessionUpdate":"current_mode_update","currentModeId":"yolo"}"""
        json.as[Json].as[SessionUpdate]
      . assert(_ == CurrentModeUpdate(t"yolo"))

    suite(m"Permission outcome codecs"):
      test(m"a selection encodes with its discriminator"):
        import dynamicJsonAccess.enabled
        val json = Selected(t"allow").asInstanceOf[RequestPermissionOutcome].in[Json]
        (json.outcome.as[Text], json.optionId.as[Text])
      . assert(_ == (t"selected", t"allow"))

      test(m"a cancellation encodes with its discriminator"):
        import dynamicJsonAccess.enabled
        Cancelled.asInstanceOf[RequestPermissionOutcome].in[Json].outcome.as[Text]
      . assert(_ == t"cancelled")

    suite(m"Capability derivation"):
      test(m"no registrations advertise no capabilities"):
        val registry: Acp.Registry^ = Acp.Registry()
        registry.capabilities
      . assert(_ == ClientCapabilities())

      test(m"a read handler advertises only readTextFile"):
        val registry: Acp.Registry^ = Acp.Registry()
        given registry0: (Acp.Registry^{registry}) = registry
        readFile(t"")
        registry.capabilities
      . assert(_ == ClientCapabilities(fs = FsCapabilities(readTextFile = true)))

      test(m"a terminal bundle advertises the terminal capability"):
        val registry: Acp.Registry^ = Acp.Registry()
        given registry0: (Acp.Registry^{registry}) = registry

        terminals:
          new Terminals:
            def create
              ( sessionId:       Text,
                command:         Text,
                args:            List[Text],
                env:             List[EnvVariable],
                cwd:             Optional[Text],
                outputByteLimit: Optional[Long] )
            :   Text =
              t"term1"

            def output(sessionId: Text, terminalId: Text): TerminalOutputResult =
              TerminalOutputResult(t"", false)

            def waitForExit(sessionId: Text, terminalId: Text): TerminalExitStatus =
              TerminalExitStatus(0)

            def kill(sessionId: Text, terminalId: Text): Unit = ()
            def release(sessionId: Text, terminalId: Text): Unit = ()

        registry.capabilities
      . assert(_ == ClientCapabilities(terminal = true))

      test(m"the adjust hook is applied last"):
        val registry: Acp.Registry^ = Acp.Registry()
        given registry0: (Acp.Registry^{registry}) = registry
        capabilities(_.copy(terminal = true))
        registry.capabilities
      . assert(_ == ClientCapabilities(terminal = true))

      test(m"the fixture derives fs.readTextFile and nothing else"):
        TestClient.capabilities
      . assert(_ == ClientCapabilities(fs = FsCapabilities(readTextFile = true)))

    suite(m"Dispatch"):
      test(m"session/update reaches the registered handler"):
        TestClient.updates.synchronized(TestClient.updates.clear())

        val message =
          t"""{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"s1",
              "update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"hi"}}}}"""

        TestClient.roundtrip(message.as[Json])
        TestClient.updates.synchronized(TestClient.updates.to(List))
      . assert(_ == List(t"s1:hi"))

      test(m"session/request_permission answers the handler's selection"):
        val message =
          t"""{"jsonrpc":"2.0","id":9,"method":"session/request_permission","params":{
              "sessionId":"s1","toolCall":{"toolCallId":"c1"},
              "options":[{"optionId":"go","name":"Go","kind":"allow_once"}]}}"""

        import dynamicJsonAccess.enabled
        TestClient.roundtrip(message.as[Json]).let(_.result.outcome.optionId.as[Text])
      . assert(_ == t"go")

      test(m"session/request_permission rejects when no option is allowable"):
        val message =
          t"""{"jsonrpc":"2.0","id":10,"method":"session/request_permission","params":{
              "sessionId":"s1","toolCall":{"toolCallId":"c1"},
              "options":[{"optionId":"no","name":"No","kind":"reject_once"}]}}"""

        import dynamicJsonAccess.enabled
        TestClient.roundtrip(message.as[Json]).let(_.result.outcome.outcome.as[Text])
      . assert(_ == t"cancelled")

      test(m"a cancelled session answers permission requests with cancelled"):
        TestClient.state.cancel(t"s2")

        val message =
          t"""{"jsonrpc":"2.0","id":11,"method":"session/request_permission","params":{
              "sessionId":"s2","toolCall":{"toolCallId":"c1"},
              "options":[{"optionId":"go","name":"Go","kind":"allow_once"}]}}"""

        import dynamicJsonAccess.enabled
        TestClient.roundtrip(message.as[Json]).let(_.result.outcome.outcome.as[Text])
      . assert(_ == t"cancelled")

      test(m"fs/read_text_file answers the handler's content"):
        val message =
          t"""{"jsonrpc":"2.0","id":12,"method":"fs/read_text_file","params":{
              "sessionId":"s1","path":"/tmp/a.txt"}}"""

        TestClient.roundtrip(message.as[Json])
        . let(_.as[JsonRpc.Response].result.as[ReadTextFileResult])
      . assert(_ == ReadTextFileResult(t"contents of /tmp/a.txt"))

      test(m"an unregistered capability is answered with an error"):
        val message =
          t"""{"jsonrpc":"2.0","id":13,"method":"fs/write_text_file","params":{
              "sessionId":"s1","path":"/tmp/a.txt","content":"x"}}"""

        import dynamicJsonAccess.enabled
        TestClient.roundtrip(message.as[Json]).let(_.error.code.as[Int])
      . assert(_ == -32601)

    suite(m"Agent exchange"):
      test(m"a full prompt turn crosses the exchange"):
        AgentFixture.received.synchronized(AgentFixture.received.clear())
        TestClient.updates.synchronized(TestClient.updates.clear())

        val toAgent: ji.PipedOutputStream = ji.PipedOutputStream()
        val agentIn: ji.PipedInputStream = ji.PipedInputStream(toAgent, 65536)
        val toClient: ji.PipedOutputStream = ji.PipedOutputStream()
        val clientIn: ji.PipedInputStream = ji.PipedInputStream(toClient, 65536)

        val log: scm.ArrayBuffer[Text] = scm.ArrayBuffer()

        supervise:
          async(AgentFixture.serve(agentIn, toClient))

          Acp.connect(Acp.Agent.streams(clientIn, toAgent)):
            updated:
              update match
                case AgentMessageChunk(TextContent(text, _)) =>
                  log.synchronized(log.append(t"chunk:$text"))

                case update: ToolCall =>
                  log.synchronized(log.append(t"tool:${update.title}"))

                case other =>
                  log.synchronized(log.append(t"other"))

            permission:
              options.filter(_.kind == PermissionOptionKind.AllowOnce).prim
              . lay(Cancelled): option => Selected(option.optionId)

          . apply:
              val session = connection.newSession(t"/workspace")
              log.synchronized(log.append(t"session:${session.sessionId}"))
              val stop = connection.prompt(session.sessionId, t"hello agent")
              log.synchronized(log.append(t"stop:$stop"))

        log.synchronized(log.to(List))
      . assert:
          _ == List
                ( t"session:sess1",
                  t"chunk:Hello, ",
                  t"chunk:world!",
                  t"tool:Reading a file",
                  t"stop:EndTurn" )

      test(m"the agent saw the turn as scripted"):
        AgentFixture.received.synchronized(AgentFixture.received.to(List))
      . assert:
          _ == List
                ( t"initialize",
                  t"session/new:/workspace",
                  t"session/prompt:sess1:hello agent",
                  t"answer:perm1:selected" )

      test(m"a cancelled turn ends with the cancelled stop reason"):
        AgentFixture.received.synchronized(AgentFixture.received.clear())

        val toAgent: ji.PipedOutputStream = ji.PipedOutputStream()
        val agentIn: ji.PipedInputStream = ji.PipedInputStream(toAgent, 65536)
        val toClient: ji.PipedOutputStream = ji.PipedOutputStream()
        val clientIn: ji.PipedInputStream = ji.PipedInputStream(toClient, 65536)

        val stopped: Promise[StopReason] = Promise()
        var stop: Optional[StopReason] = Unset

        supervise:
          async(AgentFixture.serve(agentIn, toClient))

          Acp.connect(Acp.Agent.streams(clientIn, toAgent)):
            permission:
              options.filter(_.kind == PermissionOptionKind.AllowOnce).prim
              . lay(Cancelled): option => Selected(option.optionId)

          . apply:
              val session = connection.newSession(t"/workspace")

              async(stopped.offer(connection.prompt(session.sessionId, t"cancel me")))

              // Cancel once the agent has the prompt: the fixture answers the prompt only after
              // its post-cancellation permission request is answered `cancelled`.
              while AgentFixture.received.synchronized(AgentFixture.received.length) < 3
              do Thread.sleep(10)

              connection.cancel(session.sessionId)
              stop = stopped.await()

        stop
      . assert(_ == StopReason.Cancelled)

      test(m"the cancelled turn's permission request was answered cancelled"):
        AgentFixture.received.synchronized(AgentFixture.received.to(List))
      . assert(_.has(t"answer:perm2:cancelled"))
