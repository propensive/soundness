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
package exegesis

import scala.caps

import java.io as ji

import ambience.*
import anticipation.*
import aperture.*
import contingency.*
import distillate.*
import eucalyptus.*
import fulminate.*
import gossamer.*
import guillotine.*
import jacinta.*
import obligatory.*
import parasite.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

// Adapts an `Lsp.Listener` — whose methods are named for what they mean — to the wire methods
// `JsonRpc.serve` dispatches. `LspClient` is the server-to-client half of the protocol written for
// the server that *calls* it; this is the client that *receives* it. Keeping the two apart leaves
// `LspClient`'s named wrappers free to read as the calling convention they are.
private[exegesis] class LspInbound(listener: Lsp.Listener) extends LspClient:
  import Lsp.*

  def `textDocument/publishDiagnostics`
     ( uri: Text, version: Optional[Int], diagnostics: List[Diagnostic] )
  :   Unit =
    listener.diagnostics(uri, version, diagnostics)

  def `window/showMessage`(`type`: MessageType, message: Text): Unit =
    listener.message(`type`, message)

  def `window/logMessage`(`type`: MessageType, message: Text): Unit =
    listener.log(`type`, message)

  def `telemetry/event`(params: Json): Unit = listener.telemetry(params)
  def `$/logTrace`(message: Text, verbose: Optional[Text]): Unit = listener.trace(message)
  def `$/progress`(token: Json, value: Json): Unit = listener.progress(token, value)
  def `$/cancelRequest`(id: Json): Unit = listener.cancel(id)

// Establishes a session with a language server: opens the channel to it, starts a writer draining
// the connection's outgoing messages onto it and a reader routing what comes back, lends the
// connection, and tears everything down afterwards.
//
// The reader is its own task, which is what lets a caller block on a response: a request awaits
// its promise while the reader goes on reading, so several requests may be in flight at once and
// may be answered out of order. (A server, whose handlers run on the thread reading its input,
// cannot do the same — hence the notifications-only restriction documented on `LspClient`, which
// does not apply in this direction.)
object LspSessional:
  // A free function, not a method: the reader is supplied as a partly-applied `pump`, whose
  // closure captures the observer, and a method of the session would have that same observer in
  // its own prefix — an overlap separation checking rejects.
  private[exegesis] def exchange[result]
     ( listener: Lsp.Listener,
       observer: Lsp.Observer,
       sink:     (Intake[Data] over Credit)^,
       read:     (Text => Unit) => Unit )
     ( lambda: LspConnection => result )
     ( using Monitor, Probate, Diagnostics )
  :   result =

    import strategies.throwUnsafely
    import Json.jsonEncodableInText

    // Sealed: the connection captures this session's monitor and diagnostics, and an honest
    // `LspConnection^` would hide them from the writer and reader that serve it. It is a local of
    // this method, lent to `lambda` and dead once `lambda` returns.
    val connection: LspConnection = caps.unsafe.unsafeAssumePure(LspConnection())
    val inbound: LspClient = LspInbound(listener)

    // As in `Lsp.listen`, the dispatch closure is a local of this method and its target is
    // confined to it, so sealing the reference the generated dispatcher holds is sound.
    val dispatch: Json => Optional[Json] =
      caps.unsafe.unsafeAssumeSeparate(JsonRpc.serve[LspClient](inbound))

    val notifications: List[Text] = JsonRpc.methods[LspClient]

    // A single writer, so writes never interleave. The observer sees the encoded body, not the
    // framing, matching `listen`.
    val writer: Task[Unit] = async:
      connection.outgoing.stdlib.iterator.each: json =>
        val body: Text = json.encode
        observer.sent(body)
        sink.put(LspTransport.frame(body))
        sink.flush()

    // The channel is read by a partly-applied `pump`, rather than by handing the stream over: the
    // stream is single-owner, so it is minted and consumed within the reader task, which also
    // keeps this thread off the channel's first refill — a blocking read that would otherwise
    // happen before the writer that unblocks it has started.
    val reader: Task[Unit] = async:
      read: message =>
        safely(message.as[Json]).let: json =>
          if listener.intercept(json) then Unset
          else Lsp.method(json).lay(dispatch(json)): method =>
            // A method this client models is dispatched to the listener's handler; anything else
            // is a request the server makes of its client, which the listener answers, or a
            // notification it is free to ignore.
            if notifications.has(method) then dispatch(json) else Lsp.requestId(json).let: id =>
              val params: Json = Lsp.params(json)
              val answer = listener.request(method, params).or(Json.ast(Json.Ast(Unset)))
              connection.put(JsonRpc.Response("2.0", answer, id).in[Json])

    try lambda(connection) finally
      reader.cancel()
      writer.cancel()

class LspSessional
   ( listener: Lsp.Listener = Lsp.Listener.Quiet,
     observer: Lsp.Observer = Lsp.Observer.Silent )
   ( using Monitor, Probate, Diagnostics, WorkingDirectory )
extends Sessional:
  type Self = Lsp.Server
  type Result = LspConnection^

  def session[result](target: Lsp.Server)(lambda: (session: Result) ?=> result): result =
    import strategies.throwUnsafely

    target match
      case Lsp.Server.Streams(input, output) =>
        val streamable = summon[ji.InputStream is Streamable by Data over Credit]
        val sink = summon[ji.OutputStream is Sink by Data over Credit].intake(output)

        try
          LspSessional.exchange
           ( listener, observer, sink, LspTransport.pump(streamable.stream(input), observer)(_) )
           ( lambda(using _) )

        finally sink.finish()

      case Lsp.Server.Process(command) =>
        // Launched with a silent logger and a throwing tactic: `Sessional#session` fixes the
        // signature, so a failure to launch cannot be raised through a caller's `Tactic`, and an
        // exec log has no channel to reach — a language server owns standard output.
        import logging.silentLogging

        val job = command.fork[Exit]()

        // The child is killed rather than waited for: a server that ignores `exit`, or that never
        // receives it because the exchange ended early, would otherwise outlive the session it
        // was lent to.
        try
          LspSessional.exchange
           ( listener, observer, job.intake, LspTransport.pump(job.stdout(), observer)(_) )
           ( lambda(using _) )

        finally job.abort()
