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
package synesthesia

import soundness.*

object Tests extends Suite(m"Synesthesia Tests"):
  def run(): Unit =
    // A missing `Inspectable` is never a compile error, so coverage is held in place by
    // asserting on the renderings: `fallbacks` returns those which used a marked fallback.
    // synesthesia's own types are case classes and enums, rendered structurally by derivation;
    // only `Mcp.TextInt`, whose `id` is a `Text | Int` union, needs an instance of its own.
    suite(m"Native-rendering coverage"):
      test(m"a Discourse message inspects structurally"):
        Human(t"hello").inspect
      . assert(_ == t"Human(message:t\"hello\")")

      test(m"a TextInt inspects with its union field resolved"):
        Mcp.TextInt(42).inspect
      . assert(_ == t"TextInt(id:42)")

      test(m"synesthesia's types inspect natively"):
        Inspectable.fallbacks
         ( Human(t"hello").inspect,
           Agent(t"hi").inspect,
           Mcp.BaseMetadata(t"name").inspect,
           Mcp.LoggingLevel.Debug.inspect,
           Mcp.TaskStatus.Working.inspect,
           Mcp.TextInt(42).inspect )
      . assert(_ == Nil)

    suite(m"Session termination"):
      import internetAccess.online
      import supervisors.globalSupervisor
      import probates.cancelProbate
      import strategies.throwUnsafely
      import threads.platformThreads
      import codepages.utf8Codepage
      import logging.silentLogging
      import classloaders.threadContextClassloader

      def respond(method: Http.Method, session: Optional[Text]): Http.Status =
        val headers: proscenium.List[Http.Header] =
          session.lay(proscenium.List())(id => proscenium.List(Http.Header(t"Mcp-Session-Id", id)))

        given request: Http.Request =
          Http.Request(method, 1.1, t"localhost".as[Host], t"/mcp", headers, () => Http.emptyBody())

        supervise(TestMcpServer.serve.status)

      test(m"Deleting an open session answers No Content"):
        respond(Http.Options, t"session-1")
        respond(Http.Delete, t"session-1")
      . assert(_ == Http.NoContent)

      test(m"Deleting a session twice answers Not Found the second time"):
        respond(Http.Options, t"session-2")
        respond(Http.Delete, t"session-2")
        respond(Http.Delete, t"session-2")
      . assert(_ == Http.NotFound)

      test(m"Deleting an unknown session answers Not Found"):
        respond(Http.Delete, t"session-unknown")
      . assert(_ == Http.NotFound)

      test(m"Deleting without a session id answers Bad Request"):
        respond(Http.Delete, Unset)
      . assert(_ == Http.BadRequest)

      test(m"An unsupported method answers Method Not Allowed"):
        respond(Http.Put, t"session-3")
      . assert(_ == Http.MethodNotAllowed)

    // The specification is derived here, in a module compiled with capture checking, which
    // pins the derivation to being capture-clean.
    suite(m"Derived specification"):
      import internetAccess.online
      import supervisors.globalSupervisor
      import probates.cancelProbate
      import strategies.throwUnsafely
      import threads.platformThreads
      import logging.silentLogging
      import codepages.utf8Codepage

      val spec = summon[TestMcpServer.type is Mcp.Specification]
      val interface = Mcp.Interface(t"session-spec", TestMcpServer)

      def schema(name: Text): Optional[JsonSchema.Object] =
        spec.tools().seek(_.name == name).let(_.inputSchema).let:
          case schema: JsonSchema.Object => schema
          case _                         => Unset

      def call(name: Text, arguments: Text): Mcp.CallTool =
        interface.`tools/call`(name, arguments.read[Json], Unset)

      test(m"An Optional parameter and one with a default are not required"):
        schema(t"greet").let(_.required)
      . assert(_ == List(t"name"))

      test(m"A parameter with no default or Optional type is required"):
        schema(t"color").let(_.required)
      . assert(_ == List(t"name"))

      test(m"A parameter's description comes from its own @about"):
        schema(t"greet").let(_.properties.at(t"name")).let(_.description)
      . assert(_ == t"whom to greet")

      test(m"An omitted Optional parameter is Unset and the default argument applies"):
        call(t"greet", t"""{"name": "Jon"}""").structuredContent
      . assert(_ == t"""{"result": "Hello, Jon!"}""".read[Json])

      test(m"Supplied Optional and defaulted parameters are decoded"):
        call(t"greet", t"""{"name": "Jon", "greeting": "Hi", "punctuation": "?"}""")
        . structuredContent
      . assert(_ == t"""{"result": "Hi, Jon?"}""".read[Json])

      test(m"A missing required parameter is a protocol error"):
        try
          call(t"greet", t"""{"greeting": "Hi"}""")
          Unset
        catch case error: Mcp.Error => error.reason
      . assert(_ == Mcp.Error.Reason.MissingParameter)

      test(m"An unknown tool is a protocol error"):
        try
          call(t"vanish", t"{}")
          Unset
        catch case error: Mcp.Error => error.reason
      . assert(_ == Mcp.Error.Reason.UnknownMethod)

      test(m"A tool that throws answers a result marked isError"):
        call(t"explode", t"""{"reason": "boom"}""")
      . assert(_ == Mcp.CallTool(List(Mcp.TextContent(t"boom")), isError = true))

      def mimeType(uri: Text): Optional[Text] =
        interface.`resources/read`(uri, Unset).contents.prim.let(_.contents).let:
          case contents: Mcp.TextResourceContents => contents.mimeType
          case contents: Mcp.BlobResourceContents => contents.mimeType

      test(m"A resource is read with the MIME type its annotation gives"):
        mimeType(t"doc://schema")
      . assert(_ == t"application/schema+json")

      test(m"A text resource with no MIME type is read as text/plain"):
        mimeType(t"doc://notes")
      . assert(_ == t"text/plain")

      test(m"A resource a tool's @ui names is read with the MCP app profile"):
        mimeType(t"ui://html/content")
      . assert(_ == t"text/html;profile=mcp-app")

      test(m"The resource listing carries the MIME type"):
        spec.resources().seek(_.uri == t"doc://schema").let(_.mimeType)
      . assert(_ == t"application/schema+json")

      test(m"A tool's @ui becomes its visibility metadata"):
        spec.tools().seek(_.name == t"encodeMagic").let(_._meta)
      . assert:
          _ == t"""{"ui": {"visibility": ["model", "app"], "resourceUri": "ui://html/content"}}"""
                . read[Json]

    // Manual-only MCP server runner — NOT an automated test. It serves MCP on :8080
    // and `Thread.sleep`s to keep the server alive for an external MCP client to
    // connect to; it asserts nothing and blocked CI for ~16 minutes. Disabled here;
    // uncomment (and restore the `strategies.throwUnsafely` / `codepages.utf8Codepage`
    // imports) to run a live server by hand.
    //
    // test(m"Remote server"):
    //   import internetAccess.online
    //   import supervisors.globalSupervisor
    //   import probates.cancelProbate
    //   import httpServers.jdkHttpserver
    //   import logging.silentLogging
    //   import webserverErrorPages.stackTracesErrorPage
    //   import classloaders.threadContextClassloader
    //
    //   tcp"8080".serve:
    //     request.path match
    //       case % /: t"mcp" =>
    //         try
    //           unsafely:
    //             TestMcpServer.serve
    //         catch case throwable: Throwable =>
    //           throwable.printStackTrace()
    //           ???
    //
    //       case _ =>
    //         Http.Response(Http.NotFound)(t"Error 404: Not found")
    //
    //   Thread.sleep(1000000)
    //
    // . assert()
    ()
