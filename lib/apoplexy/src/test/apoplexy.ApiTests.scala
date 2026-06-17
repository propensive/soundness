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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package apoplexy

import soundness.*

import strategies.throwUnsafely
import logging.silentLogging
import internetAccess.online
import charEncoders.utf8Encoder
import charDecoders.utf8Decoder
import textSanitizers.skipSanitizer
import formatting.compactJsonFormatting
import errorDiagnostics.stackTracesDiagnostics

case class Credentials(username: Text, password: Text)
case class NewPet(name: Text, tag: Optional[Text] = Unset)
case class Photo(url: Text, width: Optional[Int] = Unset, height: Optional[Int] = Unset)
case class Pet(id: Int, name: Text, tag: Optional[Text] = Unset)
case class Note(id: Int, text: Text)
case class NewNote(text: Text)

// A test `Http.Backend` that captures the request it is given and replies with a
// canned response, so `.call` can be exercised without any network access.
class Recorder(canned: () => Http.Response) extends Http.Backend:
  var lastUrl:     Optional[Text]        = Unset
  var lastMethod:  Optional[Http.Method] = Unset
  var lastBody:    Optional[IArray[Byte]] = Unset
  var lastHeaders: List[Http.Header]     = Nil

  def request
     ( url: Text, method: Http.Method, headers: List[Http.Header], body: () => Stream[Data] )
     ( using Tactic[ConnectError] )
  :   Http.Response =
    lastUrl = url
    lastMethod = method
    lastHeaders = headers
    val chunks = body().to(List)
    lastBody = if chunks.isEmpty then Unset else chunks.head
    canned()

object ApiTests extends Suite(m"Api client tests"):
  def run(): Unit =
    given XmlSchema = XmlSchema.Freeform

    val api = Api(cp"/apoplexy/petstore.json")

    val petJson  = t"""{"id": 42, "name": "Milo", "tag": "cat"}"""
    val petsJson = t"""[{"id": 1, "name": "Ada"}, {"id": 2, "name": "Bea"}]"""

    def ok(body: Text): Http.Response =
      Http.Response(Http.Ok, contentType = media"application/json")(body)

    suite(m"navigation refines the path type"):
      test(m"a literal segment refines Locus"):
        val pets: Api at "/pets" = api.pets
        pets.request.path
      . assert(_ == t"/pets")

      test(m"a positional arg fills the following path template"):
        val one: Api at "/pets/{petId}" = api.pets(42)
        one.request.substitutions
      . assert(_ == Map(t"petId" -> t"42"))

      test(m"nested templated navigation"):
        val photos: Api at "/pets/{petId}/photos" = api.pets(42).photos
        photos.request.path
      . assert(_ == t"/pets/{petId}/photos")

    suite(m"the apply shortcut invokes the sole non-DELETE method"):
      test(m"GET sole method with query params (the user's example shape)"):
        api.pets(42).photos(width = 10, height = 20).request
      . assert: request =>
          request.method == Http.Get && request.path == t"/pets/{petId}/photos"
          && request.substitutions == Map(t"petId" -> t"42")
          && request.query == List(t"width" -> t"10", t"height" -> t"20")

      test(m"POST sole method with a positional body"):
        api.login(Credentials(t"jon", t"pw")).request

      . assert: request =>
          request.method == Http.Post && request.path == t"/login" && request.body != Api.Body.Empty

      test(m"PUT sole method with a positional body (verb omitted)"):
        api.profile(NewPet(t"Rex")).request

      . assert: request =>
          request.method == Http.Put && request.path == t"/profile" &&
            request.body != Api.Body.Empty

      test(m"the verb is still explicitly usable on a sole-method endpoint"):
        api.profile.put(NewPet(t"Rex")).request.method
      . assert(_ == Http.Put)

      test(m"an optional query parameter may be omitted"):
        api.pets(42).photos(width = 10).request.query
      . assert(_ == List(t"width" -> t"10"))

    suite(m"explicit terminals for multi-method endpoints"):
      test(m"GET /pets via explicit .get with a query parameter"):
        api.pets.get(limit = 10).request
      . assert(request => request.method == Http.Get && request.query == List(t"limit" -> t"10"))

      test(m"POST /pets via explicit .post with a body"):
        api.pets.post(NewPet(t"Milo", tag = t"cat")).request
      . assert(request => request.method == Http.Post && (request.body != Api.Body.Empty))

      test(m"GET /pets/{petId} via explicit .get with no arguments"):
        api.pets(42).get.request
      . assert(request => request.method == Http.Get && request.path == t"/pets/{petId}")

      test(m"PUT /pets/{petId} via explicit .put with a body"):
        api.pets(42).put(NewPet(t"Rex")).request.method
      . assert(_ == Http.Put)

    suite(m"delete is always explicit"):
      test(m"DELETE /pets/{petId}"):
        api.pets(42).delete.request.method
      . assert(_ == Http.Delete)

      test(m"a DELETE-only endpoint with a path parameter"):
        api.sessions(t"abc").delete.request

      . assert: request =>
          request.method == Http.Delete && request.substitutions == Map(t"token" -> t"abc")

      test(m"a DELETE-only endpoint reached by a bare segment"):
        api.logout.delete.request.method
      . assert(_ == Http.Delete)

    suite(m"compile-time safety"):
      test(m"a nonexistent first segment is rejected"):
        demilitarize(api.unicorns).length
      . assert(_ > 0)

      test(m"an undeclared path is rejected"):
        demilitarize(api.pets(42).toys).length
      . assert(_ > 0)

      test(m"a path parameter of the wrong type is rejected"):
        demilitarize(api.pets(t"notAnInt")).length
      . assert(_ > 0)

      test(m"the apply shortcut is rejected on a multi-method endpoint"):
        demilitarize(api.pets(limit = 10)).length
      . assert(_ > 0)

      test(m"the apply shortcut cannot invoke a DELETE-only endpoint"):
        demilitarize(api.logout()).length
      . assert(_ > 0)

      test(m"a query parameter of the wrong type is rejected"):
        demilitarize(api.pets(42).photos(width = t"big")).length
      . assert(_ > 0)

      test(m"omitting a required query parameter is rejected"):
        demilitarize(api.pets(42).photos(height = 20)).length
      . assert(_ > 0)

    suite(m"sending and decoding responses"):
      test(m".call[Pet]() decodes a single pet"):
        given Http.Backend = Recorder(() => ok(petJson))
        api.pets(42).get.call[Pet]()
      . assert(_ == Pet(42, t"Milo", t"cat"))

      test(m".call[List[Pet]]() decodes a list of pets"):
        given Http.Backend = Recorder(() => ok(petsJson))
        api.pets.get(limit = 10).call[List[Pet]]()
      . assert(_ == List(Pet(1, t"Ada"), Pet(2, t"Bea")))

      test(m".call[Json]() returns the raw body"):
        given Http.Backend = Recorder(() => ok(petJson))
        api.pets(42).get.call[Json]()
      . assert(_.as[Pet] == Pet(42, t"Milo", t"cat"))

      test(m".call[Http.Response]() returns the raw response"):
        given Http.Backend = Recorder(() => ok(petJson))
        api.pets(42).get.call[Http.Response]().status
      . assert(_ == Http.Ok)

      test(m".call[Unit]() on a DELETE checks success and discards the body"):
        given Http.Backend = Recorder(() => Http.Response(Http.NoContent)())
        api.sessions(t"abc").delete.call[Unit]()
      . assert(_ == ())

      test(m"a bare .call() defaults to Unit"):
        val recorder = Recorder(() => Http.Response(Http.NoContent)())
        given Http.Backend = recorder
        api.logout.delete.call()
        recorder.lastMethod
      . assert(_ == Http.Delete)

      test(m"the request URL and method are sent as navigated"):
        val recorder = Recorder(() => ok(petJson))
        given Http.Backend = recorder
        api.pets(42).get.call[Pet]()
        (recorder.lastUrl, recorder.lastMethod)
      . assert(_ == (t"https://api.example.com/v1/pets/42", Http.Get))

      test(m"a POST sends its body"):
        val recorder = Recorder(() => ok(petJson))
        given Http.Backend = recorder
        api.pets.post(NewPet(t"Milo", tag = t"cat")).call[Pet]()
        (recorder.lastMethod, recorder.lastBody.present)
      . assert(_ == (Http.Post, true))

      test(m"a non-2xx response raises ApiError"):
        given Http.Backend = Recorder(() => Http.Response(Http.NotFound)(t"{}"))
        capture[ApiError](api.pets(42).get.call[Pet]()).reason
      . assert(_ == ApiError.Reason.Status(404))

      test(m"a type that does not conform to the schema is rejected"):
        demilitarize:
          given Http.Backend = Recorder(() => ok(petJson))
          api.pets(42).get.call[Photo]()
        . length
      . assert(_ > 0)

    suite(m"the spec decides the wire format (Api over Json / over Xml)"):
      val xmlApi = Api(cp"/apoplexy/xmlstore.json")
      val noteXml = t"<Note><id>1</id><text>hello</text></Note>"

      def okXml(body: Text): Http.Response =
        Http.Response(Http.Ok, contentType = media"application/xml")(body)

      test(m"a uniform JSON spec is tracked as `Api over Json`"):
        val typed: Api over Json = api
        typed.request.path
      . assert(_ == t"/")

      test(m"a uniform XML spec is tracked as `Api over Xml`"):
        val typed: Api over Xml = xmlApi
        typed.request.path
      . assert(_ == t"/")

      test(m"an XML endpoint's response is `Api.Response over Xml`"):
        given Http.Backend = Recorder(() => okXml(noteXml))
        val typed: Api.Response over Xml = xmlApi.notes(1).get
        typed.request.method
      . assert(_ == Http.Get)

      test(m".call[Note]() decodes an XML response body"):
        given Http.Backend = Recorder(() => okXml(noteXml))
        xmlApi.notes(1).get.call[Note]()
      . assert(_ == Note(1, t"hello"))

      test(m"an XML GET sends `Accept: application/xml`"):
        val recorder = Recorder(() => okXml(noteXml))
        given Http.Backend = recorder
        xmlApi.notes(1).get.call[Note]()
        recorder.lastHeaders.filter(_.key == t"accept").map(_.value)
      . assert(_ == List(t"application/xml"))

      test(m"the request body is encoded as XML"):
        xmlApi.notes.post(NewNote(t"hi")).request.body match
          case Api.Body.Xml(_) => true
          case _               => false
      . assert(_ == true)

      test(m"an XML POST sends an XML body and content-type"):
        val recorder = Recorder(() => Http.Response(Http.Created, contentType = media"application/xml")(noteXml))
        given Http.Backend = recorder
        xmlApi.notes.post(NewNote(t"hi")).call[Note]()
        (recorder.lastHeaders.filter(_.key == t"content-type").map(_.value), recorder.lastBody.present)
      . assert(_ == (List(t"application/xml"), true))
