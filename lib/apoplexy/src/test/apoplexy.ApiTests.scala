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
package apoplexy

import soundness.*

import strategies.throwUnsafely
import logging.silentLogging
import construables.{jsonConstruable, xmlConstruable, plainTextConstruable, pngConstruable, formConstruable}
import construables.{multipartConstruable, multipartMixedConstruable}
import construables.{zipConstruable, tarConstruable, pdfConstruable, pemConstruable}
import postables.{jsonPostable, xmlPostable}
import classloaders.threadContextClassloader
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
case class Item(id: Long, name: Text)
case class NewItem(name: Text)

// A test `Http.Backend` that captures the request it is given and replies with a
// canned response, so `.call` can be exercised without any network access.
class Recorder(canned: () => Http.Response) extends Http.Backend:
  @scala.caps.unsafe.untrackedCaptures
  var lastUrl:     Optional[Text]        = Unset
  @scala.caps.unsafe.untrackedCaptures
  var lastMethod:  Optional[Http.Method] = Unset
  @scala.caps.unsafe.untrackedCaptures
  var lastBody:    Optional[Array[Byte]^{}] = Unset
  @scala.caps.unsafe.untrackedCaptures
  var lastHeaders: List[Http.Header]     = Nil

  def request
     ( url: Text, method: Http.Method, headers: List[Http.Header],
        body: Spring[Data]^ )
     ( using Tactic[Connect.Error] )
  :   Http.Response =
    lastUrl = url
    lastMethod = method
    lastHeaders = headers
    val data = body().memoize
    lastBody = if data.readable.isEmpty then Unset else data
    canned()

object ApiTests extends Suite(m"Api client tests"):
  // A complete classic-xref PDF from pre-rendered object bodies, numbered from 1 with correct
  // byte offsets; object 1 is the catalog. After facsimile's own test helper, assembled as text
  // since every byte of such a document is ASCII.
  def pdfDocument(bodies: Text*): Data =
    def pad10(value: Int): Text =
      val digits = value.toString
      ("0".repeat(10 - digits.length).nn + digits).tt

    var out: Text = t"%PDF-1.7\n"
    val offsets = scala.collection.immutable.List.newBuilder[Int]

    bodies.zipWithIndex.each: (body, index) =>
      offsets += out.length
      out = t"$out${index + 1} 0 obj\n$body\nendobj\n"

    val xrefOffset = out.length
    out = t"${out}xref\n0 ${bodies.length + 1}\n0000000000 65535 f \n"

    offsets.result().each: offset =>
      out = t"$out${pad10(offset)} 00000 n \n"

    t"${out}trailer\n<< /Size ${bodies.length + 1} /Root 1 0 R >>\nstartxref\n$xrefOffset\n%%EOF".in[Data]

  def run(): Unit =
    given XmlSchema = XmlSchema.Freeform

    val api = Api(cp"/openapi/local/petstore.json")

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
          && request.query.values == List(t"width" -> t"10", t"height" -> t"20")

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
        api.pets(42).photos(width = 10).request.query.values
      . assert(_ == List(t"width" -> t"10"))

    suite(m"explicit terminals for multi-method endpoints"):
      test(m"GET /pets via explicit .get with a query parameter"):
        api.pets.get(limit = 10).request
      . assert: request =>
          request.method == Http.Get && request.query.values == List(t"limit" -> t"10")

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

      test(m"a status the spec does not declare raises Api.Violation"):
        given Http.Backend = Recorder(() => Http.Response(Http.NotFound)(t"gone"))
        capture[Api.Violation](api.pets(42).get.call[Pet]()).status
      . assert(_ == Http.NotFound)

      test(m"a type that does not conform to the schema is rejected"):
        demilitarize:
          given Http.Backend = Recorder(() => ok(petJson))
          api.pets(42).get.call[Photo]()
        . length
      . assert(_ > 0)

    // The refstore spec requires `queryKey` (an API key in the query) for every operation but
    // `GET /items`, and `cookieKey` for `/items/{itemId}/notes`
    given queryKey: ("queryKey" is Credential to Text) = Credential(t"q-1")
    given cookieKey: ("cookieKey" is Credential to Text) = Credential(t"s-1")

    suite(m"references, path-level parameters, headers and servers"):
      val refs = Api(cp"/openapi/local/refstore.json", base = url"https://ref.example.com")
      val itemJson = t"""{"id": 7, "name": "spoon"}"""

      test(m"a relative server URL, with its variable at its default, extends the base"):
        refs.request.base.show
      . assert(_ == t"https://ref.example.com/v2")

      test(m"a path-level int64 parameter accepts a Long"):
        refs.items(7L).request.substitutions
      . assert(_ == Map(t"itemId" -> t"7"))

      test(m"a path-level int64 parameter accepts an Int"):
        refs.items(7).request.substitutions
      . assert(_ == Map(t"itemId" -> t"7"))

      test(m"a referenced query parameter is recognised"):
        refs.items.get(limit = 5, `X-Request-Id` = t"r1").request.query.values
      . assert(_ == List(t"limit" -> t"5"))

      test(m"a path-level header parameter is sent as a header"):
        refs.items.get(`X-Request-Id` = t"r1").request.headers
      . assert(_ == List(Http.Header(t"X-Request-Id", t"r1")))

      test(m"omitting a required header parameter is rejected"):
        demilitarize(refs.items.get(limit = 5)).length
      . assert(_ > 0)

      test(m"HEAD is a verb"):
        refs.items.head(`X-Request-Id` = t"r1").request.method
      . assert(_ == Http.Head)

      test(m"a referenced response schema types the call"):
        given Http.Backend = Recorder(() => ok(itemJson))
        refs.items(7).get.call[Item]()
      . assert(_ == Item(7L, t"spoon"))

      test(m"a referenced request body is accepted, and 201 is preferred to 202"):
        val recorder = Recorder(() => Http.Response(Http.Created, contentType = media"application/json")(itemJson))
        given Http.Backend = recorder
        refs.items.post(NewItem(t"spoon"), `X-Request-Id` = t"r1").call[Item]()
        recorder.lastHeaders.filter(_.key.lower == t"x-request-id").map(_.value)
      . assert(_ == List(t"r1"))

      test(m"a type that does not conform to a referenced schema is rejected"):
        demilitarize:
          given Http.Backend = Recorder(() => ok(itemJson))
          refs.items(7).get.call[Note]()
        . length
      . assert(_ > 0)

    suite(m"media types construe their carriers"):
      val refs = Api(cp"/openapi/local/refstore.json", base = url"https://ref.example.com")
      val itemJson = t"""{"id": 7, "name": "spoon"}"""

      val problemJson = t"""{"message": "no spoon"}"""

      def problem(status: Http.Status): Http.Response =
        Http.Response(status, contentType = media"application/json")(problemJson)

      test(m"a declared 404 raises Api.NotFound, its payload a record of the error schema"):
        given Http.Backend = Recorder(() => problem(Http.NotFound))

        refs.items(7).get.attempt[Item]() match
          case Attempt.Failure(Api.NotFound(problem)) => problem.message
          case _                                      => t"?"
      . assert(_ == t"no spoon")

      test(m"a status the default response covers raises Api.OtherError with the status"):
        given Http.Backend = Recorder(() => problem(Http.InternalServerError))

        refs.items(7).get.attempt[Item]() match
          case Attempt.Failure(Api.OtherError(status, problem)) => (status, problem.message)
          case _                                                => (Http.Ok, t"?")
      . assert(_ == (Http.InternalServerError, t"no spoon"))

      test(m"the declared errors are matched exhaustively as a union"):
        given Http.Backend = Recorder(() => problem(Http.NotFound))
        val outcome = refs.items(7).get.attempt[Item]()

        outcome.recover:
          case Api.NotFound(problem)      => Item(0L, problem.message)
          case Api.OtherError(_, problem) => Item(1L, problem.message)
      . assert(_ == Item(0L, t"no spoon"))

      test(m"an operation declaring no errors raises only Api.Violation"):
        given Http.Backend = Recorder(() => Http.Response(Http.Conflict)(t"busy"))
        capture[Api.Violation](refs.items(7).label.get.call()).status
      . assert(_ == Http.Conflict)

      test(m"a case for an undeclared error does not compile"):
        demilitarize:
          given Http.Backend = Recorder(() => problem(Http.NotFound))

          refs.items(7).get.attempt[Item]() match
            case Attempt.Failure(Api.Conflict(problem)) => t"?"
            case _                                      => t"?"
        . length
      . assert(_ > 0)

      test(m"a path substitution is percent-encoded"):
        val recorder = Recorder(() => Http.Response(Http.Ok, contentType = media"text/plain")(t"x"))
        given Http.Backend = recorder
        refs.items(7).label.get.call()
        recorder.lastUrl
      . assert(_ == t"https://ref.example.com/v2/items/7/label?token=q-1")

      test(m"a bare call() on a JSON endpoint yields the Json"):
        given Http.Backend = Recorder(() => ok(itemJson))
        refs.items(7).get.call().as[Item]
      . assert(_ == Item(7L, t"spoon"))

      test(m"a text/plain response is construed as Text"):
        val recorder = Recorder(() => Http.Response(Http.Ok, contentType = media"text/plain")(t"Spoon"))
        given Http.Backend = recorder
        val label: Text = refs.items(7).label.get.call()
        (label, recorder.lastHeaders.filter(_.key == t"accept").map(_.value))
      . assert(_ == (t"Spoon", List(t"text/plain")))

      test(m"an image/png response is construed as a Raster in Png"):
        val png = cp"/openapi/local/pixel.png".read[Data]
        given Http.Backend = Recorder(() => Http.Response(Http.Ok, contentType = media"image/png")(png))
        val icon: Raster in Png = refs.items(7).icon.get.call()
        icon.width
      . assert(_ == 1)

      test(m"a text response is decoded with the charset its content-type names"):
        val latin1: Data = Array.unsafeFrozen("café".getBytes("ISO-8859-1").nn)

        given Http.Backend =
          Recorder(() => Http.Response(Http.Ok, contentType = media"text/plain"(charset = "ISO-8859-1"))(latin1))

        val label: Text = refs.items(7).label.get.call()
        label
      . assert(_ == t"café")

      test(m"a multipart request body is written between its boundary"):
        val recorder = Recorder(() => Http.Response(Http.Created, contentType = media"application/json")(itemJson))
        given Http.Backend = recorder
        val file = Part(Multipart.Disposition.FormData, Map(), t"file", t"a.txt", Chain(t"hello".in[Data]))
        refs.items(7).attachments.post(Multipart(Chain(file), t"b0")).call()
        val contentType = recorder.lastHeaders.filter(_.key == t"content-type").map(_.value)
        (contentType, recorder.lastBody.let(_.utf8))
      . assert: result =>
          result(0) == List(t"multipart/form-data; boundary=b0")
          && result(1) == t"--b0\r\nContent-Disposition: form-data; name=\"file\"; filename=\"a.txt\"\r\n\r\nhello\r\n--b0--\r\n"

      test(m"a multipart response is split at the boundary its content-type names"):
        val body = Multipart(Chain(Part(Multipart.Disposition.FormData, Map(), t"a", Unset, Chain(t"1".in[Data])),
          Part(Multipart.Disposition.FormData, Map(), t"b", Unset, Chain(t"2".in[Data]))), t"b1")
        val bytes: Data = body.source[Data].memoize

        given Http.Backend =
          Recorder(() => Http.Response(Http.Ok, contentType = media"multipart/mixed"(boundary = "b1"))(bytes))

        val received: Multipart = refs.items(7).bundle.get.call()
        received.at(t"b").let(_.source[Data].memoize.utf8)
      . assert(_ == t"2")

      test(m"an application/zip response is construed as a Zipfile"):
        val entry = Zip.Entry(t"a.txt".as[Path on Zip], t"hello".in[Data])
        val bytes: Data = Zipfile(List(entry), Unset, Unset).source[Data].memoize
        given Http.Backend = Recorder(() => Http.Response(Http.Ok, contentType = media"application/zip")(bytes))
        val archive: Zipfile = refs.items(7).archive.get.call()
        archive.entries.map(_.ref.encode)
      . assert(_ == List(t"a.txt"))

      test(m"an application/x-tar response is construed as a Tarfile"):
        val file = Tar.Entry.File(path = t"hello.txt".as[Relative on Tar], mode = UnixMode(),
            user = UnixUser(0), group = UnixGroup(0), mtime = 0.bits.u32,
            data = Archive.Body(t"hello".in[Data]))

        val bytes: Data = Tarfile(List(file), LongNameFormat.Pax).source[Data].memoize
        val tar = t"application/x-tar".as[MediaType]
        given Http.Backend = Recorder(() => Http.Response(Http.Ok, contentType = tar)(bytes))
        val backup: Tarfile = refs.items(7).backup.get.call()

        backup.entries.map:
          case file: Tar.Entry.File => file.path.show
          case _                    => t"?"
      . assert(_ == List(t"hello.txt"))

      test(m"an application/pdf response is construed as a PdfFile"):
        val bytes: Data = pdfDocument(t"<< /Type /Catalog >>")
        given Http.Backend = Recorder(() => Http.Response(Http.Ok, contentType = media"application/pdf")(bytes))
        val manual: PdfFile = refs.items(7).manual.get.call()
        manual.open[Pdf]()(pdf.version.major)
      . assert(_ == 1)

      test(m"an application/x-pem-file response is construed as a Pem"):
        val armored = t"-----BEGIN CERTIFICATE-----\nAAEC\n-----END CERTIFICATE-----\n"
        val pemFile = t"application/x-pem-file".as[MediaType]
        given Http.Backend = Recorder(() => Http.Response(Http.Ok, contentType = pemFile)(armored))
        val certificate: Pem = refs.items(7).certificate.get.call()
        (certificate.label, certificate.data.length)
      . assert(_ == (Pem.Label.Certificate, 3))

      test(m"a response nothing construes is the raw Http.Response"):
        given Http.Backend = Recorder(() => Http.Response(Http.Ok)(t"a: 1"))
        val raw: Http.Response = refs.items(7).notes.get.call()
        raw.status
      . assert(_ == Http.Ok)

      test(m"a form-encoded request body takes a Query"):
        val recorder = Recorder(() => ok(t"[]"))
        given Http.Backend = recorder
        refs.lookup(Query(List(t"q" -> t"spoon"))).call[List[Item]]()
        (recorder.lastHeaders.filter(_.key == t"content-type").map(_.value), recorder.lastBody.present)
      . assert(_ == (List(t"application/x-www-form-urlencoded"), true))

      test(m"record() reads a JSON object response as a schema-typed record"):
        given Http.Backend = Recorder(() => ok(petJson))
        val pet = api.pets(42).get.record()
        (pet.id, pet.name, pet.tag)
      . assert(_ == (42, t"Milo", t"cat"))

      test(m"record() reads an array response as a list of records"):
        given Http.Backend = Recorder(() => ok(petsJson))
        api.pets.get(limit = 10).record().map(_.name)
      . assert(_ == List(t"Ada", t"Bea"))

      test(m"record() reads an int64 property as a Long"):
        given Http.Backend = Recorder(() => ok(itemJson))
        val id: Long = refs.items(7).get.record().id
        id
      . assert(_ == 7L)

      test(m"tuple() reads the response as a named tuple"):
        given Http.Backend = Recorder(() => ok(petJson))
        api.pets(42).get.tuple().name
      . assert(_ == t"Milo")

      test(m"record() is refused for a response that is not JSON"):
        demilitarize:
          given Http.Backend = Recorder(() => Http.Response(Http.Ok)(t"Spoon"))
          refs.items(7).label.get.record()
        . length
      . assert(_ > 0)

    suite(m"security schemes"):
      val refs = Api(cp"/openapi/local/refstore.json", base = url"https://ref.example.com")

      test(m"an API key in the query is sent as a query parameter"):
        refs.items(7).get.request.query.values
      . assert(_ == List(t"token" -> t"q-1"))

      test(m"an operation whose security is empty needs no credential"):
        refs.items.get(`X-Request-Id` = t"r1").request.query.values
      . assert(_ == List())

      test(m"an API key in a cookie is sent in the cookie header"):
        val recorder = Recorder(() => Http.Response(Http.Ok)(t"a: 1"))
        given Http.Backend = recorder
        refs.items(7).notes.get.call()
        recorder.lastHeaders.filter(_.key == t"cookie").map(_.value)
      . assert(_ == List(t"session=s-1"))

    suite(m"construables"):
      test(m"a JSON endpoint cannot be read as a Raster"):
        demilitarize:
          given Http.Backend = Recorder(() => ok(itemJson))
          val icon: Raster in Png = refs.items(7).get.call()
        . length
      . assert(_ > 0)

    suite(m"the spec decides the wire format (Api over Json / over Xml)"):
      val xmlApi = Api(cp"/openapi/local/xmlstore.json")
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
          case Api.Body.Content(media, _) => media.base == media"application/xml"
          case _                          => false
      . assert(_ == true)

      test(m"an XML POST sends an XML body and content-type"):
        val recorder = Recorder(() => Http.Response(Http.Created, contentType = media"application/xml")(noteXml))
        given Http.Backend = recorder
        xmlApi.notes.post(NewNote(t"hi")).call[Note]()
        (recorder.lastHeaders.filter(_.key == t"content-type").map(_.value), recorder.lastBody.present)
      . assert(_ == (List(t"application/xml; charset=UTF-8"), true))
