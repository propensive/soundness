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

import charDecoders.utf8Decoder
import charEncoders.utf8Encoder
import classloaders.threadContextClassloader
import construables.{jsonConstruable, octetStreamConstruable, pngConstruable}
import errorDiagnostics.stackTracesDiagnostics
import formatting.compactJsonFormatting
import internetAccess.online
import logging.silentLogging
import postables.jsonPostable
import strategies.throwUnsafely
import textSanitizers.skipSanitizer

// Mirror records for the Swagger Petstore, named as its component schemas are
object Swagger:
  case class Pet
    ( id:        Optional[Long] = Unset,
      name:      Text,
      photoUrls: List[Text],
      status:    Optional[Text] = Unset )

// The typed client against real specifications: navigation, parameters, bodies and responses
// are checked, at compile time, against documents as their vendors publish them.
object CorpusApiTests extends Suite(m"OpenAPI corpus client tests"):
  def run(): Unit =
    def ok(body: Text): Http.Response =
      Http.Response(Http.Ok, contentType = media"application/json")(body)

    suite(m"Swagger Petstore v3"):
      val api = Api(cp"/openapi/swagger/petstore3.json", base = url"https://petstore3.swagger.io")

      val petJson =
        t"""{"id": 42, "name": "Milo", "photoUrls": ["a"], "status": "available",
             "tags": [{"id": 1, "name": "cat"}]}"""

      test(m"the relative server URL extends the base"):
        api.request.base.show
      . assert(_ == t"https://petstore3.swagger.io/api/v3")

      test(m"an int64 path parameter takes a Long"):
        api.pet(42L).get.request.substitutions
      . assert(_ == Map(t"petId" -> t"42"))

      test(m"call[Pet]() decodes the JSON alternative of a JSON-or-XML response"):
        given Http.Backend = Recorder(() => ok(petJson))
        api.pet(42L).get.call[Swagger.Pet]().name
      . assert(_ == t"Milo")

      test(m"record() types the pet by its schema"):
        given Http.Backend = Recorder(() => ok(petJson))
        val pet = api.pet(42L).get.record()
        (pet.name, pet.photoUrls)
      . assert(_ == (t"Milo", List(t"a")))

      test(m"a required query parameter feeds findByStatus"):
        api.pet.findByStatus.get(status = t"available").request.query.values
      . assert(_ == List(t"status" -> t"available"))

      test(m"findByStatus decodes a list of pets"):
        given Http.Backend = Recorder(() => ok(t"[$petJson]"))
        api.pet.findByStatus.get(status = t"available").call[List[Swagger.Pet]]().map(_.name)
      . assert(_ == List(t"Milo"))

      test(m"the inventory, a map schema, is construed as Json"):
        given Http.Backend = Recorder(() => ok(t"""{"sold": 1}"""))
        val inventory: Json = api.store.inventory.get.call()
        inventory(t"sold").as[Int]
      . assert(_ == 1)

      test(m"an optional header parameter is sent"):
        val recorder = Recorder(() => Http.Response(Http.Ok)())
        given Http.Backend = recorder
        api.pet(42L).delete(api_key = t"secret").call()
        recorder.lastHeaders.filter(_.key.lower == t"api_key").map(_.value)
      . assert(_ == List(t"secret"))

      test(m"an octet-stream upload takes Data"):
        val png = cp"/openapi/local/pixel.png".read[Data]
        val recorder = Recorder(() => ok(t"{}"))
        given Http.Backend = recorder
        api.pet(42L).uploadImage.post(png, additionalMetadata = t"icon").call()
        (recorder.lastMethod, recorder.lastHeaders.filter(_.key == t"content-type").map(_.value))
      . assert(_ == (Http.Post, List(t"application/octet-stream")))

      test(m"a declared 404 without a body raises Api.NotFound[Unit]"):
        given Http.Backend = Recorder(() => Http.Response(Http.NotFound)())

        api.pet(42L).get.attempt[Swagger.Pet]() match
          case Attempt.Failure(Api.NotFound(())) => true
          case _                                 => false
      . assert(_ == true)

      test(m"a query parameter of the wrong type is rejected"):
        demilitarize(api.pet.findByStatus.get(status = 1)).length
      . assert(_ > 0)

      test(m"omitting the required status is rejected"):
        demilitarize(api.pet.findByStatus.get).length
      . assert(_ > 0)

    suite(m"Redocly Museum (YAML, OpenAPI 3.1)"):
      val api = Api(cp"/openapi/redocly/museum.yaml")

      test(m"the server URL is read from YAML"):
        api.request.base.show
      . assert(_ == t"https://redocly.com/_mock/docs/openapi/museum-api")

      test(m"referenced query parameters are recognised"):
        api.`museum-hours`.get(startDate = t"2024-01-01", limit = 5).request.query.values
      . assert(_ == List(t"startDate" -> t"2024-01-01", t"limit" -> t"5"))

      test(m"a PNG ticket code is construed as a Raster"):
        val png = cp"/openapi/local/pixel.png".read[Data]
        given Http.Backend = Recorder(() => Http.Response(Http.Ok, contentType = media"image/png")(png))
        val code: Raster in Png = api.tickets(t"a1").qr.get.call()
        code.width
      . assert(_ == 1)

      test(m"special events decode as JSON"):
        given Http.Backend = Recorder(() => ok(t"[]"))
        api.`special-events`.get.call().as[List[Json]]
      . assert(_ == List())

    suite(m"Kubernetes RBAC"):
      val api = Api(cp"/openapi/kubernetes/rbac.json", base = url"https://cluster.example")

      test(m"path-level and operation parameters combine"):
        val roles = api.apis.`rbac.authorization.k8s.io`.v1.namespaces(t"default").roles
        val request = roles.get(limit = 5, pretty = t"true").request
        (request.path, request.substitutions, request.query.values)
      . assert: request =>
          request(0) == t"/apis/rbac.authorization.k8s.io/v1/namespaces/{namespace}/roles"
          && request(1) == Map(t"namespace" -> t"default")
          && request(2) == List(t"limit" -> t"5", t"pretty" -> t"true")

      test(m"JSON is preferred among the response media types"):
        val typed: Api.Response over Json = api.apis.`rbac.authorization.k8s.io`.v1.get
        typed.request.accept
      . assert(_ == media"application/json")

    suite(m"Discord (OpenAPI 3.1)"):
      val api = Api(cp"/openapi/discord/openapi.json")

      test(m"an @me segment navigates"):
        api.users.`@me`.get.request.path
      . assert(_ == t"/users/@me")

      test(m"messages take a limit"):
        api.channels(t"1").messages.get(limit = 10).request.query.values
      . assert(_ == List(t"limit" -> t"10"))

      test(m"an undeclared path is rejected"):
        demilitarize(api.channels(t"1").unicorns).length
      . assert(_ > 0)

    suite(m"Stripe"):
      val api = Api(cp"/openapi/stripe/spec3.json")

      test(m"customers are listed with a limit"):
        val request = api.v1.customers.get(limit = 3).request
        (request.path, request.query.values)
      . assert(_ == (t"/v1/customers", List(t"limit" -> t"3")))

      test(m"Stripe's default error response raises OtherError with a record payload"):
        given Http.Backend =
          Recorder(() => Http.Response(Http.BadRequest, contentType = media"application/json")(t"""{"error": {"message": "no"}}"""))

        api.v1.customers(t"cus_1").get.attempt[Json]() match
          case Attempt.Failure(Api.OtherError(status, problem)) => status
          case _                                                => Http.Ok
      . assert(_ == Http.BadRequest)

      test(m"a customer is fetched by id"):
        api.v1.customers(t"cus_1").get.request.substitutions
      . assert(_ == Map(t"customer" -> t"cus_1"))

    suite(m"Twilio"):
      val api = Api(cp"/openapi/twilio/accounts_v1.json")

      test(m"an int64 query parameter takes a Long"):
        api.v1.Credentials.AWS.get(PageSize = 5L).request.query.values
      . assert(_ == List(t"PageSize" -> t"5"))
