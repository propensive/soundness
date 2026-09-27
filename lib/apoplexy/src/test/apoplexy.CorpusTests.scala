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
import apoplexy.OpenApi.*

import charDecoders.utf8Decoder
import classloaders.threadContextClassloader
import errorDiagnostics.emptyDiagnostics
import strategies.throwUnsafely
import textSanitizers.skipSanitizer

// The offline corpus: every real-world document in `res/test/openapi` loads, with the shape its
// upstream has, and the JSON and YAML forms of a document decode to the same model.
object CorpusTests extends Suite(m"OpenAPI corpus tests"):
  case class Fixture(path: Text, version: Text, paths: Int, operation: Optional[Text], schemas: Int)

  val fixtures: List[Fixture] = List
    ( Fixture(t"/openapi/oai/v3.0/petstore.json", t"3.0.0", 2, t"listPets", 3),
      Fixture(t"/openapi/oai/v3.0/petstore-expanded.json", t"3.0.0", 2, t"findPets", 3),
      Fixture(t"/openapi/oai/v3.0/uspto.json", t"3.0.1", 3, t"list-data-sets", 1),
      Fixture(t"/openapi/oai/v3.0/link-example.json", t"3.0.0", 6, t"getUserByName", 3),
      Fixture(t"/openapi/oai/v3.0/callback-example.json", t"3.0.0", 1, Unset, 0),
      Fixture(t"/openapi/oai/v3.0/api-with-examples.json", t"3.0.0", 2, t"listVersionsv2", 0),
      Fixture(t"/openapi/oai/v3.1/tictactoe.json", t"3.1.0", 2, t"get-board", 6),
      Fixture(t"/openapi/oai/v3.1/webhook-example.json", t"3.1.0", 0, Unset, 1),
      Fixture(t"/openapi/oai/v3.1/non-oauth-scopes.json", t"3.1.0", 1, Unset, 0),
      Fixture(t"/openapi/swagger/petstore3.json", t"3.0.4", 13, t"getPetById", 6),
      Fixture(t"/openapi/redocly/museum.yaml", t"3.1.0", 5, t"getMuseumHours", 22),
      Fixture(t"/openapi/twilio/accounts_v1.json", t"3.0.1", 11, t"UpdateAuthTokenPromotion", 10),
      Fixture(t"/openapi/twilio/lookups_v2.json", t"3.0.1", 5, t"FetchPhoneNumber", 25),
      Fixture(t"/openapi/kubernetes/version.json", t"3.0.0", 1, t"getCodeVersion", 1),
      Fixture(t"/openapi/kubernetes/rbac.json", t"3.0.0", 21, t"getRbacAuthorizationV1APIResources", 31),
      Fixture(t"/openapi/discord/openapi.json", t"3.1.0", 153, t"get_my_application", 538),
      Fixture(t"/openapi/box/openapi.json", t"3.0.2", 187, t"get_authorize", 310),
      Fixture(t"/openapi/stripe/spec3.json", t"3.0.0", 431, t"GetAccount", 1538) )

  // The documents published in both forms. Upstream's `tictactoe` YAML declares a header
  // parameter and a 202 response its JSON does not, so that pair is not compared.
  val pairs: List[Text] = List
    ( t"/openapi/oai/v3.0/petstore", t"/openapi/oai/v3.0/petstore-expanded", t"/openapi/oai/v3.0/uspto",
      t"/openapi/oai/v3.0/link-example", t"/openapi/oai/v3.0/callback-example",
      t"/openapi/oai/v3.0/api-with-examples", t"/openapi/oai/v3.1/webhook-example",
      t"/openapi/oai/v3.1/non-oauth-scopes", t"/openapi/twilio/accounts_v1" )

  // The fixtures are named by `Text`, so they are read through the classloader directly rather
  // than the literal-only `cp""` interpolator
  def text(path: Text): Text =
    val stream = Optional(getClass.getResourceAsStream(path.s)).or:
      panic(m"the fixture $path is not on the classpath")

    scala.io.Source.fromInputStream(stream).mkString.tt

  def load(path: Text): OpenApi = text(path).read[OpenApi]

  def operations(doc: OpenApi): List[OpenApi.Operation] =
    doc.paths.values.flatMap(_.operations.values).to[List]

  def run(): Unit =
    suite(m"every document loads with its upstream shape"):
      fixtures.each: fixture =>
        val name = fixture.path.skip(t"/openapi/".length)

        test(m"$name has the expected version, paths and schemas"):
          val doc = load(fixture.path)
          (doc.openapi, doc.paths.size, doc.components.let(_.schemas.size).or(0))
        . assert(_ == (fixture.version, fixture.paths, fixture.schemas))

        fixture.operation.let: operation =>
          test(m"$name declares the operation $operation"):
            operations(load(fixture.path)).exists(_.operationId == operation)
          . assert(_ == true)

    suite(m"the JSON and YAML forms decode to the same model"):
      pairs.each: pair =>
        val name = pair.skip(t"/openapi/".length)

        test(m"$name is the same document in JSON and YAML"):
          load(t"$pair.yaml")
        . assert(_ == load(t"$pair.json"))

    suite(m"constructs real documents use"):
      val petstore = load(t"/openapi/swagger/petstore3.json")
      val museum = load(t"/openapi/redocly/museum.yaml")
      val kubernetes = load(t"/openapi/kubernetes/rbac.json")
      val discord = load(t"/openapi/discord/openapi.json")
      val stripe = load(t"/openapi/stripe/spec3.json")

      test(m"an int64 property keeps its format"):
        petstore.components.let(_.schemas(t"Pet")) match
          case schema: JsonSchema.Object => schema.properties(t"id")
          case _                         => Unset
      . assert(_ == JsonSchema.Integer(format = JsonSchema.Format.Int64))

      test(m"a binary upload body is a string of format binary"):
        given OpenApi = petstore

        petstore.paths(t"/pet/{petId}/uploadImage").let(_.post).let(_.requestBody).let(_())
        . let(_.content(t"application/octet-stream")).let(_.schema)
      . assert(_ == JsonSchema.String(format = JsonSchema.Format.Binary))

      test(m"a referenced query parameter resolves in a YAML 3.1 document"):
        given OpenApi = museum

        museum.paths(t"/museum-hours").let(_.get).let(_.parameters.prim).let(_()).let(_.`in`)
      . assert(_ == OpenApi.Parameter.In.Query)

      test(m"a map schema keeps the schema of its values"):
        kubernetes.components.let(_.schemas(t"io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta"))
        . let:
            case schema: JsonSchema.Object => schema.properties(t"labels")
            case _                         => Unset
        . let:
            case schema: JsonSchema.Object => schema.additionalSchema
            case _                         => Unset
      . assert(_ == JsonSchema.String())

      test(m"a nullable type array reads as an optional property"):
        discord.components.let(_.schemas.values.exists(nullableString)).or(false)
      . assert(_ == true)

      test(m"a document with 1,538 schemas resolves a reference"):
        given OpenApi = stripe
        JsonSchema.Ref(t"#/components/schemas/customer".as[JsonPointer])()
      . assert:
          case _: JsonSchema.Object => true
          case _                    => false

      test(m"a document with no paths still loads its webhooks' schemas"):
        load(t"/openapi/oai/v3.1/webhook-example.json").components.let(_.schemas.size)
      . assert(_ == 1)

    suite(m"documents which are not OpenAPI 3"):
      test(m"a truncated document is malformed"):
        capture[OpenApi.Error](t"""{"openapi": "3.0.0", "info": {""".read[OpenApi]).reason
      . assert(_ == OpenApi.Error.Reason.Malformed)

      test(m"a Swagger 2 parameter location is rejected"):
        capture[OpenApi.Error]:
          t"""{"openapi": "3.0.0", "info": {"title": "x", "version": "1"}, "paths": {"/a": {"get":
              {"parameters": [{"name": "b", "in": "body"}], "responses": {}}}}}""".read[OpenApi]
        . reason
      . assert(_ == OpenApi.Error.Reason.BadParameterLocation(t"body"))

  def nullableString(schema: JsonSchema): Boolean = schema match
    case schema: JsonSchema.Object =>
      schema.properties.values.exists:
        case property: JsonSchema.String => property.optional
        case _                           => false

    case _ =>
      false
