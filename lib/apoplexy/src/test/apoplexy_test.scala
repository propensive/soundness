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

import proscenium.compat.*
import apoplexy.OpenApi.*

import errorDiagnostics.emptyDiagnostics
import strategies.throwUnsafely

object Tests extends Suite(m"OpenAPI tests"):
  val jsonSpec: Text =
    """{
      "openapi": "3.0.3",
      "info": { "title": "Petstore", "version": "1.0.0" },
      "paths": {
        "/pets": {
          "get": {
            "operationId": "listPets",
            "responses": {
              "200": {
                "description": "a list of pets",
                "content": {
                  "application/json": {
                    "schema": { "type": "array", "items": { "$ref": "#/components/schemas/Pet" } }
                  }
                }
              }
            }
          }
        },
        "/pets/{id}": {
          "get": {
            "operationId": "getPet",
            "parameters": [
              { "name": "id", "in": "path", "required": true, "schema": { "type": "integer" } }
            ],
            "responses": {
              "200": {
                "description": "a pet",
                "content": {
                  "application/json": { "schema": { "$ref": "#/components/schemas/Pet" } }
                }
              }
            }
          }
        }
      },
      "components": {
        "schemas": {
          "Pet": {
            "type": "object",
            "required": ["id", "name"],
            "properties": {
              "id": { "type": "integer" },
              "name": { "type": "string" },
              "owner": { "$ref": "#/components/schemas/Owner" }
            }
          },
          "Owner": {
            "type": "object",
            "properties": {
              "name": { "type": "string" },
              "pet": { "$ref": "#/components/schemas/Pet" }
            }
          }
        }
      }
    }""".tt

  val yamlSpec: Text =
    """openapi: "3.0.3"
info:
  title: Petstore
  version: "1.0.0"
paths:
  /pets:
    get:
      operationId: listPets
      responses:
        "200":
          description: a list of pets
          content:
            application/json:
              schema:
                type: array
                items:
                  $ref: "#/components/schemas/Pet"
  /pets/{id}:
    get:
      operationId: getPet
      parameters:
        - name: id
          in: path
          required: true
          schema:
            type: integer
      responses:
        "200":
          description: a pet
          content:
            application/json:
              schema:
                $ref: "#/components/schemas/Pet"
components:
  schemas:
    Pet:
      type: object
      required:
        - id
        - name
      properties:
        id:
          type: integer
        name:
          type: string
        owner:
          $ref: "#/components/schemas/Owner"
    Owner:
      type: object
      properties:
        name:
          type: string
        pet:
          $ref: "#/components/schemas/Pet"
""".tt

  def run(): Unit =
    val fromJson = jsonSpec.read[OpenApi]
    val fromYaml = yamlSpec.read[OpenApi]

    test(m"JSON and YAML specs decode to the same model")(fromYaml).assert(_ == fromJson)

    test(m"the operationId of GET /pets is read"):
      fromJson.paths.at(t"/pets").vouch.get.vouch.operationId.vouch
    .assert(_ == t"listPets")

    test(m"the path parameter location is decoded as Path"):
      fromJson.paths.at(t"/pets/{id}").vouch.get.vouch.parameters.head.in
    .assert(_ == OpenApi.Parameter.In.Path)

    test(m"a reference property is kept lazy"):
      fromJson.components.vouch.schemas.at(t"Pet").vouch
    .assert:
      case JsonSchema.Object(_, properties, _, _, _, _, _) =>
        properties.at(t"owner").vouch match
          case JsonSchema.Ref(pointer, _, _) => pointer.encode == t"#/components/schemas/Owner"
          case _                             => false
      case _ => false

    test(m"a component reference resolves to its schema"):
      given OpenApi = fromJson
      JsonSchema.Ref(t"#/components/schemas/Pet".as[JsonPointer])()
    .assert:
      case _: JsonSchema.Object => true
      case _                    => false

    test(m"a cyclic reference resolves one hop without looping"):
      given OpenApi = fromJson
      JsonSchema.Ref(t"#/components/schemas/Owner".as[JsonPointer])()
    .assert:
      case _: JsonSchema.Object => true
      case _                    => false

    test(m"an unsupported OpenAPI version is rejected"):
      capture[OpenApiError]:
        """{"openapi": "2.0", "info": {"title": "x", "version": "1"}}""".tt.read[OpenApi]
    .assert(_.reason == OpenApiError.Reason.UnsupportedVersion(t"2.0"))

    ApiTests()
