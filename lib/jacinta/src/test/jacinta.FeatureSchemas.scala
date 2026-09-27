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
package jacinta

import soundness.*

import codepages.utf8Codepage
import strategies.throwUnsafely

// Small hand-written schemas, each exercising one JSON Schema feature the provider handles,
// compiled before the tests so that the `record` macro can evaluate them.

// A `$ref` to a definition, under both `definitions` (draft 7) and `$defs` (2019-09 onwards)
object DefinitionsSchema extends Json.Provider(t"""{
  "type": "object",
  "required": ["home"],
  "properties": {
    "home": { "$$ref": "#/definitions/address" },
    "work": { "$$ref": "#/$$defs/address2" }
  },
  "definitions": {
    "address": {
      "type": "object",
      "required": ["street"],
      "properties": { "street": { "type": "string" }, "number": { "type": "integer" } }
    }
  },
  "$$defs": {
    "address2": { "type": "object", "properties": { "building": { "type": "string" } } }
  }
}""".read[Json])

// A recursive definition: a tree whose children are trees. The recursion reads as raw JSON.
object RecursiveSchema extends Json.Provider(t"""{
  "type": "object",
  "required": ["label"],
  "properties": {
    "label": { "type": "string" },
    "children": { "type": "array", "items": { "$$ref": "#" } }
  }
}""".read[Json])

// An `allOf` merging a referenced base object with the node's own properties
object AllOfSchema extends Json.Provider(t"""{
  "allOf": [
    { "$$ref": "#/definitions/named" },
    { "type": "object", "required": ["age"], "properties": { "age": { "type": "integer" } } }
  ],
  "properties": { "email": { "type": "string", "format": "email" } },
  "definitions": {
    "named": {
      "type": "object",
      "required": ["name"],
      "properties": { "name": { "type": "string" } }
    }
  }
}""".read[Json])

// Nullable and union types, `enum` and `const`, with and without a `type`
object VariantsSchema extends Json.Provider(t"""{
  "type": "object",
  "required": ["nickname", "level", "kind", "mixed", "either", "choice", "stringy"],
  "properties": {
    "nickname": { "type": ["string", "null"] },
    "level": { "enum": ["low", "high"] },
    "kind": { "const": "widget" },
    "count": { "enum": [1, 2, 3] },
    "mixed": { "type": ["string", "integer"] },
    "either": { "anyOf": [ { "type": "string" }, { "type": "null" } ] },
    "choice": {
      "oneOf": [ { "enum": ["a"], "description": "A" }, { "enum": ["b"], "description": "B" } ]
    },
    "stringy": { "anyOf": [ { "enum": ["auto"] }, { "type": "string", "pattern": "^[0-9]+$$" } ] },
    "anything": true,
    "unknown": { "not": { "type": "string" } }
  }
}""".read[Json])

// Numeric and string bounds, in draft 4's boolean form and in the later numeric form
object BoundsSchema extends Json.Provider(t"""{
  "type": "object",
  "required": ["port", "ratio", "code", "positive", "below"],
  "properties": {
    "port": { "type": "integer", "minimum": 1, "maximum": 65535 },
    "ratio": { "type": "number", "minimum": 0, "maximum": 1 },
    "code": { "type": "string", "minLength": 2, "maxLength": 4 },
    "positive": { "type": "integer", "minimum": 0, "exclusiveMinimum": true },
    "below": { "type": "number", "exclusiveMaximum": 10 },
    "score": { "type": "integer", "exclusiveMinimum": 0, "exclusiveMaximum": 100 }
  }
}""".read[Json])

// Containers the provider cannot type further: dictionaries, untyped arrays, tuple-typed arrays
object ContainersSchema extends Json.Provider(t"""{
  "type": "object",
  "properties": {
    "labels": { "type": "object", "additionalProperties": { "type": "string" } },
    "byPattern": { "type": "object", "patternProperties": { "^x-": { "type": "string" } } },
    "anything": { "type": "array" },
    "pair": { "type": "array", "items": [ { "type": "string" }, { "type": "integer" } ] },
    "matrix": { "type": "array", "items": { "type": "array", "items": { "type": "number" } } },
    "people": { "type": "array", "items": { "$$ref": "#/definitions/person" } }
  },
  "definitions": {
    "person": {
      "type": "object", "required": ["name"], "properties": { "name": { "type": "string" } }
    }
  }
}""".read[Json])

// Property names which are Scala keywords, symbols, or members of `Record` itself
object AwkwardNamesSchema extends Json.Provider(t"""{
  "type": "object",
  "required": ["data", "type"],
  "properties": {
    "data": { "type": "object", "properties": { "value": { "type": "integer" } } },
    "type": { "type": "string" },
    "class": { "type": "string" },
    "content-type": { "type": "string" },
    "$$id": { "type": "string" },
    "toString": { "type": "boolean" }
  }
}""".read[Json])

// Roots the provider cannot use: an array, a bare string, and a reference it cannot follow
object ArrayRootSchema extends Json.Provider(t"""{
  "type": "array",
  "items": { "type": "object", "properties": { "name": { "type": "string" } } }
}""".read[Json])

object StringRootSchema extends Json.Provider(t"""{ "type": "string" }""".read[Json])

object ExternalRootSchema extends Json.Provider(t"""{
  "$$ref": "https://example.com/schemas/other.json"
}""".read[Json])

// A root which may be an object or a string: the provider reads the object form
object ObjectOrStringRootSchema extends Json.Provider(t"""{
  "oneOf": [
    { "type": "string" },
    { "type": "object", "required": ["name"], "properties": { "name": { "type": "string" } } }
  ]
}""".read[Json])

// Unions of different types, chosen by the value's kind, including fallible alternatives
object UnionsSchema extends Json.Provider(t"""{
  "type": "object",
  "required": ["id", "exec", "ignore"],
  "properties": {
    "id": { "type": ["string", "integer"] },
    "exec": {
      "anyOf": [
        { "type": "string" },
        {
          "type": "object",
          "required": ["command"],
          "properties": {
            "command": { "type": "string" },
            "args": { "type": "array", "items": { "type": "string" } }
          }
        }
      ]
    },
    "ignore": {
      "anyOf": [ { "type": "string" }, { "type": "array", "items": { "type": "string" } } ]
    },
    "flag": { "oneOf": [ { "type": "boolean" }, { "enum": ["auto"] } ] },
    "port": {
      "anyOf": [ { "type": "integer", "minimum": 1 }, { "type": "string", "pattern": "^[0-9]+$$" } ]
    },
    "shapes": {
      "oneOf": [
        { "type": "object", "properties": { "radius": { "type": "number" } } },
        { "type": "object", "properties": { "side": { "type": "number" } } }
      ]
    }
  }
}""".read[Json])

// Dictionaries: objects whose values one schema describes
object DictionariesSchema extends Json.Provider(t"""{
  "type": "object",
  "properties": {
    "labels": { "type": "object", "additionalProperties": { "type": "string" } },
    "scores": { "type": "object", "additionalProperties": { "type": "integer", "minimum": 0 } },
    "groups": {
      "type": "object",
      "additionalProperties": { "type": "array", "items": { "type": "string" } }
    },
    "people": { "type": "object", "additionalProperties": { "$$ref": "#/definitions/person" } },
    "byPattern": {
      "type": "object",
      "patternProperties": { "^x-": { "type": "string" }, "^y-": { "type": "string" } }
    },
    "mixedPatterns": {
      "type": "object",
      "patternProperties": { "^x-": { "type": "string" }, "^n-": { "type": "integer" } }
    },
    "open": { "type": "object", "additionalProperties": true },
    "nested": {
      "type": "object",
      "additionalProperties": { "type": "object", "additionalProperties": { "type": "integer" } }
    }
  },
  "definitions": {
    "person": {
      "type": "object", "required": ["name"], "properties": { "name": { "type": "string" } }
    }
  }
}""".read[Json])
