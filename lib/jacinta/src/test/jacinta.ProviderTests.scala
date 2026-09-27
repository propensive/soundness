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
import emailAddressInterfaces.soundnessEmailAddress
import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely
import urlInterfaces.soundnessUrl

// Records over real-world JSON Schema documents (`ProviderSchemas.scala`) and over small schemas
// each exercising one feature (`FeatureSchemas.scala`), read from valid and invalid documents.
object ProviderTests extends Suite(m"JSON Provider tests"):
  import Json.Provider.Error.Reason.*

  def json(text: Text): Json = text.read[Json]

  def run(): Unit =
    suite(m"json-schema.org: address"):
      val full = json(t"""{
        "post-office-box": "PO Box 1", "extended-address": "Suite 2",
        "street-address": "3 High Street", "locality": "Oxford", "region": "Oxfordshire",
        "postal-code": "OX1 1AA", "country-name": "United Kingdom"
      }""")

      val minimal = json(t"""{"locality": "Oxford", "region": "Oxon", "country-name": "UK"}""")

      test(m"a required field reads as Text"):
        AddressSchema.record(full).locality
      . assert(_ == t"Oxford")

      test(m"a hyphenated property is read with a backticked name"):
        AddressSchema.record(full).`street-address`
      . assert(_ == (t"3 High Street": Optional[Text]))

      test(m"an optional field is Unset when omitted"):
        AddressSchema.record(minimal).`postal-code`
      . assert(_ == Unset)

      test(m"a missing required field fails when read"):
        capture[Json.Error](AddressSchema.record(json(t"""{"region": "Oxon"}""")).locality).reason
      . assert(_ == Json.Error.Reason.Absent)

      test(m"a required field of the wrong type fails when read"):
        capture[Json.Error](AddressSchema.record(json(t"""{"locality": 7}""")).locality).reason
      . assert:
          case Json.Error.Reason.NotType(_, _) => true
          case _                              => false

      test(m"a tuple reads every field at once"):
        val tuple = AddressSchema.tuple(minimal)
        (tuple.locality, tuple.region, tuple.`postal-code`)
      . assert(_ == (t"Oxford", t"Oxon", Unset))

    suite(m"json-schema.org: geographical location"):
      test(m"coordinates within their bounds read as Double"):
        val record = GeographicalLocationSchema.record(json(t"""{"latitude": 51.75, "longitude": -1.26}"""))
        (record.latitude, record.longitude)
      . assert(_ == (51.75, -1.26))

      test(m"a bound is inclusive"):
        GeographicalLocationSchema.record(json(t"""{"latitude": -90, "longitude": 180}""")).latitude
      . assert(_ == -90.0)

      test(m"a latitude above 90 is out of range"):
        capture[Json.Provider.Error]:
          GeographicalLocationSchema.record(json(t"""{"latitude": 91, "longitude": 0}""")).latitude
        . reason
      . assert(_ == NumberOutOfRange(91.0, -90.0, 90.0))

      test(m"an out-of-range coordinate fails a tuple as it is built"):
        capture[Json.Provider.Error]:
          GeographicalLocationSchema.tuple(json(t"""{"latitude": 0, "longitude": -181}"""))
        . reason
      . assert(_ == NumberOutOfRange(-181.0, -180.0, 180.0))

    suite(m"json-schema.org: calendar and card"):
      val event = json(t"""{
        "dtstart": "2026-10-01T09:00:00Z", "summary": "Standup", "duration": "PT15M",
        "geo": {"latitude": 51.5, "longitude": 0}
      }""")

      test(m"required and optional strings read as declared"):
        val record = CalendarSchema.record(event)
        (record.summary, record.duration, record.location)
      . assert(_ == (t"Standup", (t"PT15M": Optional[Text]), Unset))

      test(m"a property referring to another document reads as raw JSON"):
        CalendarSchema.record(event).geo.let(_(t"latitude").as[Double])
      . assert(_ == (51.5: Optional[Double]))

      val card = json(t"""{
        "familyName": "Curie", "givenName": "Marie", "additionalName": ["Salomea"],
        "honorificPrefix": ["Dr", "Prof"],
        "email": {"type": "work", "value": "marie@example.org"},
        "org": {"organizationName": "Sorbonne"}
      }""")

      test(m"an array of strings reads as a List of Text"):
        CardSchema.record(card).honorificPrefix
      . assert(_ == List(t"Dr", t"Prof"))

      test(m"an absent array reads as an empty list"):
        CardSchema.record(card).honorificSuffix
      . assert(_ == List())

      test(m"a nested object reads as a nested record"):
        CardSchema.record(card).email.let(_.value)
      . assert(_ == (t"marie@example.org": Optional[Text]))

      test(m"an optional nested object is Unset when absent"):
        CardSchema.record(card).tel.let(_.value)
      . assert(_ == Unset)

    suite(m"GeoJSON"):
      val point = json(t"""{"type": "Point", "coordinates": [-1.26, 51.75]}""")

      test(m"an enum constant reads as Text"):
        GeoJsonPointSchema.record(point).`type`
      . assert(_ == t"Point")

      test(m"a value outside the enum is rejected"):
        capture[Json.Provider.Error]:
          GeoJsonPointSchema.record(json(t"""{"type": "Polygon", "coordinates": []}""")).`type`
        . reason
      . assert(_ == NotPermitted(t"Polygon", List(t"Point")))

      test(m"an array of numbers reads as a List of Double"):
        GeoJsonPointSchema.record(point).coordinates
      . assert(_ == List(-1.26, 51.75))

      test(m"an optional bounding box is an empty list when absent"):
        GeoJsonPointSchema.record(point).bbox
      . assert(_ == List())

      val feature = json(t"""{
        "type": "Feature", "id": 7,
        "geometry": {"type": "Point", "coordinates": [0, 0]},
        "properties": {"name": "Origin"}
      }""")

      test(m"a oneOf over different objects reads as raw JSON, optional since null is allowed"):
        GeoJsonFeatureSchema.record(feature).geometry.let(_(t"type").as[Text])
      . assert(_ == (t"Point": Optional[Text]))

      test(m"an object-or-null property reads null as Unset"):
        GeoJsonFeatureSchema.record(json(t"""{"type": "Feature", "geometry": null, "properties": null}""")).properties
      . assert(_ == Unset)

      test(m"an untyped object property reads as raw JSON"):
        GeoJsonFeatureSchema.record(feature).properties.let(_(t"name").as[Text])
      . assert(_ == (t"Origin": Optional[Text]))

      test(m"a property typed number-or-string reads as a union"):
        GeoJsonFeatureSchema.record(feature).id.let:
          case number: Double => number
          case text: Text     => -1.0
      . assert(_ == (7.0: Optional[Double]))

    suite(m"SchemaStore: lerna, nyc, vsconfig, global.json"):
      val lerna = json(t"""{
        "version": "independent", "npmClient": "yarn", "packages": ["packages/*"],
        "command": {"publish": {"message": "chore(release): publish"}, "init": {"exact": true}}
      }""")

      test(m"nested optional records read their fields"):
        val record = LernaSchema.record(lerna)
        (record.command.let(_.publish.let(_.message)), record.command.let(_.init.let(_.exact)))
      . assert(_ == ((t"chore(release): publish": Optional[Text]), (true: Optional[Boolean])))

      test(m"a string-or-array property reads as raw JSON"):
        LernaSchema.record(lerna).command.let(_.version.let(_.allowBranch))
      . assert(_ == Unset)

      test(m"a boolean property with a hyphenated name"):
        NycrcSchema.record(json(t"""{"check-coverage": true, "reporter": ["text", "html"]}""")).`check-coverage`
      . assert(_ == (true: Optional[Boolean]))

      test(m"a pattern-constrained version is accepted"):
        VsconfigSchema.record(json(t"""{"version": "1.0", "components": ["Microsoft.VisualStudio.Workload.CoreEditor"]}""")).version
      . assert(_ == (t"1.0": Optional[Text]))

      test(m"a version not matching the pattern is rejected"):
        capture[Json.Provider.Error]:
          VsconfigSchema.record(json(t"""{"version": "latest", "components": []}""")).version
        . reason
      . assert:
          case PatternMismatch(t"latest", _) => true
          case _                             => false

      test(m"a component shorter than minLength is rejected"):
        capture[Json.Provider.Error]:
          VsconfigSchema.record(json(t"""{"version": "1.0", "components": [""]}""")).components
        . reason
      . assert(_ == LengthOutOfRange(t"", 1, Unset))

      val global = json(t"""{"sdk": {"version": "8.0.100", "rollForward": "latestMinor", "allowPrerelease": false}, "msbuild-sdks": {"MSBuild.Sdk.Extras": "3.0.44"}}""")

      test(m"a semantic version matches its pattern"):
        GlobalJsonSchema.record(global).sdk.let(_.version)
      . assert(_ == (t"8.0.100": Optional[Text]))

      test(m"an enum property accepts a listed value"):
        GlobalJsonSchema.record(global).sdk.let(_.rollForward)
      . assert(_ == (t"latestMinor": Optional[Text]))

      test(m"an enum property rejects an unlisted value"):
        capture[Json.Provider.Error]:
          GlobalJsonSchema.record(json(t"""{"sdk": {"rollForward": "sideways"}}""")).sdk.let(_.rollForward)
        . reason
      . assert:
          case NotPermitted(t"sideways", _) => true
          case _                            => false

      test(m"a dictionary property reads as a Map"):
        GlobalJsonSchema.record(global).`msbuild-sdks`
      . assert(_ == Map(t"MSBuild.Sdk.Extras" -> t"3.0.44"))

    suite(m"SchemaStore: nodemon, bower, lint-staged, commitlint"):
      val nodemon = json(t"""{"watch": ["src"], "ext": "ts,js", "delay": 2.5, "exec": "ts-node src/index.ts", "ignoreRoot": [".git"], "nodeArgs": ["--inspect"]}""")

      test(m"a number reads as Double"):
        NodemonSchema.record(nodemon).delay
      . assert(_ == (2.5: Optional[Double]))

      test(m"a string-or-array property reads as a union"):
        NodemonSchema.record(nodemon).exec
      . assert(_ == (t"ts-node src/index.ts": Optional[Text | List[Text]]))

      test(m"an array with untyped items reads as a List of raw JSON"):
        NodemonSchema.record(nodemon).nodeArgs.map(_.as[Text])
      . assert(_ == List(t"--inspect"))

      val bower = json(t"""{"directory": "app/components", "proxy": "http://proxy.example.com:8080", "timeout": 30000, "resolvers": ["bower-shorthand-resolver"]}""")

      test(m"a uri-formatted string reads as an HttpUrl"):
        BowerrcSchema.record(bower).proxy.let(_.show.s.startsWith("http://proxy.example.com:8080"))
      . assert(_ == (true: Optional[Boolean]))

      test(m"an invalid uri fails when read"):
        capture[Url.Error](BowerrcSchema.record(json(t"""{"proxy": "not a url"}""")).proxy)
      . assert(_ => true)

      val lintStaged = json(t"""{"$$schema": "https://json.schemastore.org/lintstagedrc.schema.json", "concurrent": false, "chunkSize": 5, "subTaskConcurrency": 2, "renderer": "verbose", "linters": {"*.js": "eslint"}}""")

      test(m"a root offering an object or a string reads the object form"):
        LintStagedSchema.record(lintStaged).`$schema`
      . assert(_ == (t"https://json.schemastore.org/lintstagedrc.schema.json": Optional[Text]))

      test(m"a number below its minimum is rejected"):
        capture[Json.Provider.Error](LintStagedSchema.record(json(t"""{"chunkSize": 0}""")).chunkSize).reason
      . assert(_ == NumberOutOfRange(0.0, 1.0, Unset))

      test(m"an integer below its minimum is rejected"):
        capture[Json.Provider.Error](LintStagedSchema.record(json(t"""{"subTaskConcurrency": 0}""")).subTaskConcurrency).reason
      . assert(_ == IntOutOfRange(0, 1, Unset))

      test(m"a bounded integer within range reads as Int"):
        LintStagedSchema.record(lintStaged).subTaskConcurrency
      . assert(_ == (2: Optional[Int]))

      test(m"commitlint's plugins array reads as Text"):
        CommitlintSchema.record(json(t"""{"extends": ["@commitlint/config-conventional"], "plugins": ["commitlint-plugin-x"]}""")).plugins
      . assert(_ == List(t"commitlint-plugin-x"))

    suite(m"JSON Resume"):
      val resume = json(t"""{
        "basics": {
          "name": "Ada Lovelace", "email": "ada@example.org", "url": "https://ada.example.org/",
          "location": {"city": "London", "countryCode": "GB"},
          "profiles": [{"network": "Mastodon", "username": "ada"}]
        },
        "work": [
          {"name": "Analytical Engine Co", "position": "Programmer", "startDate": "1843-01", "highlights": ["Note G"]},
          {"name": "Byron Estate", "startDate": "eighteen-fifty"}
        ],
        "skills": [{"name": "Mathematics", "keywords": ["Bernoulli numbers"]}]
      }""")

      test(m"an email-formatted string reads as an EmailAddress"):
        ResumeSchema.record(resume).basics.let(_.email)
      . assert(_ == (email"ada@example.org": Optional[EmailAddress]))

      test(m"a nested array of records inside a nested record"):
        ResumeSchema.record(resume).basics.let(_.profiles.map(_.network))
      . assert(_ == (List((t"Mastodon": Optional[Text])): Optional[List[Optional[Text]]]))

      test(m"an array of records reads each record's fields"):
        ResumeSchema.record(resume).work.map(_.position)
      . assert(_ == List((t"Programmer": Optional[Text]), Unset))

      test(m"a date matching the pattern is accepted"):
        ResumeSchema.record(resume).work.prim.let(_.startDate)
      . assert(_ == (t"1843-01": Optional[Text]))

      test(m"a date not matching the pattern is rejected when that record is read"):
        capture[Json.Provider.Error](ResumeSchema.record(resume).work.stdlib(1).startDate).reason
      . assert:
          case PatternMismatch(t"eighteen-fifty", _) => true
          case _                                     => false

      test(m"a valid record's fields are unaffected by an invalid sibling"):
        ResumeSchema.record(resume).work.prim.let(_.highlights)
      . assert(_ == (List(t"Note G"): Optional[List[Text]]))

    suite(m"SchemaStore: launchSettings, husky, jsdoc, prettier, mocha"):
      val launch = json(t"""{"iisSettings": {"windowsAuthentication": false, "iisExpress": {"applicationUrl": "http://localhost:5000", "sslPort": 44300}}}""")

      test(m"a doubly-nested bounded integer reads as Int"):
        LaunchSettingsSchema.record(launch).iisSettings.let(_.iisExpress.let(_.sslPort))
      . assert(_ == (44300: Optional[Int]))

      test(m"a port above 65535 is rejected"):
        capture[Json.Provider.Error]:
          LaunchSettingsSchema.record(json(t"""{"iisSettings": {"iisExpress": {"sslPort": 70000}}}""")).iisSettings.let(_.iisExpress.let(_.sslPort))
        . reason
      . assert(_ == IntOutOfRange(70000, 0, 65535))

      test(m"a required nested object is read directly"):
        HuskySchema.record(json(t"""{"hooks": {"pre-commit": "npm test"}}""")).hooks.`pre-commit`
      . assert(_ == (t"npm test": Optional[Text]))

      test(m"a missing required nested object fails when read"):
        capture[Json.Error](HuskySchema.record(json(t"""{}""")).hooks).reason
      . assert(_ == Json.Error.Reason.Absent)

      val jsdoc = json(t"""{"sourceType": "module", "tags": {"dictionaries": ["jsdoc", "closure"]}, "templates": {"default": {"staticFiles": {"include": ["static"]}}}}""")

      test(m"an array of enum values reads as a List of Text"):
        JsdocSchema.record(jsdoc).tags.let(_.dictionaries)
      . assert(_ == (List(t"jsdoc", t"closure"): Optional[List[Text]]))

      test(m"an unlisted value in an enum array is rejected"):
        capture[Json.Provider.Error]:
          JsdocSchema.record(json(t"""{"tags": {"dictionaries": ["jsdoc", "typedoc"]}}""")).tags.let(_.dictionaries)
        . reason
      . assert(_ == NotPermitted(t"typedoc", List(t"jsdoc", t"closure")))

      test(m"a record nested four levels deep"):
        JsdocSchema.record(jsdoc).templates.let(_.default.let(_.staticFiles.let(_.include)))
      . assert(_ == (List(t"static"): Optional[List[Text]]))

      val prettier = json(t"""{"printWidth": 100, "semi": false, "arrowParens": "avoid", "trailingComma": "all", "plugins": ["prettier-plugin-x"], "overrides": [{"files": "*.md", "options": {"proseWrap": "always"}}]}""")

      test(m"a oneOf of documented enum values reads as one enum"):
        PrettierSchema.record(prettier).arrowParens
      . assert(_ == (t"avoid": Optional[Text]))

      test(m"a value outside a unified enum is rejected"):
        capture[Json.Provider.Error](PrettierSchema.record(json(t"""{"arrowParens": "sometimes"}""")).arrowParens).reason
      . assert(_ == NotPermitted(t"sometimes", List(t"always", t"avoid")))

      test(m"an overrides array reads records with nested options"):
        PrettierSchema.record(prettier).overrides.map(_.options.let(_.proseWrap))
      . assert(_ == List((t"always": Optional[Text])))

      test(m"mocha's timeout below zero is rejected"):
        capture[Json.Provider.Error](MochaSchema.record(json(t"""{"timeout": -1}""")).timeout).reason
      . assert(_ == IntOutOfRange(-1, 0, Unset))

    suite(m"SchemaStore: codecov, cloudbuild, static web app, package.json, eslintrc"):
      val codecov = json(t"""{"coverage": {"precision": 2, "round": "down", "notify": {"slack": {"url": "https://hooks.slack.example", "paths": ["src/"]}, "email": {"+to": ["a@example.org"]}}}, "ignore": ["vendor/"]}""")

      test(m"an integer within a small range"):
        CodecovSchema.record(codecov).coverage.let(_.precision)
      . assert(_ == (2: Optional[Int]))

      test(m"a precision above 5 is rejected"):
        capture[Json.Provider.Error](CodecovSchema.record(json(t"""{"coverage": {"precision": 7}}""")).coverage.let(_.precision)).reason
      . assert(_ == IntOutOfRange(7, 0, 5))

      test(m"a property named with a plus sign"):
        // `notify` is a method of every JVM object, so the `notify` property is only reachable
        // through `selectDynamic`; see the limitation test below.
        CodecovSchema.record(codecov).coverage.let: coverage =>
          coverage.selectDynamic("notify").asInstanceOf[Optional[Record]].let: notify =>
            notify.selectDynamic("email").asInstanceOf[Optional[Record]].let: email =>
              email.selectDynamic("+to").asInstanceOf[List[Text]]
      . assert(_ == (List(t"a@example.org"): Optional[List[Text]]))

      test(m"a property named after a JVM Object method is not selectable by name"):
        demilitarize(CodecovSchema.record(codecov).coverage.let(_.notify.let(_.email)))
      . assert(_.exists(_.message.contains("notify in class Object")))

      val build = json(t"""{"steps": [{"name": "gcr.io/cloud-builders/docker", "args": ["build", "."], "timeout": "120s", "allowExitCodes": [0, 1]}, {"name": "x", "timeout": "2m"}], "tags": ["ci"]}""")

      test(m"a duration matching its pattern"):
        CloudBuildSchema.record(build).steps.prim.let(_.timeout)
      . assert(_ == (t"120s": Optional[Text]))

      test(m"a duration in the wrong unit is rejected"):
        capture[Json.Provider.Error](CloudBuildSchema.record(build).steps.stdlib(1).timeout).reason
      . assert:
          case PatternMismatch(t"2m", _) => true
          case _                         => false

      test(m"an array of integers reads as a List of Int"):
        CloudBuildSchema.record(build).steps.prim.let(_.allowExitCodes)
      . assert(_ == (List(0, 1): Optional[List[Int]]))

      val swa = json(t"""{"routes": [{"route": "/api/*", "methods": ["GET", "POST"], "allowedRoles": ["admin"]}], "navigationFallback": {"rewrite": "/index.html"}, "platform": {"apiRuntime": "node:18"}}""")

      test(m"a required field inside an array element"):
        StaticWebAppSchema.record(swa).routes.map(_.route)
      . assert(_ == List(t"/api/*"))

      test(m"an enum array inside an array element"):
        StaticWebAppSchema.record(swa).routes.prim.let(_.methods)
      . assert(_ == (List(t"GET", t"POST"): Optional[List[Text]]))

      test(m"an unlisted HTTP method is rejected"):
        capture[Json.Provider.Error](StaticWebAppSchema.record(json(t"""{"routes": [{"route": "/", "methods": ["FETCH"]}]}""")).routes.prim.let(_.methods)).reason
      . assert:
          case NotPermitted(t"FETCH", _) => true
          case _                         => false

      val pkg = json(t"""{"name": "soundness", "version": "0.1.0", "type": "module", "scripts": {"test": "mill test"}, "engines": {"node": ">=20"}, "dependencies": {"left-pad": "^1.3.0"}, "publishConfig": {"access": "public"}, "keywords": ["json", "schema"]}""")

      test(m"package.json's scripts read as a record"):
        PackageJsonSchema.record(pkg).scripts.let(_.test)
      . assert(_ == (t"mill test": Optional[Text]))

      test(m"a name within its length bounds"):
        PackageJsonSchema.record(pkg).name
      . assert(_ == (t"soundness": Optional[Text]))

      test(m"an empty name is rejected by minLength"):
        capture[Json.Provider.Error](PackageJsonSchema.record(json(t"""{"name": ""}""")).name).reason
      . assert(_ == LengthOutOfRange(t"", 1, 214))

      test(m"the module type enum"):
        PackageJsonSchema.record(pkg).`type`
      . assert(_ == (t"module": Optional[Text]))

      test(m"a dependencies dictionary reads as a Map"):
        PackageJsonSchema.record(pkg).dependencies
      . assert(_ == Map(t"left-pad" -> t"^1.3.0"))

      test(m"eslintrc's nested environment flags"):
        EslintrcSchema.record(json(t"""{"env": {"browser": true, "node": false}, "parserOptions": {"ecmaFeatures": {"jsx": true}}}""")).parserOptions.let(_.ecmaFeatures.let(_.jsx))
      . assert(_ == (true: Optional[Boolean]))

    suite(m"Schema features: references"):
      val doc = json(t"""{"home": {"street": "High St", "number": 3}, "work": {"building": "Tower"}}""")

      test(m"a $$ref to definitions reads the definition's record"):
        DefinitionsSchema.record(doc).home.street
      . assert(_ == t"High St")

      test(m"a $$ref to $$defs reads the definition's record"):
        DefinitionsSchema.record(doc).work.let(_.building)
      . assert(_ == (t"Tower": Optional[Text]))

      val tree = json(t"""{"label": "root", "children": [{"label": "branch", "children": [{"label": "leaf"}]}]}""")

      test(m"a reference to the root unrolls one level"):
        RecursiveSchema.record(tree).children.prim.let(_.label)
      . assert(_ == (t"branch": Optional[Text]))

      test(m"a recursive reference reads as raw JSON at the recursion"):
        RecursiveSchema.record(tree).children.prim.let(_.children.prim.let(_(t"label").as[Text]))
      . assert(_ == (t"leaf": Optional[Text]))

      test(m"an allOf merges the referenced base with the node's own properties"):
        val record = AllOfSchema.record(json(t"""{"name": "Ann", "age": 41, "email": "ann@example.org"}"""))
        (record.name, record.age, record.email)
      . assert(_ == (t"Ann", 41, (email"ann@example.org": Optional[EmailAddress])))

      test(m"the object form is chosen when the root may also be a string"):
        ObjectOrStringRootSchema.record(json(t"""{"name": "cfg"}""")).name
      . assert(_ == t"cfg")

    suite(m"Schema features: variants and bounds"):
      val variants = json(t"""{"nickname": null, "level": "high", "kind": "widget", "count": 2, "mixed": 5, "either": "x", "choice": "b", "stringy": "auto", "anything": [1], "unknown": 3}""")

      test(m"a nullable required field reads null as Unset"):
        VariantsSchema.record(variants).nickname
      . assert(_ == Unset)

      test(m"an enum without a type is inferred as a string enum"):
        VariantsSchema.record(variants).level
      . assert(_ == t"high")

      test(m"an enum value outside the list is rejected"):
        capture[Json.Provider.Error](VariantsSchema.record(json(t"""{"level": "medium"}""")).level).reason
      . assert(_ == NotPermitted(t"medium", List(t"low", t"high")))

      test(m"a const reads as an enum of one value"):
        capture[Json.Provider.Error](VariantsSchema.record(json(t"""{"kind": "gadget"}""")).kind).reason
      . assert(_ == NotPermitted(t"gadget", List(t"widget")))

      test(m"an integer enum reads as Int"):
        VariantsSchema.record(variants).count
      . assert(_ == (2: Optional[Int]))

      test(m"a union of two types reads as a union type"):
        VariantsSchema.record(variants).mixed
      . assert(_ == 5)

      test(m"an anyOf with null marks a required field optional"):
        (VariantsSchema.record(variants).either, VariantsSchema.record(json(t"""{"either": null}""")).either)
      . assert(_ == ((t"x": Optional[Text]), Unset))

      test(m"a oneOf of single-value enums unifies to one enum"):
        VariantsSchema.record(variants).choice
      . assert(_ == t"b")

      test(m"an anyOf of an enum and a pattern reads as a plain string"):
        VariantsSchema.record(json(t"""{"stringy": "anything goes"}""")).stringy
      . assert(_ == t"anything goes")

      test(m"a boolean schema reads as raw JSON"):
        VariantsSchema.record(variants).anything.let(_.as[List[Int]])
      . assert(_ == (List(1): Optional[List[Int]]))

      test(m"a not-schema reads as raw JSON"):
        VariantsSchema.record(variants).unknown.let(_.as[Int])
      . assert(_ == (3: Optional[Int]))

      val bounds = json(t"""{"port": 8080, "ratio": 0.5, "code": "GB", "positive": 1, "below": 9.99, "score": 50}""")

      test(m"values within every bound read at their types"):
        val record = BoundsSchema.record(bounds)
        (record.port, record.ratio, record.code, record.positive, record.below, record.score)
      . assert(_ == (8080, 0.5, t"GB", 1, 9.99, (50: Optional[Int])))

      test(m"a draft-4 boolean exclusiveMinimum excludes the minimum"):
        capture[Json.Provider.Error](BoundsSchema.record(json(t"""{"positive": 0}""")).positive).reason
      . assert(_ == IntOutOfRange(0, 1, Unset))

      test(m"a numeric exclusiveMaximum excludes the maximum"):
        capture[Json.Provider.Error](BoundsSchema.record(json(t"""{"below": 10}""")).below).reason
      . assert(_ == NumberOutOfRange(10.0, Unset, 10.0))

      test(m"exclusive bounds on an integer become inclusive ones"):
        capture[Json.Provider.Error](BoundsSchema.record(json(t"""{"score": 100}""")).score).reason
      . assert(_ == IntOutOfRange(100, 1, 99))

      test(m"a string longer than maxLength is rejected"):
        capture[Json.Provider.Error](BoundsSchema.record(json(t"""{"code": "GBR12"}""")).code).reason
      . assert(_ == LengthOutOfRange(t"GBR12", 2, 4))

      test(m"a tuple over bounded fields discharges their errors"):
        val tuple = BoundsSchema.tuple(bounds)
        (tuple.port, tuple.code)
      . assert(_ == (8080, t"GB"))

    suite(m"Schema features: containers and names"):
      val containers = json(t"""{"labels": {"a": "1"}, "byPattern": {"x-a": "1"}, "anything": [1, "two"], "pair": ["a", 1], "matrix": [[1, 2], [3]], "people": [{"name": "Al"}]}""")

      test(m"a dictionary constrained by additionalProperties reads as a Map"):
        ContainersSchema.record(containers).labels
      . assert(_ == Map(t"a" -> t"1"))

      test(m"an array without items reads as a List of raw JSON"):
        ContainersSchema.record(containers).anything.map(_.toString.length > 0)
      . assert(_ == List(true, true))

      test(m"a tuple-typed array reads as a List of raw JSON"):
        ContainersSchema.record(containers).pair.stdlib.size
      . assert(_ == 2)

      test(m"an array of arrays reads as a List of Lists"):
        ContainersSchema.record(containers).matrix.stdlib.map(_.stdlib.sum)
      . assert(_ == scala.collection.immutable.List(3.0, 3.0))

      test(m"an array of referenced records"):
        ContainersSchema.record(containers).people.map(_.name)
      . assert(_ == List(t"Al"))

      val awkward = json(t"""{"data": {"value": 1}, "type": "t", "class": "c", "content-type": "text/plain", "$$id": "i", "toString": true}""")

      test(m"a property named data is a field, not the record's own data"):
        AwkwardNamesSchema.record(awkward).data.value
      . assert(_ == (1: Optional[Int]))

      test(m"the record's underlying JSON is still reachable"):
        AwkwardNamesSchema.record(awkward).recordData(t"type").as[Text]
      . assert(_ == t"t")

      test(m"properties named with keywords and symbols"):
        val record = AwkwardNamesSchema.record(awkward)
        (record.`type`, record.`class`, record.`content-type`, record.`$id`)
      . assert(_ == (t"t", (t"c": Optional[Text]), (t"text/plain": Optional[Text]), (t"i": Optional[Text])))

      test(m"a property named toString is read through selectDynamic"):
        AwkwardNamesSchema.record(awkward).selectDynamic("toString")
      . assert(_ == (true: Optional[Boolean]))

      test(m"a tuple over awkward names"):
        AwkwardNamesSchema.tuple(awkward).`content-type`
      . assert(_ == (t"text/plain": Optional[Text]))

    suite(m"Schema features: unions"):
      val text = json(t"""{"id": "abc", "exec": "make", "ignore": "build", "flag": "auto", "port": "8080"}""")
      val other = json(t"""{"id": 7, "exec": {"command": "make", "args": ["-j", "4"]}, "ignore": ["a", "b"], "flag": true, "port": 8080, "shapes": {"radius": 2}}""")

      test(m"a type list reads as a union, as a string"):
        UnionsSchema.record(text).id
      . assert(_ == t"abc")

      test(m"a type list reads as a union, as an integer"):
        UnionsSchema.record(other).id
      . assert(_ == 7)

      test(m"a union field has the union type"):
        val id: Text | Int = UnionsSchema.record(other).id
        id
      . assert(_ == 7)

      test(m"a string-or-object union reads the string"):
        UnionsSchema.record(text).exec
      . assert(_ == t"make")

      test(m"a string-or-object union reads the object as a record"):
        UnionsSchema.record(other).exec match
          case record: Record => record.selectDynamic("command")
          case other          => other
      . assert(_ == t"make")

      test(m"a string-or-array union reads the array as a list"):
        UnionsSchema.record(other).ignore
      . assert(_ == List(t"a", t"b"))

      test(m"a string-or-array union reads the string"):
        UnionsSchema.record(text).ignore
      . assert(_ == t"build")

      test(m"a boolean-or-enum union reads the boolean"):
        UnionsSchema.record(other).flag
      . assert(_ == (true: Optional[Boolean | Text]))

      test(m"a boolean-or-enum union checks the enum"):
        capture[Json.Provider.Error](UnionsSchema.record(json(t"""{"flag": "manual"}""")).flag).reason
      . assert(_ == NotPermitted(t"manual", List(t"auto")))

      test(m"a union of fallible alternatives reads the integer"):
        UnionsSchema.record(other).port
      . assert(_ == (8080: Optional[Int | Text]))

      test(m"a union of fallible alternatives checks the chosen alternative"):
        capture[Json.Provider.Error](UnionsSchema.record(json(t"""{"port": "eighty"}""")).port).reason
      . assert:
          case PatternMismatch(t"eighty", _) => true
          case _                             => false

      test(m"a union of fallible alternatives checks the other alternative too"):
        capture[Json.Provider.Error](UnionsSchema.record(json(t"""{"port": 0}""")).port).reason
      . assert(_ == IntOutOfRange(0, 1, Unset))

      test(m"a value of a kind the union does not offer fails when read"):
        try
          UnionsSchema.record(json(t"""{"id": true}""")).id
          t"no failure"
        catch case panic: Panic => panic.message.text
      . assert(_.s.contains("boolean"))

      test(m"alternatives of the same kind cannot be told apart and read as raw JSON"):
        UnionsSchema.record(other).shapes.let(_(t"radius").as[Double])
      . assert(_ == (2.0: Optional[Double]))

      test(m"a tuple reads a union too, its record alternative as a nested tuple"):
        UnionsSchema.tuple(other).exec match
          case tuple: Tuple => tuple.productElement(0)
          case other        => other
      . assert(_ == t"make")

    suite(m"Schema features: dictionaries"):
      val dict = json(t"""{
        "labels": {"a": "1", "b": "2"}, "scores": {"x": 3}, "groups": {"g": ["p", "q"]},
        "people": {"ann": {"name": "Ann"}}, "byPattern": {"x-a": "1", "y-b": "2"},
        "mixedPatterns": {"x-a": "1"}, "open": {"any": [1]}, "nested": {"outer": {"inner": 5}}
      }""")

      test(m"additionalProperties reads as a Map of Text"):
        DictionariesSchema.record(dict).labels
      . assert(_ == Map(t"a" -> t"1", t"b" -> t"2"))

      test(m"an absent dictionary reads as an empty Map"):
        DictionariesSchema.record(json(t"{}")).labels
      . assert(_ == Map())

      test(m"a dictionary of bounded integers checks each value"):
        capture[Json.Provider.Error](DictionariesSchema.record(json(t"""{"scores": {"x": -1}}""")).scores).reason
      . assert(_ == IntOutOfRange(-1, 0, Unset))

      test(m"a dictionary of lists"):
        DictionariesSchema.record(dict).groups
      . assert(_ == Map(t"g" -> List(t"p", t"q")))

      test(m"a dictionary of referenced records"):
        DictionariesSchema.record(dict).people.stdlib.map { (key, person) => (key, person.name) }
      . assert(_ == scala.collection.immutable.Map(t"ann" -> t"Ann"))

      test(m"patternProperties of one type reads as a Map"):
        DictionariesSchema.record(dict).byPattern
      . assert(_ == Map(t"x-a" -> t"1", t"y-b" -> t"2"))

      test(m"patternProperties of differing types reads as raw JSON"):
        DictionariesSchema.record(dict).mixedPatterns.let(_(t"x-a").as[Text])
      . assert(_ == (t"1": Optional[Text]))

      test(m"an open object reads as raw JSON"):
        DictionariesSchema.record(dict).open.let(_(t"any").as[List[Int]])
      . assert(_ == (List(1): Optional[List[Int]]))

      test(m"a dictionary of dictionaries"):
        DictionariesSchema.record(dict).nested
      . assert(_ == Map(t"outer" -> Map(t"inner" -> 5)))

    suite(m"Schema features: unusable roots"):
      test(m"a root array is a compile error"):
        demilitarize(ArrayRootSchema.record(json(t"[]")))
      . assert(_.exists(_.message.contains("root does not describe an object")))

      test(m"a root string is a compile error"):
        demilitarize(StringRootSchema.record(json(t"\"x\"")))
      . assert(_.exists(_.message.contains("root does not describe an object")))

      test(m"a root referring to another document is a compile error naming the cause"):
        demilitarize(ExternalRootSchema.record(json(t"{}")))
      . assert(_.exists(_.message.contains("refers to another document")))

      test(m"a real schema composed only of external references is a compile error"):
        demilitarize(WebManifestCombinedSchema.record(json(t"{}")))
      . assert(_.exists(_.message.contains("refers to another document")))

      test(m"a real root-array schema is a compile error"):
        demilitarize(OpenWeatherRoadRiskSchema.record(json(t"[]")))
      . assert(_.exists(_.message.contains("root does not describe an object")))

      test(m"a property the schema does not declare does not compile"):
        demilitarize(AddressSchema.record(json(t"{}")).county)
      . assert(_.exists(_.reason == CompileError.Reason.NotAMember))
