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

import classloaders.threadContextClassloader
import strategies.throwUnsafely

// Real-world JSON Schema documents, as published, under `res/test/schemas`, each bound to a
// `Json.Provider` for the provider tests. Sources: json-schema.org's examples, the GeoJSON
// schema, the JSON Resume schema, and SchemaStore. Each provider lives in this file, compiled
// before the tests, so that the `record` macro can evaluate it while the test call sites are
// being compiled; the resource is read then, from the compilation classpath.

object AddressSchema extends Json.Provider(cp"/schemas/address.schema.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object GeographicalLocationSchema extends Json.Provider(cp"/schemas/geographical-location.schema.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object CalendarSchema extends Json.Provider(cp"/schemas/calendar.schema.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object CardSchema extends Json.Provider(cp"/schemas/card.schema.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object GeoJsonPointSchema extends Json.Provider(cp"/schemas/geojson-point.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object GeoJsonFeatureSchema extends Json.Provider(cp"/schemas/geojson-feature.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object LernaSchema extends Json.Provider(cp"/schemas/lerna.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object NycrcSchema extends Json.Provider(cp"/schemas/nycrc.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object VsconfigSchema extends Json.Provider(cp"/schemas/vsconfig.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object GlobalJsonSchema extends Json.Provider(cp"/schemas/global.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object NodemonSchema extends Json.Provider(cp"/schemas/nodemon.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object BowerrcSchema extends Json.Provider(cp"/schemas/bowerrc.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object LintStagedSchema extends Json.Provider(cp"/schemas/lintstagedrc.schema.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object CommitlintSchema extends Json.Provider(cp"/schemas/commitlintrc.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object ResumeSchema extends Json.Provider(cp"/schemas/jsonresume.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object LaunchSettingsSchema extends Json.Provider(cp"/schemas/launchsettings.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object HuskySchema extends Json.Provider(cp"/schemas/huskyrc.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object DotnetCliHostSchema extends Json.Provider(cp"/schemas/dotnetcli.host.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object JsdocSchema extends Json.Provider(cp"/schemas/jsdoc-1.0.0.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object WebManifestCombinedSchema extends Json.Provider(cp"/schemas/web-manifest-combined.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object PrettierSchema extends Json.Provider(cp"/schemas/prettierrc.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object MochaSchema extends Json.Provider(cp"/schemas/mocharc.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object CoffeelintSchema extends Json.Provider(cp"/schemas/coffeelint.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object CodecovSchema extends Json.Provider(cp"/schemas/codecov.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object CloudBuildSchema extends Json.Provider(cp"/schemas/cloudbuild.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object StaticWebAppSchema extends Json.Provider(cp"/schemas/staticwebapp.config.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object BabelSchema extends Json.Provider(cp"/schemas/babelrc.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object OpenWeatherRoadRiskSchema extends Json.Provider(cp"/schemas/openweather.roadrisk.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object RenovateSchema extends Json.Provider(cp"/schemas/renovate.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object PackageJsonSchema extends Json.Provider(cp"/schemas/package.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}

object EslintrcSchema extends Json.Provider(cp"/schemas/eslintrc.json"):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}
