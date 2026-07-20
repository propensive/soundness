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
package ypsiloid

import scala.quoted.*

import ambience.*, environments.javaEnvironment, systems.javaSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import probably.*
import proscenium.*
import quantitative.*
import sedentary.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import vacuous.*

object Benchmarks extends Suite(m"Ypsiloid YAML parser benchmarks"):
  sealed trait Information extends Dimension
  sealed trait Bytes[Power <: Nat] extends Units[Power, Information]
  val Byte: MetricUnit[Bytes[1]] = MetricUnit(1.0)

  given byteDesignation: Designation[Bytes[1]] = () => t"B"
  given decimalizer:     Decimalizer            = Decimalizer(2)
  given device:          BenchmarkDevice        = LocalhostDevice
  given prefixes:        Prefixes               = Prefixes(List(Kilo, Mega, Giga, Tera))

  // SnakeYAML Engine — YAML 1.2 strict reference parser. Constructed once
  // and reused across iterations to amortise builder/setting allocation,
  // matching how it would be used in production.
  val snakeYaml: org.snakeyaml.engine.v2.api.Load =
    new org.snakeyaml.engine.v2.api.Load(
      org.snakeyaml.engine.v2.api.LoadSettings.builder().nn.build().nn)

  def parseWithSnakeYaml(text: String): Object =
    snakeYaml.loadFromString(text).nn

  def run(): Unit =
    val bench = Bench()

    val size1 = yamlText1.s.length*Byte
    val size2 = yamlText2.s.length*Byte
    val size3 = yamlText3.s.length*Byte
    val size4 = yamlText4.s.length*Byte
    val size5 = yamlText5.s.length*Byte

    suite(m"Small flow document"):
      bench(m"Parse with Ypsiloid")
        ( target = 1*Second, operationSize = size1, baseline = Baseline(compare = Min) ):
        '{ Yaml.Parser.parse(ypsiloid.Benchmarks.yamlText1) }

      bench(m"Parse with Ypsiloid (tracking)")(target = 1*Second, operationSize = size1):
        '{ Yaml.Parser.parseTracked(ypsiloid.Benchmarks.yamlText1) }

      bench(m"Parse with snakeyaml-engine")(target = 1*Second, operationSize = size1):
        '{ ypsiloid.Benchmarks.parseWithSnakeYaml(ypsiloid.Benchmarks.yamlText1.s) }

    suite(m"Block-mapping config (~30 keys)"):
      bench(m"Parse with Ypsiloid")
        ( target = 1*Second, operationSize = size2, baseline = Baseline(compare = Min) ):
        '{ Yaml.Parser.parse(ypsiloid.Benchmarks.yamlText2) }

      bench(m"Parse with Ypsiloid (tracking)")(target = 1*Second, operationSize = size2):
        '{ Yaml.Parser.parseTracked(ypsiloid.Benchmarks.yamlText2) }

      bench(m"Parse with snakeyaml-engine")(target = 1*Second, operationSize = size2):
        '{ ypsiloid.Benchmarks.parseWithSnakeYaml(ypsiloid.Benchmarks.yamlText2.s) }

    suite(m"Block sequence of 100 records"):
      bench(m"Parse with Ypsiloid")
        ( target = 1*Second, operationSize = size3, baseline = Baseline(compare = Min) ):
        '{ Yaml.Parser.parse(ypsiloid.Benchmarks.yamlText3) }

      bench(m"Parse with Ypsiloid (tracking)")(target = 1*Second, operationSize = size3):
        '{ Yaml.Parser.parseTracked(ypsiloid.Benchmarks.yamlText3) }

      bench(m"Parse with snakeyaml-engine")(target = 1*Second, operationSize = size3):
        '{ ypsiloid.Benchmarks.parseWithSnakeYaml(ypsiloid.Benchmarks.yamlText3.s) }

    suite(m"Deeply nested block structures"):
      bench(m"Parse with Ypsiloid")
        ( target = 1*Second, operationSize = size4, baseline = Baseline(compare = Min) ):
        '{ Yaml.Parser.parse(ypsiloid.Benchmarks.yamlText4) }

      bench(m"Parse with Ypsiloid (tracking)")(target = 1*Second, operationSize = size4):
        '{ Yaml.Parser.parseTracked(ypsiloid.Benchmarks.yamlText4) }

      bench(m"Parse with snakeyaml-engine")(target = 1*Second, operationSize = size4):
        '{ ypsiloid.Benchmarks.parseWithSnakeYaml(ypsiloid.Benchmarks.yamlText4.s) }

    suite(m"Heavy quoted strings"):
      bench(m"Parse with Ypsiloid")
        ( target = 1*Second, operationSize = size5, baseline = Baseline(compare = Min) ):
        '{ Yaml.Parser.parse(ypsiloid.Benchmarks.yamlText5) }

      bench(m"Parse with Ypsiloid (tracking)")(target = 1*Second, operationSize = size5):
        '{ Yaml.Parser.parseTracked(ypsiloid.Benchmarks.yamlText5) }

      bench(m"Parse with snakeyaml-engine")(target = 1*Second, operationSize = size5):
        '{ ypsiloid.Benchmarks.parseWithSnakeYaml(ypsiloid.Benchmarks.yamlText5.s) }

  // Fixture 1: small flow document — measures fixed overhead, dispatch.
  lazy val yamlText1: Text = t"""{name: Alice, age: 30, items: [1, 2, 3]}"""

  // Fixture 2: typical application config — block mapping, ~30 keys, mixed
  // scalar types. Stresses the line-by-line block-mapping path and
  // primitive scalar resolution.
  lazy val yamlText2: Text = t"""server:
  host: example.com
  port: 8080
  threads: 16
  timeout: 30
  tls: true
  cors:
    enabled: true
    origins: [https://app.example.com, https://admin.example.com]
database:
  driver: postgres
  url: jdbc:postgresql://db.example.com:5432/app
  user: app_service
  pool_size: 20
  ssl_mode: require
logging:
  level: info
  format: json
  destination: stdout
features:
  beta_dashboard: false
  new_billing: true
  experiment_threshold: 0.15
limits:
  max_request_bytes: 1048576
  max_uploads_per_day: 1000
  rate_limit_rpm: 60
"""

  // Fixture 3: block sequence of 100 record-shaped mappings — typical
  // "list of records" pattern. Stresses repeated parse of the same
  // mapping shape.
  lazy val yamlText3: Text =
    val sb = new scala.collection.mutable.StringBuilder
    var i = 0
    while i < 100 do
      val active = if (i & 1) == 0 then "true" else "false"
      val role = if i % 10 == 0 then "admin" else "user"
      sb.append("- id: ").append(i)
      sb.append("\n  username: user").append(i)
      sb.append("\n  email: user").append(i).append("@example.com")
      sb.append("\n  active: ").append(active)
      sb.append("\n  role: ").append(role)
      sb.append("\n  score: ").append((i*7)%100).append('.').append(i%10)
      sb.append('\n')
      i += 1
    sb.toString.tt

  // Fixture 4: deeply nested block structures (5 levels) — stresses
  // recursive descent and block-context indent tracking.
  lazy val yamlText4: Text = t"""level1:
  level2:
    level3:
      level4:
        level5:
          a: 1
          b: 2
          c: [10, 20, 30]
          d:
            inner: nested
            list:
              - one
              - two
              - three
        sibling:
          x: false
          y: true
      another:
        items: [a, b, c, d, e, f]
        count: 6
    second:
      foo: bar
      baz: 42
  third:
    leaf: value
"""

  // Fixture 5: heavy quoted strings — exercises both the unescape path
  // (double-quoted with multiple escape sequences) and the multi-line
  // line-folding path. Mixes single- and double-quoted styles.
  lazy val yamlText5: Text = t"""title: "The Quick Brown \\"Fox\\" Jumps"
description: "Line one\\nLine two\\nLine three with a \\t tab"
multiline: "first line
  second line
  third line"
unicode: "smile \\u263A and accent \\u00e9"
single: 'don''t panic, but it''s on fire'
escape_heavy: "\\\\backslash and \\"quotes\\" and \\\\n literal"
empty_double: ""
empty_single: ''
nested:
  inner: "one \\n two \\t three"
  more: "again with
   line folding"
"""
