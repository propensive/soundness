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
package merino

import scala.quoted.*

import ambience.*, environments.java, systems.java
import anticipation.*
import contingency.*, strategies.throwUnsafely
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContext
import probably.*
import proscenium.*
import quantitative.*
import rudiments.*
import sedentary.*
import symbolism.*
import temporaryDirectories.system
import vacuous.*

object Benchmarks extends Suite(m"Merino benchmarks"):
  sealed trait Information extends Dimension
  sealed trait Bytes[Power <: Nat] extends Units[Power, Information]
  val Byte: MetricUnit[Bytes[1]] = MetricUnit(1.0)

  given byteDesignation: Designation[Bytes[1]] = () => t"B"
  given decimalizer:     Decimalizer            = Decimalizer(2)
  given device:          BenchmarkDevice        = LocalhostDevice

  // Auto-scale byte sizes (B → kB → MB → GB → TB) and byte rates so the
  // table prints "1.3 GB·s¯¹" instead of "1.3×10⁹ B·s¯¹".
  given prefixes: Prefixes = Prefixes(List(Kilo, Mega, Giga, Tera))

  // Codec used by the Jsoniter benchmark to parse into a circe `Json` AST.
  val jsoniterCodec: com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec[io.circe.Json] =
    com.github.plokhotnyuk.jsoniter_scala.circe.JsoniterScalaCodec.jsonCodec()

  def parseWithJsoniter(text: String): io.circe.Json =
    com.github.plokhotnyuk.jsoniter_scala.core.readFromString[io.circe.Json](text)
      (using jsoniterCodec)

  def run(): Unit =
    val bench = Bench()

    val size1 = jsonBytes1.length*Byte
    val size2 = jsonBytes2.length*Byte
    val size3 = jsonBytes3.length*Byte
    val size4 = jsonBytes4.length*Byte
    val size5 = jsonBytes5.length*Byte

    suite(m"Parse example 1"):
      bench(m"Parse file with Merino")
        (target = 1*Second, operationSize = size1, baseline = Baseline(compare = Min)):
        '{ JsonAst.parse(merino.Benchmarks.jsonBytes1) }

      bench(m"Parse file with Jawn")(target = 1*Second, operationSize = size1):
        '{
            import org.typelevel.jawn.ast.JParser
            JParser.parseFromString(merino.Benchmarks.jsonText1)
          }

      bench(m"Parse file with Circe")(target = 1*Second, operationSize = size1):
        '{ io.circe.parser.parse(merino.Benchmarks.jsonText1) }

      bench(m"Parse file with Jsoniter")(target = 1*Second, operationSize = size1):
        '{ merino.Benchmarks.parseWithJsoniter(merino.Benchmarks.jsonText1) }

    suite(m"Parse example 2"):
      bench(m"Parse file with Merino")
        (target = 1*Second, operationSize = size2, baseline = Baseline(compare = Min)):
        '{ JsonAst.parse(merino.Benchmarks.jsonBytes2) }

      bench(m"Parse file with Jawn")(target = 1*Second, operationSize = size2):
        '{
            import org.typelevel.jawn.ast.JParser
            JParser.parseFromString(merino.Benchmarks.jsonText2)
          }

      bench(m"Parse file with Circe")(target = 1*Second, operationSize = size2):
        '{ io.circe.parser.parse(merino.Benchmarks.jsonText2) }

      bench(m"Parse file with Jsoniter")(target = 1*Second, operationSize = size2):
        '{ merino.Benchmarks.parseWithJsoniter(merino.Benchmarks.jsonText2) }

    suite(m"Parse example 3"):
      bench(m"Parse file with Merino")
        (target = 1*Second, operationSize = size3, baseline = Baseline(compare = Min)):
        '{ JsonAst.parse(merino.Benchmarks.jsonBytes3) }

      bench(m"Parse file with Jawn")(target = 1*Second, operationSize = size3):
        '{
            import org.typelevel.jawn.ast.JParser
            JParser.parseFromString(merino.Benchmarks.jsonText3)
          }

      bench(m"Parse file with Circe")(target = 1*Second, operationSize = size3):
        '{ io.circe.parser.parse(merino.Benchmarks.jsonText3) }

      bench(m"Parse file with Jsoniter")(target = 1*Second, operationSize = size3):
        '{ merino.Benchmarks.parseWithJsoniter(merino.Benchmarks.jsonText3) }

    suite(m"Parse example 4 (100 user records)"):
      bench(m"Parse file with Merino")
        (target = 1*Second, operationSize = size4, baseline = Baseline(compare = Min)):
        '{ JsonAst.parse(merino.Benchmarks.jsonBytes4) }

      bench(m"Parse file with Jawn")(target = 1*Second, operationSize = size4):
        '{
            import org.typelevel.jawn.ast.JParser
            JParser.parseFromString(merino.Benchmarks.jsonText4)
          }

      bench(m"Parse file with Circe")(target = 1*Second, operationSize = size4):
        '{ io.circe.parser.parse(merino.Benchmarks.jsonText4) }

      bench(m"Parse file with Jsoniter")(target = 1*Second, operationSize = size4):
        '{ merino.Benchmarks.parseWithJsoniter(merino.Benchmarks.jsonText4) }

    suite(m"Parse example 5 (500 log entries)"):
      bench(m"Parse file with Merino")
        (target = 1*Second, operationSize = size5, baseline = Baseline(compare = Min)):
        '{ JsonAst.parse(merino.Benchmarks.jsonBytes5) }

      bench(m"Parse file with Jawn")(target = 1*Second, operationSize = size5):
        '{
            import org.typelevel.jawn.ast.JParser
            JParser.parseFromString(merino.Benchmarks.jsonText5)
          }

      bench(m"Parse file with Circe")(target = 1*Second, operationSize = size5):
        '{ io.circe.parser.parse(merino.Benchmarks.jsonText5) }

      bench(m"Parse file with Jsoniter")(target = 1*Second, operationSize = size5):
        '{ merino.Benchmarks.parseWithJsoniter(merino.Benchmarks.jsonText5) }

  lazy val jsonText1: String = jsonExample1.s
  lazy val jsonText2: String = jsonExample2.s
  lazy val jsonText3: String = jsonExample3.s
  lazy val jsonBytes1: Data = jsonText1.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val jsonBytes2: Data = jsonText2.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val jsonBytes3: Data = jsonText3.getBytes("UTF-8").nn.immutable(using Unsafe)

  // Example 4: array of 100 user records — the typical "JSON array of records"
  // pattern with a small fixed key set repeated across all elements.
  lazy val jsonText4: String =
    val sb = new _root_.java.lang.StringBuilder
    sb.append("{\"users\":[")
    var i = 0
    while i < 100 do
      if i > 0 then sb.append(',')
      val active = if (i & 1) == 0 then "true" else "false"
      val role = if i % 10 == 0 then "admin" else "user"
      sb.append(s"""{"id":$i,"username":"user$i","email":"user$i@example.com","active":$active,"role":"$role"}""")
      i += 1
    sb.append("]}")
    sb.toString.nn

  lazy val jsonBytes4: Data = jsonText4.getBytes("UTF-8").nn.immutable(using Unsafe)

  // Example 5: a longer NDJSON-style log array with 500 entries; 6 keys per
  // entry, all repeating across entries. Larger than Example 4 and stresses
  // the per-key path more heavily.
  lazy val jsonText5: String =
    val sb = new _root_.java.lang.StringBuilder
    sb.append("{\"logs\":[")
    val levels = Array("info", "debug", "warn", "error")
    val services = Array("auth", "api", "db", "cache", "worker")
    var i = 0
    while i < 500 do
      if i > 0 then sb.append(',')
      val ts = 1700000000L + i
      val level = levels(i & 3)
      val service = services(i % 5)
      val userId = 1000 + (i % 50)
      sb.append(s"""{"timestamp":$ts,"level":"$level","service":"$service","requestId":"req-$i","userId":$userId,"message":"event $i processed"}""")
      i += 1
    sb.append("]}")
    sb.toString.nn

  lazy val jsonBytes5: Data = jsonText5.getBytes("UTF-8").nn.immutable(using Unsafe)

  val jsonExample1: Text = t"""

{"web-app": {
  "servlet": [
    {
      "servlet-name": "cofaxCDS",
      "servlet-class": "org.cofax.cds.CDSServlet",
      "init-param": {
        "configGlossary:installationAt": "Philadelphia, PA",
        "configGlossary:adminEmail": "ksm@pobox.com",
        "configGlossary:poweredBy": "Cofax",
        "configGlossary:poweredByIcon": "/images/cofax.gif",
        "configGlossary:staticPath": "/content/static",
        "templateProcessorClass": "org.cofax.WysiwygTemplate",
        "templateLoaderClass": "org.cofax.FilesTemplateLoader",
        "templatePath": "templates",
        "templateOverridePath": "",
        "defaultListTemplate": "listTemplate.htm",
        "defaultFileTemplate": "articleTemplate.htm",
        "useJSP": false,
        "jspListTemplate": "listTemplate.jsp",
        "jspFileTemplate": "articleTemplate.jsp",
        "cachePackageTagsTrack": 200,
        "cachePackageTagsStore": 200,
        "cachePackageTagsRefresh": 60,
        "cacheTemplatesTrack": 100,
        "cacheTemplatesStore": 50,
        "cacheTemplatesRefresh": 15,
        "cachePagesTrack": 200,
        "cachePagesStore": 100,
        "cachePagesRefresh": 10,
        "cachePagesDirtyRead": 10,
        "searchEngineListTemplate": "forSearchEnginesList.htm",
        "searchEngineFileTemplate": "forSearchEngines.htm",
        "searchEngineRobotsDb": "WEB-INF/robots.db",
        "useDataStore": true,
        "dataStoreClass": "org.cofax.SqlDataStore",
        "redirectionClass": "org.cofax.SqlRedirection",
        "dataStoreName": "cofax",
        "dataStoreDriver": "com.microsoft.jdbc.sqlserver.SQLServerDriver",
        "dataStoreUrl": "jdbc:microsoft:sqlserver://LOCALHOST:1433;DatabaseName=goon",
        "dataStoreUser": "sa",
        "dataStorePassword": "dataStoreTestQuery",
        "dataStoreTestQuery": "SET NOCOUNT ON;select test='test';",
        "dataStoreLogFile": "/usr/local/tomcat/logs/datastore.log",
        "dataStoreInitConns": 10,
        "dataStoreMaxConns": 100,
        "dataStoreConnUsageLimit": 100,
        "dataStoreLogLevel": "debug",
        "maxUrlLength": 500}},
    {
      "servlet-name": "cofaxEmail",
      "servlet-class": "org.cofax.cds.EmailServlet",
      "init-param": {
      "mailHost": "mail1",
      "mailHostOverride": "mail2"}},
    {
      "servlet-name": "cofaxAdmin",
      "servlet-class": "org.cofax.cds.AdminServlet"},

    {
      "servlet-name": "fileServlet",
      "servlet-class": "org.cofax.cds.FileServlet"},
    {
      "servlet-name": "cofaxTools",
      "servlet-class": "org.cofax.cms.CofaxToolsServlet",
      "init-param": {
        "templatePath": "toolstemplates/",
        "log": 1,
        "logLocation": "/usr/local/tomcat/logs/CofaxTools.log",
        "logMaxSize": "",
        "dataLog": 1,
        "dataLogLocation": "/usr/local/tomcat/logs/dataLog.log",
        "dataLogMaxSize": "",
        "removePageCache": "/content/admin/remove?cache=pages&id=",
        "removeTemplateCache": "/content/admin/remove?cache=templates&id=",
        "fileTransferFolder": "/usr/local/tomcat/webapps/content/fileTransferFolder",
        "lookInContext": 1,
        "adminGroupID": 4,
        "betaServer": true}}],
  "servlet-mapping": {
    "cofaxCDS": "/",
    "cofaxEmail": "/cofaxutil/aemail/*",
    "cofaxAdmin": "/admin/*",
    "fileServlet": "/static/*",
    "cofaxTools": "/tools/*"},

  "taglib": {
    "taglib-uri": "cofax.tld",
    "taglib-location": "/WEB-INF/tlds/cofax.tld"}}}
"""

  val jsonExample2: Text = t"""
{"menu":{"id":"file","value":"File","popup":{"menuitem":[{"value":"New","onclick":"CreateNewDoc()"},
{"value":"Open","onclick":"OpenDoc()"},{"value":"Close","onclick":"CloseDoc()"}]}}}
"""

  val jsonExample3: Text = t"""
{"menu": {
  "header": "SVG Viewer",
    "items": [
        {"id": "Open"},
        {"id": "OpenNew", "label": "Open New"},
        null,
        {"id": "ZoomIn", "label": "Zoom In"},
        {"id": "ZoomOut", "label": "Zoom Out"},
        {"id": "OriginalView", "label": "Original View"},
        null,
        {"id": "Quality"},
        {"id": "Pause"},
        {"id": "Mute"},
        null,
        {"id": "Find", "label": "Find..."},
        {"id": "FindAgain", "label": "Find Again"},
        {"id": "Copy"},
        {"id": "CopyAgain", "label": "Copy Again"},
        {"id": "CopySVG", "label": "Copy SVG"},
        {"id": "ViewSVG", "label": "View SVG"},
        {"id": "ViewSource", "label": "View Source"},
        {"id": "SaveAs", "label": "Save As"},
        null,
        {"id": "Help"},
        {"id": "About", "label": "About Adobe CVG Viewer..."}
    ]
}}
"""
