/*
    Merino
jawn, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package merino

import probably.*
import gossamer.*
//import galilei.*, filesystems.unix
import anticipation.* //, fileApi.galileiApi
import eucalyptus.*
import rudiments.*
import hieroglyph.*, characterEncodings.utf8, badEncodingHandlers.strict
import parasite.*, monitors.global
import turbulence.*, stdioSources.virtualMachine
import unsafeExceptions.canThrowAny
import ambience.*, environments.system

import LogFormat.standardAnsi

val OutSink = Out.sink
given Log({ case _ => OutSink })

object Benchmarks extends Suite(t"Merino tests"):
  def run(): Unit =
    suite(t"Parse example 1"):
      test(t"Parse file with Jawn"):
        import org.typelevel.jawn.*, ast.*
        JParser.parseFromByteBuffer(java.nio.ByteBuffer.wrap(jsonExample1).nn)
      .benchmark(warmup = 1000L, duration = 1000L, baseline = Baseline(compare = Min), confidence = 99)
      
      test(t"Parse file with Merino"):
        JsonAst.parse(jsonExample1.nn.immutable(using Unsafe))
      .benchmark(warmup = 1000L, duration = 1000L, confidence = 99)
    
    suite(t"Parse example 2"):
      test(t"Parse file with Jawn"):
        import org.typelevel.jawn.*, ast.*
        JParser.parseFromByteBuffer(java.nio.ByteBuffer.wrap(jsonExample2).nn)
      .benchmark(warmup = 1000L, duration = 1000L, baseline = Baseline(compare = Min), confidence = 99)
      
      test(t"Parse file with Merino"):
        JsonAst.parse(jsonExample2.nn.immutable(using Unsafe))
      .benchmark(warmup = 1000L, duration = 1000L, confidence = 99)
    
    suite(t"Parse example 3"):
      test(t"Parse file with Jawn"):
        import org.typelevel.jawn.*, ast.*
        JParser.parseFromByteBuffer(java.nio.ByteBuffer.wrap(jsonExample3).nn)
      .benchmark(warmup = 1000L, duration = 1000L, baseline = Baseline(compare = Min), confidence = 99)
      
      test(t"Parse file with Merino"):
        JsonAst.parse(jsonExample3.nn.immutable(using Unsafe))
      .benchmark(warmup = 1000L, duration = 1000L, confidence = 99)
        
given realm: Realm = Realm(t"tests")

lazy val jsonExample1 = """

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
""".getBytes("UTF-8")

val jsonExample2 = """
{"menu":{"id":"file","value":"File","popup":{"menuitem":[{"value":"New","onclick":"CreateNewDoc()"},{"value":"Open","onclick":"OpenDoc()"},{"value":"Close","onclick":"CloseDoc()"}]}}}
""".getBytes("UTF-8")

lazy val jsonExample3 = """
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
""".getBytes("UTF-8")

