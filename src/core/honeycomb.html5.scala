/*
    Honeycomb, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package honeycomb

import anticipation.*
import gossamer.*
import rudiments.*

import language.dynamics

object html5:
  val A = ClearTag["a", NonInteractive, Global | "href" | "target" | "download" | "ping" | "rel" |
      "hreflang" | "type" | "referrerpolicy"]("a")

  val Abbr = Tag["abbr", Phrasing, Global]("abbr")
  val Address = Tag["address", Flow | Palpable, Global]("address")

  object Area
  extends Tag["area", Nothing, Global | "alt" | "coords" | "shape" | "href" | "target" |
      "download" | "ping" | "rel" | "referrerpolicy"]("area"):
    val Default = preset("shape" -> t"default")
    val Rect = preset("shape" -> t"rect")
    val Circle = preset("shape" -> t"circle")
    val Poly = preset("shape" -> t"poly")

  val Article = Tag["article", Flow, Global]("article") // further constraints on descendants
  val Aside = Tag["aside", Flow, Global]("aside")

  val Audio = Tag["audio", Label, Global | "src" | "crossorigin" | "preload" | "autoplay" |
      "loop" | "muted" | "controls"]("audio")

  val B = Tag["b", Phrasing, Global]("b")
  val Base = Tag["base", Nothing, Global | "href" | "target"]("base")
  val Bdi = Tag["bdi", Phrasing, Global]("bdi")
  val Bdo = Tag["bdo", Phrasing, Global]("bdo")
  val Blockquote = Tag["blockquote", Flow, Global | "cite"]("blockquote")

  val Body = Tag["body", Flow, Global | "onafterprint" | "onbeforeprint" | "onbeforeunload" |
      "onhashchange" | "onlanguagechange" | "onmessage" | "onmessageerror" | "onoffline" |
      "ononline" | "onpagehide" | "onpageshow" | "onpopstate" | "onrejectionhandled" | "onstorage" |
      "onunhandledrejection" | "onunload"]("body")

  val Br = Tag["br", Nothing, Global]("br")

  val Button = Tag["button", Phrasing, Global | "disabled" | "form" | "formaction" |
      "formenctype" | "formmethod" | "formnovalidate" | "formtarget" | "name" | "type" | "value"]
      ("button")

  // complicated
  val Canvas = ClearTag["canvas", NonInteractive, Global | "width" | "height"]("canvas")
  val Caption = Tag["caption", Flow, Global]("caption") // no tables
  val Cite = Tag["cite", Phrasing, Global]("cite")
  val Code = Tag["code", Phrasing, Global]("code")
  val Col = Tag["col", Nothing, Global | "span"]("col")
  val Colgroup = Tag["colgroup", "col" | "template", Global | "span"]("colgroup")
  val Data = Tag["data", Phrasing, Global | "value"]("data")
  val Datalist = Tag["datalist", Phrasing | "option" | ScriptSupporting, Global]("datalist")
  val Dd = Tag["dd", Flow, Global]("dd")
  val Del = ClearTag["del", Label, Global | "cite" | "datetime"]("del")
  val Details = Tag["details", "summary" | Flow, Global | "open"]("details")
  val Dfn = Tag["dfn", Phrasing, Global]("dfn")
  val Dialog = Tag["dialog", Flow, Global | "open"]("dialog")
  val Div = Tag["div", Flow, Global]("div")
  val Dl = Tag["dl", "dt" | "dl" | "div", Global]("dl")
  val Dt = Tag["dt", Flow, Global]("dt") // further constraints
  val Em = Tag["em", Phrasing, Global]("em")
  val Embed = Tag["embed", Nothing, Global | "src" | "type" | "width" | "height"]("embed")
  val Fieldset = Tag["fieldset", "legend" | Flow, Global | "disabled" | "form" | "name"]("fieldset")
  val Figcaption = Tag["figcaption", Flow, Global]("figcaption")

  // first or last element may be figcaption, but not both
  val Figure = Tag["figure", "figcaption" | Flow, Global]("figure")
  val Footer = Tag["footer", Flow, Global]("footer")

  val Form = Tag["form", Flow, Global | "acceptCharset" | "action" | "autocomplete" | "enctype" |
      "method" | "name" | "novalidate" | "target" | "rel"]("form")

  val H1 = Tag["h1", Phrasing, Global]("h1")
  val H2 = Tag["h2", Phrasing, Global]("h2")
  val H3 = Tag["h3", Phrasing, Global]("h3")
  val H4 = Tag["h4", Phrasing, Global]("h4")
  val H5 = Tag["h5", Phrasing, Global]("h5")
  val H6 = Tag["h6", Phrasing, Global]("h6")
  val Map = ClearTag["map", Phrasing | Flow | Palpable, Global | "name"]("map")
  val Head = Tag["head", Metadata, Global]("head")
  val Header = Tag["header", Flow, Global]("header")
  val Hgroup = Tag["hgroup", "h1" | "h2" | "h3" | "h4" | "h5" | "h6", Global]("hgroup")
  val Hr = Tag["hr", Nothing, Global]("hr")
  val I = Tag["i", Phrasing, Global]("i")

  val Iframe = Tag["iframe", Nothing, Global | "src" | "srcdoc" | "name" | "sandbox" | "allow" |
      "allowfullscreen" | "width" | "height" | "referrerpolicy" | "loading"]("iframe")

  val Img = Tag["img", Nothing, Global | "alt" | "src" | "srcset" | "sizes" | "crossorigin" |
      "usemap" | "ismap" | "width" | "height" | "referrerpolicy" | "decoding" | "loading"]("img")

  object Input extends Tag["input", Nothing, Global | "accept" | "alt" | "autocomplete" |
      "checked" | "dirname" | "disabled" | "form" | "formaction" | "formenctype" | "formmethod" |
      "formnovalidate" | "formtarget" | "height" | "list" | "max" | "maxlength" | "min" |
      "minlength" | "multiple" | "name" | "pattern" | "placeholder" | "readonly" | "required" |
      "size" | "src" | "step" | "value" | "width" | "capture"]("input"):

    val Button = preset("type" -> t"button")
    val Checkbox = preset("type" -> t"checkbox")
    val Color = preset("type" -> t"color")
    val Date = preset("type" -> t"date")
    val DatetimeLocal = preset("type" -> t"datetime-local")
    val Email = preset("type" -> t"email")
    val File = preset("type" -> t"file")
    val Hidden = preset("type" -> t"hidden")
    val Image = preset("type" -> t"image")
    val Month = preset("type" -> t"month")
    val Number = preset("type" -> t"number")
    val Password = preset("type" -> t"password")
    val Radio = preset("type" -> t"radio")
    val Range = preset("type" -> t"range")
    val Reset = preset("type" -> t"reset")
    val Search = preset("type" -> t"search")
    val Submit = preset("type" -> t"submit")
    val Tel = preset("type" -> t"tel")
    val Text = preset("type" -> t"text")
    val Time = preset("type" -> t"time")
    val Url = preset("type" -> t"url")
    val Week = preset("type" -> t"week")

  val Ins = ClearTag["ins", Label, Global | "cite" | "datetime"]("ins")
  val Kbd = Tag["kbd", Phrasing, Global]("kbd")
  val Label = Tag["label", Phrasing, Global | "for" | "for"]("label")
  val Legend = Tag["legend", Phrasing | Heading, Global]("legend")
  val Li = Tag["li", Flow, Global | "value"]("li")

  object Link extends Tag
     ["link",
      Nothing,
      Global
      | "href"
      | "crossorigin"
      | "media"
      | "integrity"
      | "hreflang"
      | "type"
      | "referrerpolicy"
      | "sizes"
      | "imagesrcset"
      | "imagesizes"
      | "as"
      | "color"
      | "disabled"]
     ("link"):

    val Stylesheet = preset("rel" -> t"stylesheet")
    val Icon = preset("rel" -> t"icon")
    val Manifest = preset("rel" -> t"manifest")
    val Alternate = preset("rel" -> t"alternate")
    val Prev = preset("rel" -> t"prev")
    val Next = preset("rel" -> t"next")
    val Canonical = preset("rel" -> t"canonical")
    val Nofollow = preset("rel" -> t"nofollow")
    val Noreferrer = preset("rel" -> t"noreferrer")
    val Noopener = preset("rel" -> t"noopener")
    val Tag = preset("rel" -> t"tag")
    val Preload = preset("rel" -> t"preload")
    val Prefetch = preset("rel" -> t"prefetch")
    val DnsPrefetch = preset("rel" -> t"dns-prefetch")
    val Preconnect = preset("rel" -> t"preconnect")
    val Prerender = preset("rel" -> t"prerender")
    val Author = preset("rel" -> t"author")
    val Help = preset("rel" -> t"help")
    val License = preset("rel" -> t"license")
    val Bookmark = preset("rel" -> t"bookmark")
    val Ugc = preset("rel" -> t"ugc")
    val Hub = preset("rel" -> t"hub")
    val Modulepreload = preset("rel" -> t"modulepreload")
    val Archives = preset("rel" -> t"archives")
    val Feed = preset("rel" -> t"feed")
    val Pingback = preset("rel" -> t"pingback")
    val Shortlink = preset("rel" -> t"shortlink")
    val Sidebar = preset("rel" -> t"sidebar")

  val Main = Tag["main", Flow, Global]("main")
  val Mark = Tag["mark", Phrasing, Global]("mark")
  val Menu = Tag["menu", Flow, Global]("menu")

  val Meta = Tag["meta", Nothing, Global | "name" | "httpEquiv" | "content" | "charset"]("meta")

  val Meter = Tag["meter", Phrasing, Global | "value" | "min" | "max" | "low" | "high" |
      "optimum"]("meter")

  val Nav = Tag["nav", Flow, Global]("nav")

  val Noscript = Tag["noscript", Label, Global]("noscript")

  val HObject = Tag["object", Label, Global | "data" | "type" | "name" | "form" | "width" |
      "height"]("object")

  val Ol = Tag["ol", "li" | ScriptSupporting, Global | "reversed" | "start" | "type"]("ol")

  val Optgroup = Tag["optgroup", "option" | ScriptSupporting, Global | "disabled" | "label"]
      ("optgroup")

  val Option =
    Tag["option", Nothing, Global | "disabled" | "label" | "selected" | "value"]("option")

  val Output = Tag["output", Phrasing, Global | "for" | "form" | "name"]("output")
  val P = Tag["p", Phrasing, Global]("p")
  val Param = Tag["param", Nothing, Global | "name" | "value"]("param")
  val Picture = Tag["picture", "source" | "img" | ScriptSupporting, Global]("picture")
  val Pre = Tag["pre", Phrasing, Global]("pre")
  val Progress = Tag["progress", Phrasing, Global | "value" | "max"]("progress")
  val Q = Tag["q", Phrasing, Global | "cite"]("q")
  val Rb = Tag["rb", Phrasing, Global]("rb")
  val Rp = Tag["rp", Nothing, Global]("rp")
  val Rt = Tag["rt", Phrasing, Global]("rt")
  val Ruby = Tag["ruby", Phrasing | "rp" | "rt", Global]("ruby")
  val S = Tag["s", Phrasing, Global]("s")
  val Samp = Tag["samp", Phrasing, Global]("samp")

  val Script = Tag["script", Nothing, Global | "src" | "type" | "nomodule" | "async" | "defer" |
      "crossorigin" | "integrity" | "referrerpolicy"]("script")

  val Section = Tag["section", Flow, Global]("section")

  val Select = Tag["select", "option" | "optgroup" | ScriptSupporting, Global | "autocomplete" |
      "disabled" | "form" | "multiple" | "name" | "required" | "size"]("select")

  val Slot = ClearTag["slot", Label, Global | "name"]("slot")
  val Small = Tag["small", Phrasing, Global]("small")

  val Source = Tag["source", Nothing, Global | "type" | "src" | "srcset" | "sizes" | "media" |
      "width" | "height"]("source")

  val Span = Tag["span", Phrasing, Global]("span")
  val Strong = Tag["strong", Phrasing, Global | "media"]("strong")
  val Style = Tag["style", Nothing, Global]("style")
  val Sub = Tag["sub", Phrasing, Global]("sub")
  val Summary = Tag["summary", Phrasing | Heading, Global]("summary")
  val Sup = Tag["sup", Phrasing, Global]("sup")

  val Table = Tag["table", "caption" | "colgroup" | "thead" | "tbody" | "tr" | "tfoot" |
      ScriptSupporting, Global]("table")

  val Tbody = Tag["tbody", "tr" | ScriptSupporting, Global]("tbody")
  val Td = Tag["td", Flow, Global | "colspan" | "rowspan" | "headers"]("td")
  val Template = Tag["template", Nothing, Global]("template")

  val Textarea =
    Tag
     ["textarea",
      Nothing,
      Global | "autocomplete" | "cols" | "dirname" | "disabled" | "form" | "maxlength" | "minlength" |
          "name" | "readonly" | "placeholder" | "required" | "rows" | "wrap"]
     ("textarea")

  val Tfoot = Tag["tfoot", "tr" | ScriptSupporting, Global]("tfoot")

  object Th
  extends Tag["th", Flow, Global | "colspan" | "rowspan" | "headers" | "scope" | "abbr"]("th"):
    val Col = preset("scope" -> t"col")
    val Colgroup = preset("scope" -> t"colgroup")
    val Row = preset("scope" -> t"row")
    val Rowgroup = preset("scope" -> t"rowgroup")

  val Thead = Tag["thead", "tr" | ScriptSupporting, Global]("thead")
  val Time = Tag["time", Phrasing, Global | "datetime"]("time")
  val Title = Tag["title", Nothing, Global]("title")
  val Tr = Tag["tr", "td" | "th" | ScriptSupporting, Global]("tr")

  object Track
  extends Tag["track", Nothing, Global | "kind" | "src" | "srclang" | "label" | "default"]("track"):
    val Captions = preset("kind" -> t"captions")
    val Chapters = preset("kind" -> t"chapters")
    val Descriptions = preset("kind" -> t"descriptions")
    val Metadata = preset("kind" -> t"metadata")
    val Subtitles = preset("kind" -> t"subtitles")

  val U = Tag["u", Phrasing, Global]("u")
  val Ul = Tag["ul", "li" | ScriptSupporting, Global]("ul")
  val Var = Tag["var", Nothing, Global]("var")

  val Video = Tag["video", Label, Global | "src" | "crossorigin" | "poster" | "preload" | "loop" |
      "autoplay" | "playsinline" | "muted" | "controls" | "width" | "height"]("video")

  val Wbr = Tag["wbr", Nothing, Global]("wbr")
