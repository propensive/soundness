/*
    Honeycomb, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

extension (sc: StringContext)
  def cls(): CssClass = CssClass(sc.parts.head.tt)
  def id(): DomId = DomId(sc.parts.head.tt)

val A = ClearTagType["a", NonInteractive, Global | "href" | "target" | "download" | "ping" | "rel" |
    "hreflang" | "type" | "referrerpolicy"]("a", block = false)

val Abbr = TagType["abbr", Phrasing, Global]("abbr", block = false)
val Address = TagType["address", Flow | Palpable, Global]("address")

object Area
extends TagType["area", Nothing, Global | "alt" | "coords" | "shape" | "href" | "target" |
    "download" | "ping" | "rel" | "referrerpolicy"]("area", block = false):
  val Default = preset("shape" -> t"default")
  val Rect = preset("shape" -> t"rect")
  val Circle = preset("shape" -> t"circle")
  val Poly = preset("shape" -> t"poly")

val Article = TagType["article", Flow, Global]("article") // further constraints on descendants
val Aside = TagType["aside", Flow, Global]("aside")

val Audio = TagType["audio", Label, Global | "src" | "crossorigin" | "preload" | "autoplay" |
    "loop" | "muted" | "controls"]("audio", block = false)

val B = TagType["b", Phrasing, Global]("b", block = false)
val Base = TagType["base", Nothing, Global | "href" | "target"]("base", block = false)
val Bdi = TagType["bdi", Phrasing, Global]("bdi", block = false)
val Bdo = TagType["bdo", Phrasing, Global]("bdo", block = false)
val Blockquote = TagType["blockquote", Flow, Global | "cite"]("blockquote")

val Body = TagType["body", Flow, Global | "onafterprint" | "onbeforeprint" | "onbeforeunload" |
    "onhashchange" | "onlanguagechange" | "onmessage" | "onmessageerror" | "onoffline" |
    "ononline" | "onpagehide" | "onpageshow" | "onpopstate" | "onrejectionhandled" | "onstorage" |
    "onunhandledrejection" | "onunload"]("body")

val Br = TagType["br", Nothing, Global]("br", block = false, unclosed = true)

val Button = TagType["button", Phrasing, Global | "disabled" | "form" | "formaction" |
    "formenctype" | "formmethod" | "formnovalidate" | "formtarget" | "name" | "type" | "value"]
    ("button", block = false)

val Canvas = ClearTagType["canvas", NonInteractive, Global | "width" | "height"]("canvas", block =
    false) // complicated

val Caption = TagType["caption", Flow, Global]("caption") // no tables
val Cite = TagType["cite", Phrasing, Global]("cite", block = false)
val Code = TagType["code", Phrasing, Global]("code", block = false)
val Col = TagType["col", Nothing, Global | "span"]("col", block = false)
val Colgroup = TagType["colgroup", "col" | "template", Global | "span"]("colgroup")
val Data = TagType["data", Phrasing, Global | "value"]("data", block = false)

val Datalist = TagType["datalist", Phrasing | "option" | ScriptSupporting, Global]("datalist",
    block = false)

val Dd = TagType["dd", Flow, Global]("dd")
val Del = ClearTagType["del", Label, Global | "cite" | "datetime"]("del", block = false)
val Details = TagType["details", "summary" | Flow, Global | "open"]("details")
val Dfn = TagType["dfn", Phrasing, Global]("dfn", block = false)
val Dialog = TagType["dialog", Flow, Global | "open"]("dialog")
val Div = TagType["div", Flow, Global]("div")
val Dl = TagType["dl", "dt" | "dl" | "div", Global]("dl")
val Dt = TagType["dt", Flow, Global]("dt") // further constraints
val Em = TagType["em", Phrasing, Global]("em", block = false)

val Embed = TagType["embed", Nothing, Global | "src" | "type" | "width" | "height"]("embed",
    block = false)

val Fieldset =
  TagType["fieldset", "legend" | Flow, Global | "disabled" | "form" | "name"]("fieldset")

val Figcaption = TagType["figcaption", Flow, Global]("figcaption")

// first or last element may be figcaption, but not both
val Figure = TagType["figure", "figcaption" | Flow, Global]("figure")

val Footer = TagType["footer", Flow, Global]("footer")

val Form = TagType["form", Flow, Global | "acceptCharset" | "action" | "autocomplete" | "enctype" |
    "method" | "name" | "novalidate" | "target" | "rel"]("form")

val H1 = TagType["h1", Phrasing, Global]("h1")
val H2 = TagType["h2", Phrasing, Global]("h2")
val H3 = TagType["h3", Phrasing, Global]("h3")
val H4 = TagType["h4", Phrasing, Global]("h4")
val H5 = TagType["h5", Phrasing, Global]("h5")
val H6 = TagType["h6", Phrasing, Global]("h6")
val HMap = ClearTagType["map", Phrasing | Flow | Palpable, Global | "name"]("map", block = false)
val Head = TagType["head", Metadata, Global]("head")
val Header = TagType["header", Flow, Global]("header")
val Hgroup = TagType["hgroup", "h1" | "h2" | "h3" | "h4" | "h5" | "h6", Global]("hgroup")
val Hr = TagType["hr", Nothing, Global]("hr", block = false, unclosed = true)
val I = TagType["i", Phrasing, Global]("i", block = false)

val Iframe = TagType["iframe", Nothing, Global | "src" | "srcdoc" | "name" | "sandbox" | "allow" |
    "allowfullscreen" | "width" | "height" | "referrerpolicy" | "loading"]("iframe", unclosed =
     false, block = false)

val Img = TagType["img", Nothing, Global | "alt" | "src" | "srcset" | "sizes" | "crossorigin" |
    "usemap" | "ismap" | "width" | "height" | "referrerpolicy" | "decoding" | "loading"]("img",
    block = false, unclosed = true)

object Input extends TagType["input", Nothing, Global | "accept" | "alt" | "autocomplete" |
    "checked" | "dirname" | "disabled" | "form" | "formaction" | "formenctype" | "formmethod" |
    "formnovalidate" | "formtarget" | "height" | "list" | "max" | "maxlength" | "min" |
    "minlength" | "multiple" | "name" | "pattern" | "placeholder" | "readonly" | "required" |
    "size" | "src" | "step" | "value" | "width" | "capture"]("input", block = false,
    unclosed = true):
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

val Ins = ClearTagType["ins", Label, Global | "cite" | "datetime"]("ins", block = false)
val Kbd = TagType["kbd", Phrasing, Global]("kbd", block = false)
val Label = TagType["label", Phrasing, Global | "for" | "for"]("label", block = false)
val Legend = TagType["legend", Phrasing | Heading, Global]("legend")
val Li = TagType["li", Flow, Global | "value"]("li")

object Link extends TagType["link", Nothing, Global | "href" | "crossorigin" | "media" |
    "integrity" | "hreflang" | "type" | "referrerpolicy" | "sizes" | "imagesrcset" | "imagesizes" |
    "as" | "color" | "disabled"]("link", block = false, unclosed = true):
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

val Main = TagType["main", Flow, Global]("main")
val Mark = TagType["mark", Phrasing, Global]("mark", block = false)
val Menu = TagType["menu", Flow, Global]("menu")

val Meta = TagType["meta", Nothing, Global | "name" | "httpEquiv" | "content" | "charset"]("meta",
    block = false, unclosed = true)

val Meter = TagType["meter", Phrasing, Global | "value" | "min" | "max" | "low" | "high" |
    "optimum"]("meter", block = false)

val Nav = TagType["nav", Flow, Global]("nav")

val Noscript = TagType["noscript", Label, Global]("noscript", block = false)

val HObject = TagType["object", Label, Global | "data" | "type" | "name" | "form" | "width" |
    "height"]("object", block = false)

val Ol = TagType["ol", "li" | ScriptSupporting, Global | "reversed" | "start" | "type"]("ol")

val Optgroup = TagType["optgroup", "option" | ScriptSupporting, Global | "disabled" | "label"]
    ("optgroup")

val Option = TagType["option", Nothing, Global | "disabled" | "label" | "selected" | "value"]
    ("option", unclosed = true)

val Output = TagType["output", Phrasing, Global | "for" | "form" | "name"]("output", block = false)
val P = TagType["p", Phrasing, Global]("p")
val Param = TagType["param", Nothing, Global | "name" | "value"]("param", unclosed = true)

val Picture = TagType["picture", "source" | "img" | ScriptSupporting, Global]("picture", block =
    false)

val Pre = TagType["pre", Phrasing, Global]("pre", verbatim = true, block = false)
val Progress = TagType["progress", Phrasing, Global | "value" | "max"]("progress", block = false)
val Q = TagType["q", Phrasing, Global | "cite"]("q", block = false)
val Rb = TagType["rb", Phrasing, Global]("rb")
val Rp = TagType["rp", Nothing, Global]("rp")
val Rt = TagType["rt", Phrasing, Global]("rt")
val Ruby = TagType["ruby", Phrasing | "rp" | "rt", Global]("ruby", block = false)
val S = TagType["s", Phrasing, Global]("s", block = false)
val Samp = TagType["samp", Phrasing, Global]("samp", block = false)

val Script = TagType["script", Nothing, Global | "src" | "type" | "nomodule" | "async" | "defer" |
    "crossorigin" | "integrity" | "referrerpolicy"]("script", block = false, verbatim = true)

val Section = TagType["section", Flow, Global]("section")

val Select = TagType["select", "option" | "optgroup" | ScriptSupporting, Global | "autocomplete" |
    "disabled" | "form" | "multiple" | "name" | "required" | "size"]("select", block = false)

val Slot = ClearTagType["slot", Label, Global | "name"]("slot", block = false)
val Small = TagType["small", Phrasing, Global]("small", block = false)

val Source = TagType["source", Nothing, Global | "type" | "src" | "srcset" | "sizes" | "media" |
    "width" | "height"]("source")

val Span = TagType["span", Phrasing, Global]("span", block = false)
val Strong = TagType["strong", Phrasing, Global | "media"]("strong", block = false)
val Style = TagType["style", Nothing, Global]("style")
val Sub = TagType["sub", Phrasing, Global]("sub", block = false)
val Summary = TagType["summary", Phrasing | Heading, Global]("summary")
val Sup = TagType["sup", Phrasing, Global]("sup", block = false)

val Table = TagType["table", "caption" | "colgroup" | "thead" | "tbody" | "tr" | "tfoot" |
    ScriptSupporting, Global]("table")

val Tbody = TagType["tbody", "tr" | ScriptSupporting, Global]("tbody")
val Td = TagType["td", Flow, Global | "colspan" | "rowspan" | "headers"]("td")
val Template = TagType["template", Nothing, Global]("template", unclosed = false, block = false)

val Textarea =
  TagType
   ["textarea",
    Nothing,
    Global | "autocomplete" | "cols" | "dirname" | "disabled" | "form" | "maxlength" | "minlength" |
        "name" | "readonly" | "placeholder" | "required" | "rows" | "wrap"]
   ("textarea", block = false, verbatim = true)

val Tfoot = TagType["tfoot", "tr" | ScriptSupporting, Global]("tfoot")

object Th
extends TagType["th", Flow, Global | "colspan" | "rowspan" | "headers" | "scope" | "abbr"]("th"):
  val Col = preset("scope" -> t"col")
  val Colgroup = preset("scope" -> t"colgroup")
  val Row = preset("scope" -> t"row")
  val Rowgroup = preset("scope" -> t"rowgroup")

val Thead = TagType["thead", "tr" | ScriptSupporting, Global]("thead")
val Time = TagType["time", Phrasing, Global | "datetime"]("time", block = false)
val Title = TagType["title", Nothing, Global]("title")
val Tr = TagType["tr", "td" | "th" | ScriptSupporting, Global]("tr")

object Track
extends TagType["track", Nothing, Global | "kind" | "src" | "srclang" | "label" | "default"]
   ("track"):
  val Captions = preset("kind" -> t"captions")
  val Chapters = preset("kind" -> t"chapters")
  val Descriptions = preset("kind" -> t"descriptions")
  val Metadata = preset("kind" -> t"metadata")
  val Subtitles = preset("kind" -> t"subtitles")

val U = TagType["u", Phrasing, Global]("u", block = false)
val Ul = TagType["ul", "li" | ScriptSupporting, Global]("ul")
val Var = TagType["var", Nothing, Global]("var", block = false)

val Video = TagType["video", Label, Global | "src" | "crossorigin" | "poster" | "preload" | "loop" |
    "autoplay" | "playsinline" | "muted" | "controls" | "width" | "height"]("video", block = false)

val Wbr = TagType["wbr", Nothing, Global]("wbr", block = false)
