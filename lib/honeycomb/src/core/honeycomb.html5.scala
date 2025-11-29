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
┃    Soundness, version 0.46.0.                                                                    ┃
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
package honeycomb

import anticipation.*
import gossamer.*
import proscenium.*
import vacuous.*

import language.dynamics

object html5:
  type Interactive =
    "a" | "audio" | "button" | "details" | "embed" | "iframe" | "img" | "input" | "label" | "select"
    | "textarea" | "video"

  type Noninteractive =
    "abbr" | "address" | "area" | "article" | "aside" | "audio" | "b" | "base" | "bdi" | "bdo"
    | "blockquote" | "br" | "canvas" | "cite" | "code" | "data" | "datalist" | "del" | "dfn"
    | "dialog" | "div" | "dl" | "em" | "fieldset" | "figure" | "footer" | "form" | "h1" | "h2"
    | "h3" | "h4" | "h5" | "h6" | "header" | "hgroup" | "hr" | "i" | "img" | "input" | "ins"
    | "kbd" | "link" | "main" | "hmap" | "mark" | "menu" | "meta" | "meter" | "nav" | "noscript"
    | "object" | "ol" | "output" | "p" | "picture" | "pre" | "progress" | "q" | "ruby" | "s"
    | "samp" | "script"
    | "section" | "slot" | "small" | "span" | "strong" | "style" | "sub" | "sup" | "table"
    | "template" | "time" | "title" | "u" | "ul" | "var" | "wbr"

  type Global =
    "accesskey" | "autocapitalize" | "autofocus" | "contenteditable" | "dir" | "draggable"
    | "enterkeyhint" | "class" | "hidden" | "id" | "inputmode" | "is" | "itemid" | "itemprop"
    | "itemref" | "itemscope" | "itemtype" | "lang" | "nonce" | "spellcheck" | "style" | "tabindex"
    | "title" | "translate" | "name" | EventHandlers

  type Flow =
    "a" | "abbr" | "address" | "area" | "article" | "aside" | "audio" | "b" | "bdi" | "bdo"
    | "blockquote" | "br" | "button" | "canvas" | "cite" | "code" | "data" | "datalist" | "del"
    | "details" | "dfn" | "dialog" | "div" | "dl" | "em" | "embed" | "fieldset" | "figure"
    | "footer" | "form" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "header" | "hgroup" | "hr"
    | "i" | "iframe" | "img" | "input" | "ins" | "kbd" | "label" | "link" | "main" | "hmap"
    | "mark" | "menu" | "meta" | "meter" | "nav" | "noscript" | "object" | "ol" | "output" | "p"
    | "picture" | "pre" | "progress" | "q" | "ruby" | "s" | "samp" | "script" | "section" | "select"
    | "slot" | "small" | "span" | "strong" | "sub" | "sup" | "table" | "template" | "textarea"
    | "time" | "u" | "ul" | "var"
    | "video" | "wbr"

  type Palpable =
    "a" | "abbr" | "address" | "article" | "aside" | "audio" | "b" | "bdi" | "bdo"| "blockquote"
    | "button" | "canvas" | "cite" | "code" | "data" | "details" | "dfn" | "div" | "dl" | "em"
    | "embed" | "fieldset" | "figure" | "footer" | "form" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6"
    | "header" | "hgroup" | "i" | "iframe" | "img" | "input" | "ins" | "kbd" | "label" | "main"
    | "hmap" | "mark" | "menu" | "meter" | "nav" | "object" | "ol" | "output" | "p" | "pre"
    | "progress" | "q" | "ruby" | "s" | "samp" | "section" | "select" | "small" | "span" | "strong"
    | "sub" | "sup" | "table" | "textarea" | "time" | "u" | "ul" | "var" | "video"

  type Phrasing =
    "a" | "abbr" | "area" | "audio" | "b" | "bdi" | "bdo" | "br" | "button" | "canvas" | "cite"
    | "code" | "data" | "datalist" | "del" | "dfn" | "em" | "embed" | "i" | "iframe" | "img"
    | "input" | "ins" | "kbd" | "label" | "link" | "hmap" | "mark" | "meta" | "meter" | "noscript"
    | "object" | "output" | "picture" | "progress" | "q" | "ruby" | "s" | "samp" | "script"
    | "select" | "slot"
    | "small" | "span" | "strong" | "sub" | "sup" | "template" | "textarea" | "time" | "u" | "var"
    | "video" | "wbr"

  type EventHandlers =
    "onabort" | "onauxclick" | "oncancel" | "oncanplay" | "oncanplaythrough" | "onchange"
    | "onclick" | "onclose" | "oncontextmenu" | "oncuechange" | "ondblclick" | "ondrag"
    | "ondragend" | "ondragenter" | "ondragleave" | "ondragover" | "ondragstart" | "ondrop"
    | "ondurationchange" | "onemptied" | "onended" | "onformdata" | "oninput" | "oninvalid"
    | "onkeydown" | "onkeypress" | "onkeyup" | "onloadeddata" | "onloadedmetadata" | "onloadstart"
    | "onmousedown" | "onmouseenter" | "onmouseleave" | "onmousemove" | "onmouseout" | "onmouseover"
    | "onmouseup" | "onpause" | "onplay" | "onplaying" | "onprogress" | "onratechange" | "onreset"
    | "onsecuritypolicyviolation" | "onseeked" | "onseeking" | "onselect" | "onslotchange"
    | "onstalled" | "onsubmit" | "onsuspend" | "ontimeupdate" | "ontoggle" | "onvolumechange"
    | "onwaiting" | "onwebkitanimationend" | "onwebkitanimationiteration" | "onwebkitanimationstart"
    | "onwebkittransitionend" | "onwheel"
    | "onblur" | "onerror" | "onfocus" | "onload" | "onresize" | "onscroll"

  type Metadata = "base" | "link" | "meta" | "noscript" | "script" | "style" | "template" | "title"
  type Heading = "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "hgroup"
  type Embedded = "audio" | "canvas" | "embed" | "iframe" | "img" | "object" | "picture" | "video"
  type Sectioning = "article" | "aside" | "nav" | "section"
  type ScriptSupporting = "script" | "template"

  val A = ClearTag["a", Noninteractive, Global | "href" | "target" | "download" | "ping" | "rel" |
      "hreflang" | "type" | "referrerpolicy"]("a")

  val Abbr = OldTag["abbr", Phrasing, Global]("abbr")
  val Address = OldTag["address", Flow | Palpable, Global]("address")

  object Area
  extends OldTag["area", Nothing, Global | "alt" | "coords" | "shape" | "href" | "target" |
      "download" | "ping" | "rel" | "referrerpolicy"]("area"):
    val Default = preset("shape" -> t"default")
    val Rect = preset("shape" -> t"rect")
    val Circle = preset("shape" -> t"circle")
    val Poly = preset("shape" -> t"poly")

  val Article = OldTag["article", Flow, Global]("article") // further constraints on descendants
  val Aside = OldTag["aside", Flow, Global]("aside")

  val Audio = OldTag["audio", Label, Global | "src" | "crossorigin" | "preload" | "autoplay" |
      "loop" | "muted" | "controls"]("audio")

  val B = OldTag["b", Phrasing, Global]("b")
  val Base = OldTag["base", Nothing, Global | "href" | "target"]("base")
  val Bdi = OldTag["bdi", Phrasing, Global]("bdi")
  val Bdo = OldTag["bdo", Phrasing, Global]("bdo")
  val Blockquote = OldTag["blockquote", Flow, Global | "cite"]("blockquote")

  val Body = OldTag["body", Flow, Global | "onafterprint" | "onbeforeprint" | "onbeforeunload" |
      "onhashchange" | "onlanguagechange" | "onmessage" | "onmessageerror" | "onoffline" |
      "ononline" | "onpagehide" | "onpageshow" | "onpopstate" | "onrejectionhandled" | "onstorage" |
      "onunhandledrejection" | "onunload"]("body")

  val Br = OldTag["br", Nothing, Global]("br")

  val Button =
    OldTag
     ["button",
      Phrasing, Global | "disabled" | "form" | "formaction" | "formenctype" | "formmethod"
      | "formnovalidate" | "formtarget" | "name" | "type" | "value"]
     ("button")

  // complicated
  val Canvas = ClearTag["canvas", Noninteractive, Global | "width" | "height"]("canvas")

  val Caption = OldTag["caption", Flow, Global]("caption") // no tables
  val Cite = OldTag["cite", Phrasing, Global]("cite")
  val Code = OldTag["code", Phrasing, Global]("code")
  val Col = OldTag["col", Nothing, Global | "span"]("col")
  val Colgroup = OldTag["colgroup", "col" | "template", Global | "span"]("colgroup")
  val Data = OldTag["data", Phrasing, Global | "value"]("data")
  val Datalist = OldTag["datalist", Phrasing | "option" | ScriptSupporting, Global]("datalist")
  val Dd = OldTag["dd", Flow, Global]("dd")
  val Del = ClearTag["del", Label, Global | "cite" | "datetime"]("del")
  val Details = OldTag["details", "summary" | Flow, Global | "open"]("details")
  val Dfn = OldTag["dfn", Phrasing, Global]("dfn")
  val Dialog = OldTag["dialog", Flow, Global | "open"]("dialog")
  val Div = OldTag["div", Flow, Global]("div")
  val Dl = OldTag["dl", "dt" | "dl" | "div", Global]("dl")
  val Dt = OldTag["dt", Flow, Global]("dt") // further constraints
  val Em = OldTag["em", Phrasing, Global]("em")
  val Embed = OldTag["embed", Nothing, Global | "src" | "type" | "width" | "height"]("embed")
  val Fieldset = OldTag["fieldset", "legend" | Flow, Global | "disabled" | "form" | "name"]("fieldset")
  val Figcaption = OldTag["figcaption", Flow, Global]("figcaption")

  // first or last element may be figcaption, but not both
  val Figure = OldTag["figure", "figcaption" | Flow, Global]("figure")

  val Footer = OldTag["footer", Flow, Global]("footer")

  val Form = OldTag["form", Flow, Global | "acceptCharset" | "action" | "autocomplete" | "enctype" |
      "method" | "name" | "novalidate" | "target" | "rel"]("form")

  val H1 = OldTag["h1", Phrasing, Global]("h1")
  val H2 = OldTag["h2", Phrasing, Global]("h2")
  val H3 = OldTag["h3", Phrasing, Global]("h3")
  val H4 = OldTag["h4", Phrasing, Global]("h4")
  val H5 = OldTag["h5", Phrasing, Global]("h5")
  val H6 = OldTag["h6", Phrasing, Global]("h6")
  val Map = ClearTag["map", Phrasing | Flow | Palpable, Global | "name"]("map")
  val Head = OldTag["head", Metadata, Global]("head")
  val Header = OldTag["header", Flow, Global]("header")
  val Hgroup = OldTag["hgroup", "h1" | "h2" | "h3" | "h4" | "h5" | "h6", Global]("hgroup")
  val Hr = OldTag["hr", Nothing, Global]("hr")
  val I = OldTag["i", Phrasing, Global]("i")

  val Iframe = OldTag["iframe", Nothing, Global | "src" | "srcdoc" | "name" | "sandbox" | "allow" |
      "allowfullscreen" | "width" | "height" | "referrerpolicy" | "loading"]("iframe")

  val Img = OldTag["img", Nothing, Global | "alt" | "src" | "srcset" | "sizes" | "crossorigin" |
      "usemap" | "ismap" | "width" | "height" | "referrerpolicy" | "decoding" | "loading"]("img")

  object Input extends OldTag["input", Nothing, Global | "accept" | "alt" | "autocomplete" |
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
  val Kbd = OldTag["kbd", Phrasing, Global]("kbd")
  val Label = OldTag["label", Phrasing, Global | "for" | "for"]("label")
  val Legend = OldTag["legend", Phrasing | Heading, Global]("legend")
  val Li = OldTag["li", Flow, Global | "value"]("li")

  object Link extends OldTag
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

  val Main = OldTag["main", Flow, Global]("main")
  val Mark = OldTag["mark", Phrasing, Global]("mark")
  val Menu = OldTag["menu", Flow, Global]("menu")

  val Meta = OldTag["meta", Nothing, Global | "name" | "httpEquiv" | "content" | "charset"]("meta")

  val Meter = OldTag["meter", Phrasing, Global | "value" | "min" | "max" | "low" | "high" |
      "optimum"]("meter")

  val Nav = OldTag["nav", Flow, Global]("nav")

  val Noscript = OldTag["noscript", Label, Global]("noscript")

  val HObject = OldTag["object", Label, Global | "data" | "type" | "name" | "form" | "width" |
      "height"]("object")

  val Ol = OldTag["ol", "li" | ScriptSupporting, Global | "reversed" | "start" | "type"]("ol")

  val Optgroup =
    OldTag["optgroup", "option" | ScriptSupporting, Global | "disabled" | "label"]("optgroup")

  val Option =
    OldTag["option", Nothing, Global | "disabled" | "label" | "selected" | "value"]("option")

  val Output = OldTag["output", Phrasing, Global | "for" | "form" | "name"]("output")
  val P = OldTag["p", Phrasing, Global]("p")
  val Param = OldTag["param", Nothing, Global | "name" | "value"]("param")
  val Picture = OldTag["picture", "source" | "img" | ScriptSupporting, Global]("picture")
  val Pre = OldTag["pre", Phrasing, Global]("pre")
  val Progress = OldTag["progress", Phrasing, Global | "value" | "max"]("progress")
  val Q = OldTag["q", Phrasing, Global | "cite"]("q")
  val Rb = OldTag["rb", Phrasing, Global]("rb")
  val Rp = OldTag["rp", Nothing, Global]("rp")
  val Rt = OldTag["rt", Phrasing, Global]("rt")
  val Ruby = OldTag["ruby", Phrasing | "rp" | "rt", Global]("ruby")
  val S = OldTag["s", Phrasing, Global]("s")
  val Samp = OldTag["samp", Phrasing, Global]("samp")

  val Script = OldTag["script", Nothing, Global | "src" | "type" | "nomodule" | "async" | "defer" |
      "crossorigin" | "integrity" | "referrerpolicy"]("script")

  val Section = OldTag["section", Flow, Global]("section")

  val Select = OldTag["select", "option" | "optgroup" | ScriptSupporting, Global | "autocomplete" |
      "disabled" | "form" | "multiple" | "name" | "required" | "size"]("select")

  val Slot = ClearTag["slot", Label, Global | "name"]("slot")
  val Small = OldTag["small", Phrasing, Global]("small")

  val Source = OldTag["source", Nothing, Global | "type" | "src" | "srcset" | "sizes" | "media" |
      "width" | "height"]("source")

  val Span = OldTag["span", Phrasing, Global]("span")
  val Strong = OldTag["strong", Phrasing, Global | "media"]("strong")
  val Style = OldTag["style", Nothing, Global]("style")
  val Sub = OldTag["sub", Phrasing, Global]("sub")
  val Summary = OldTag["summary", Phrasing | Heading, Global]("summary")
  val Sup = OldTag["sup", Phrasing, Global]("sup")

  val Table = OldTag["table", "caption" | "colgroup" | "thead" | "tbody" | "tr" | "tfoot" |
      ScriptSupporting, Global]("table")

  val Tbody = OldTag["tbody", "tr" | ScriptSupporting, Global]("tbody")
  val Td = OldTag["td", Flow, Global | "colspan" | "rowspan" | "headers"]("td")
  val Template = OldTag["template", Nothing, Global]("template")

  val Textarea =
    OldTag
     ["textarea",
      Nothing,
      Global | "autocomplete" | "cols" | "dirname" | "disabled" | "form" | "maxlength"
      | "minlength" | "name" | "readonly" | "placeholder" | "required" | "rows" | "wrap"]
     ("textarea")

  val Tfoot = OldTag["tfoot", "tr" | ScriptSupporting, Global]("tfoot")

  object Th
  extends OldTag["th", Flow, Global | "colspan" | "rowspan" | "headers" | "scope" | "abbr"]("th"):
    val Col = preset("scope" -> t"col")
    val Colgroup = preset("scope" -> t"colgroup")
    val Row = preset("scope" -> t"row")
    val Rowgroup = preset("scope" -> t"rowgroup")

  val Thead = OldTag["thead", "tr" | ScriptSupporting, Global]("thead")
  val Time = OldTag["time", Phrasing, Global | "datetime"]("time")
  val Title = OldTag["title", Nothing, Global]("title")
  val Tr = OldTag["tr", "td" | "th" | ScriptSupporting, Global]("tr")

  object Track
  extends OldTag["track", Nothing, Global | "kind" | "src" | "srclang" | "label" | "default"]("track"):
    val Captions = preset("kind" -> t"captions")
    val Chapters = preset("kind" -> t"chapters")
    val Descriptions = preset("kind" -> t"descriptions")
    val Metadata = preset("kind" -> t"metadata")
    val Subtitles = preset("kind" -> t"subtitles")

  val U = OldTag["u", Phrasing, Global]("u")
  val Ul = OldTag["ul", "li" | ScriptSupporting, Global]("ul")
  val Var = OldTag["var", Nothing, Global]("var")

  val Video = OldTag["video", Label, Global | "src" | "crossorigin" | "poster" | "preload" | "loop" |
      "autoplay" | "playsinline" | "muted" | "controls" | "width" | "height"]("video")

  val Wbr = OldTag["wbr", Nothing, Global]("wbr")
