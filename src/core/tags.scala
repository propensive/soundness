/*
    Honeycomb, version 0.9.0. Copyright 2018-22 Jon Pretty, Propensive OÜ.

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

import rudiments.*

import language.dynamics

val A = TransTagType["a", NonInteractive, Global | "href" | "target" | "download" | "ping" | "rel" |
                         "hreflang" | "htype" | "referrerpolicy"]
                    ("a", inline = true)

val Abbr = TagType["abbr", Phrasing, Global]("abbr", inline = true)
val Address = TagType["address", Flow | Palpable, Global]("address")

val Area = TagType["area", Nothing, Global | "alt" | "coords" | "shape" | "href" | "target" |
                       "download" | "ping" | "rel" | "referrerpolicy"]
                  ("area", inline = true)

val Article = TagType["article", Flow, Global]("article") // further constraints on descendants
val Aside = TagType["aside", Flow, Global]("aside")

val Audio = TagType["audio", Label, Global | "src" | "crossorigin" | "preload" | "autoplay" |
                        "loop" | "muted" | "controls"]
                   ("audio", inline = true)

val B = TagType["b", Phrasing, Global]("b", inline = true)
val Base = TagType["base", Nothing, Global | "href" | "target"]("base", inline = true)
val Bdi = TagType["bdi", Phrasing, Global]("bdi", inline = true)
val Bdo = TagType["bdo", Phrasing, Global]("bdo", inline = true)
val Blockquote = TagType["blockquote", Flow, Global | "cite"]("blockquote")

val Body = TagType["body", Flow, Global | "onafterprint" | "onbeforeprint" | "onbeforeunload" |
                       "onhashchange" | "onlanguagechange" | "onmessage" | "onmessageerror" |
                       "onoffline" | "ononline" | "onpagehide" | "onpageshow" | "onpopstate" |
                       "onrejectionhandled" | "onstorage" | "onunhandledrejection" | "onunload"]
                  ("body")

val Br = TagType["br", Nothing, Global]("br", inline = true, unclosed = true)

val Button = TagType["button", Phrasing, Global | "disabled" | "form" | "formaction" |
                         "formenctype" | "formmethod" | "formnovalidate" | "formtarget" | "name" |
                         "htype" | "value"]
                    ("button", inline = true)

val Canvas = TransTagType["canvas", NonInteractive, Global | "width" | "height"]
                     ("canvas", inline = true) // complicated

val Caption = TagType["caption", Flow, Global]("caption") // no tables
val Cite = TagType["cite", Phrasing, Global]("cite", inline = true)
val Code = TagType["code", Phrasing, Global]("code", inline = true)
val Col = TagType["col", Nothing, Global | "span"]("col", inline = true)
val Colgroup = TagType["colgroup", "col" | "template", Global | "span"]("colgroup")
val Data = TagType["data", Phrasing, Global | "value"]("data", inline = true)

val Datalist = TagType["datalist", Phrasing | "option" | ScriptSupporting, Global]
                      ("datalist", inline = true)

val Dd = TagType["dd", Flow, Global]("dd")
val Del = TransTagType["del", Label, Global | "cite" | "datetime"]("del", inline = true)
val Details = TagType["details", "summary" | Flow, Global | "open"]("details")
val Dfn = TagType["dfn", Phrasing, Global]("dfn", inline = true)
val Dialog = TagType["dialog", Flow, Global | "open"]("dialog")
val Div = TagType["div", Flow, Global]("div")
val Dl = TagType["dl", "dt" | "dl" | "div", Global]("dl")
val Dt = TagType["dt", Flow, Global]("dt") // further constraints
val Em = TagType["em", Phrasing, Global]("em", inline = true)

val Embed = TagType["embed", Nothing, Global | "src" | "htype" | "width" | "height"]
                   ("embed", inline = true)

val Fieldset = TagType["fieldset", "legend" | Flow, Global | "disabled" | "form" | "name"]
                      ("fieldset")

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
val HMap = TransTagType["map", Phrasing | Flow | Palpable, Global | "name"]("map", inline = true)
val Head = TagType["head", Metadata, Global]("head")
val Header = TagType["header", Flow, Global]("header")
val Hgroup = TagType["hgroup", "h1" | "h2" | "h3" | "h4" | "h5" | "h6", Global]("hgroup")
val Hr = TagType["hr", Nothing, Global]("hr", inline = true, unclosed = true)

val I = TagType["i", Phrasing, Global]("i", inline = true)

val Iframe = TagType["iframe", Nothing, Global | "src" | "srcdoc" | "name" | "sandbox" | "allow" |
                         "allowfullscreen" | "width" | "height" | "referrerpolicy" | "loading"]
                    ("iframe", unclosed = false, inline = true)

val Img = TagType["img", Nothing, Global | "alt" | "src" | "srcset" | "sizes" | "crossorigin" |
                      "usemap" | "ismap" | "width" | "height" | "referrerpolicy" | "decoding" |
                      "loading"]
                 ("img", inline = true, unclosed = true)

val Input = TagType["input", Nothing, Global | "accept" | "alt" | "autocomplete" | "checked" |
                        "dirname" | "disabled" | "form" | "formaction" | "formenctype" |
                        "formmethod" | "formnovalidate" | "formtarget" | "height" | "list" | "max" |
                        "maxlength" | "min" | "minlength" | "multiple" | "name" | "pattern" |
                        "placeholder" | "readonly" | "required" | "size" | "src" | "step" |
                        "htype" | "value" | "width"]
                   ("input", inline = true, unclosed = true)

val Ins = TransTagType["ins", Label, Global | "cite" | "datetime"]("ins", inline = true)
val Kbd = TagType["kbd", Phrasing, Global]("kbd", inline = true)
val Label = TagType["label", Phrasing, Global | "hfor"]("label", inline = true)
val Legend = TagType["legend", Phrasing | Heading, Global]("legend")
val Li = TagType["li", Flow, Global | "value"]("li")

val Link = TagType["link", Nothing, Global | "href" | "crossorigin" | "rel" | "media" |
                   "integrity" | "hreflang" | "htype" | "referrerpolicy" | "sizes" | "imagesrcset" |
                   "imagesizes" | "as" | "color" | "disabled"]
                  ("link", inline = true, unclosed = true)

val Main = TagType["main", Flow, Global]("main")
val Mark = TagType["mark", Phrasing, Global]("mark", inline = true)
val Menu = TagType["menu", Flow, Global]("menu")

val Meta = TagType["meta", Nothing, Global | "name" | "httpEquiv" | "content" | "charset"]
              ("meta", inline = true, unclosed = true)

val Meter = TagType["meter", Phrasing, Global | "value" | "min" | "max" | "low" | "high" |
                        "optimum"]
                   ("meter", inline = true)

val Nav = TagType["nav", Flow, Global]("nav")

val Noscript = TagType["noscript", Label, Global]("noscript", inline = true)

val HObject = TagType["object", Label, Global | "data" | "htype" | "name" | "form" | "width" |
                          "height"]
                     ("object", inline = true)

val Ol = TagType["ol", "li" | ScriptSupporting, Global | "reversed" | "start" | "htype"]("ol")

val Optgroup = TagType["optgroup", "option" | ScriptSupporting, Global | "disabled" | "label"]
                      ("optgroup")

val Option = TagType["option", Nothing, Global | "disabled" | "label" | "selected" | "value"]
                    ("option", unclosed = true)

val Output = TagType["output", Phrasing, Global | "hfor" | "form" | "name"]("output", inline = true)
val P = TagType["p", Phrasing, Global]("p")
val Param = TagType["param", Nothing, Global | "name" | "value"]("param", unclosed = true)

val Picture = TagType["picture", "source" | "img" | ScriptSupporting, Global]
                     ("picture", inline = true)

val Pre = TagType["pre", Phrasing, Global]("pre", verbatim = true, inline = true)
val Progress = TagType["progress", Phrasing, Global | "value" | "max"]("progress", inline = true)
val Q = TagType["q", Phrasing, Global | "cite"]("q", inline = true)
val Rb = TagType["rb", Phrasing, Global]("rb")
val Rp = TagType["rp", Nothing, Global]("rp")
val Rt = TagType["rt", Phrasing, Global]("rt")
val Ruby = TagType["ruby", Phrasing | "rp" | "rt", Global]("ruby", inline = true)
val S = TagType["s", Phrasing, Global]("s", inline = true)
val Samp = TagType["samp", Phrasing, Global]("samp", inline = true)

val Script = TagType["script", Nothing, Global | "src" | "htype" | "nomodule" | "async" | "defer" |
                         "crossorigin" | "integrity" | "referrerpolicy"]
                    ("script", inline = true, verbatim = true)

val Section = TagType["section", Flow, Global]("section")

val Select = TagType["select", "option" | "optgroup" | ScriptSupporting, Global | "autocomplete" |
                             "disabled" | "form" | "multiple" | "name" | "required" | "size"]
                        ("select", inline = true)

val Slot = TransTagType["slot", Label, Global | "name"]("slot", inline = true)
val Small = TagType["small", Phrasing, Global]("small", inline = true)

val Source = TagType["source", Nothing, Global | "htype" | "src" | "srcset" | "sizes" | "media" |
                         "width" | "height"]
                    ("source")

val Span = TagType["span", Phrasing, Global]("span", inline = true)
val Strong = TagType["strong", Phrasing, Global | "media"]("strong", inline = true)
val Style = TagType["style", Nothing, Global]("style")
val Sub = TagType["sub", Phrasing, Global]("sub", inline = true)
val Summary = TagType["summary", Phrasing | Heading, Global]("summary")
val Sup = TagType["sup", Phrasing, Global]("sup", inline = true)

val Table = TagType["table", "caption" | "colgroup" | "thead" | "tbody" | "tr" | "tfoot" |
                        ScriptSupporting, Global]
                   ("table")

val Tbody = TagType["tbody", "tr" | ScriptSupporting, Global]("tbody")
val Td = TagType["td", Flow, Global | "colspan" | "rowspan" | "headers"]("td")
val Template = TagType["template", Nothing, Global]("template", unclosed = false, inline = true)

val Textarea = TagType["textarea", Nothing, Global | "autocomplete" | "cols" | "dirname" |
                           "disabled" | "form" | "maxlength" | "minlength" | "name" | "readonly" |
                           "placeholder" | "required" | "rows" | "wrap"]
                      ("textarea", inline = true, verbatim = true)

val Tfoot = TagType["tfoot", "tr" | ScriptSupporting, Global]("tfoot")
val Th = TagType["th", Flow, Global | "colspan" | "rowspan" | "headers" | "scope" | "abbr"]("th")
val Thead = TagType["thead", "tr" | ScriptSupporting, Global]("thead")
val Time = TagType["time", Phrasing, Global | "datetime"]("time", inline = true)
val Title = TagType["title", Nothing, Global]("title")
val Tr = TagType["tr", "td" | "th" | ScriptSupporting, Global]("tr")

val Track = TagType["track", Nothing, Global | "kind" | "src" | "srclang" | "label" | "default"]
                   ("track")

val U = TagType["u", Phrasing, Global]("u", inline = true)
val Ul = TagType["ul", "li" | ScriptSupporting, Global]("ul")
val Var = TagType["var", Nothing, Global]("var", inline = true)

val Video = TagType["video", Label, Global | "src" | "crossorigin" | "poster" | "preload" | "loop" |
                        "autoplay" | "playsinline" | "muted" | "controls" | "width" | "height"]
                   ("video", inline = true)

val Wbr = TagType["wbr", Nothing, Global]("wbr", inline = true)

trait ToHtml[-T, +R <: Label]:
  def convert(value: T): Seq[Html[R]]
