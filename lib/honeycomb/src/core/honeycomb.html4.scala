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
┃    Soundness, version 0.37.0.                                                                    ┃
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

import language.dynamics

object html4:
  type EventHandlers =
    "onclick" | "ondblclick" | "onmousedown" | "onmouseup" | "onmouseover" | "onmousemove"
    | "onmouseout" | "onkeypress" | "onkeydown" | "onkeyup" | "onfocus" | "onblur" | "onchange"
    | "onselect" | "onsubmit" | "onreset" | "onload" | "onunload" | "onabort" | "onerror"
    | "onresize" | "onscroll"

  type Global = "id" | "class" | "title" | "style" | "lang" | "dir" | EventHandlers

  type Phrasing =
    "a" | "abbr" | "acronym" | "applet" | "b" | "basefont" | "bdo" | "big" | "br" | "button"
    | "cite" | "code" | "del" | "dfn" | "em" | "font" | "i" | "iframe" | "img" | "input" | "ins"
    | "kbd" | "label" | "map" | "object" | "q" | "s" | "samp" | "script" | "select" | "small"
    | "span" | "strike" | "strong" | "sub" | "sup" | "textarea" | "tt" | "u" | "var"

  type Flow =
    Phrasing | "address" | "blockquote" | "center" | "dd" | "dir" | "div" | "dl" | "dt" | "fieldset"
    | "form" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "hr" | "isindex" | "li" | "menu"
    | "noframes" | "noscript" | "ol" | "p" | "pre" | "table" | "ul"

  val Head = Tag
              ["head", "title" | "base" | "meta" | "script" | "style" | "link", Global | "profile"]
              ("head")
  val Title = Tag["title", Nothing, Global]("title")
  val Base = Tag["base", Nothing, Global | "href" | "target"]("base")

  val Link = Tag
              ["link",
               Nothing,
               Global | "href" | "rel" | "rev" | "type" | "media" | "hreflang" | "charset"
               | "target"]
              ("link")

  val Meta = Tag["meta", Nothing, Global | "http-equiv" | "name" | "content" | "scheme"]("meta")
  val Style = Tag["style", Nothing, Global | "type" | "media" | "title"]("style")

  val Script = Tag["script", Nothing, Global | "src" | "type" | "charset" | "language" | "defer"]
                ("script")

  val Noscript = Tag["noscript", Flow, Global]("noscript")

  val Body = Tag
              ["body",
               Flow,
               Global | "background" | "bgcolor" | "text" | "link" | "vlink" | "alink" | "onload"
               | "onunload"]
              ("body")

  val Div = Tag["div", Flow, Global | "align"]("div")
  val Center = Tag["center", Flow, Global]("center")
  val P = Tag["p", Phrasing, Global | "align"]("p")
  val Address = Tag["address", Flow, Global]("address")
  val Blockquote = Tag["blockquote", Flow, Global | "cite"]("blockquote")
  val Pre = Tag["pre", Phrasing, Global | "width"]("pre")
  val Hr = Tag["hr", Nothing, Global | "align" | "noshade" | "size" | "width"]("hr")
  val Br = Tag["br", Nothing, Global | "clear"]("br")
  val Ins = Tag["ins", Flow, Global | "cite" | "datetime"]("ins")
  val Del = Tag["del", Flow, Global | "cite" | "datetime"]("del")

  val H1 = Tag["h1", Phrasing, Global | "align"]("h1")
  val H2 = Tag["h2", Phrasing, Global | "align"]("h2")
  val H3 = Tag["h3", Phrasing, Global | "align"]("h3")
  val H4 = Tag["h4", Phrasing, Global | "align"]("h4")
  val H5 = Tag["h5", Phrasing, Global | "align"]("h5")
  val H6 = Tag["h6", Phrasing, Global | "align"]("h6")

  val Ul = Tag["ul", Flow, Global | "type" | "compact"]("ul")
  val Ol = Tag["ol", Flow, Global | "type" | "start" | "compact"]("ol")
  val Li = Tag["li", Flow, Global | "type" | "value"]("li")
  val Dl = Tag["dl", Flow, Global | "compact"]("dl")
  val Dt = Tag["dt", Phrasing, Global]("dt")
  val Dd = Tag["dd", Flow, Global]("dd")
  val Dir = Tag["dir", Flow, Global | "compact"]("dir")
  val Menu = Tag["menu", Flow, Global | "compact"]("menu")

  val A = Tag
           ["a",
            Phrasing,
            Global | "href" | "target" | "name" | "rel" | "rev" | "shape" | "coords" | "hreflang"
            | "type" | "charset" | "tabindex" | "accesskey"]
           ("a")

  val Abbr = Tag["abbr", Phrasing, Global]("abbr")
  val Acronym = Tag["acronym", Phrasing, Global]("acronym")
  val Bdo = Tag["bdo", Phrasing, Global | "dir"]("bdo")
  val Span = Tag["span", Phrasing, Global]("span")
  val Big = Tag["big", Phrasing, Global]("big")
  val Small = Tag["small", Phrasing, Global]("small")
  val B = Tag["b", Phrasing, Global]("b")
  val I = Tag["i", Phrasing, Global]("i")
  val U = Tag["u", Phrasing, Global]("u")
  val S = Tag["s", Phrasing, Global]("s")
  val Strike = Tag["strike", Phrasing, Global]("strike")
  val Em = Tag["em", Phrasing, Global]("em")
  val Strong = Tag["strong", Phrasing, Global]("strong")
  val Dfn = Tag["dfn", Phrasing, Global]("dfn")
  val Code = Tag["code", Phrasing, Global]("code")
  val Samp = Tag["samp", Phrasing, Global]("samp")
  val Kbd = Tag["kbd", Phrasing, Global]("kbd")
  val VarT = Tag["var", Phrasing, Global]("var")
  val Cite = Tag["cite", Phrasing, Global]("cite")
  val Sub = Tag["sub", Phrasing, Global]("sub")
  val Sup = Tag["sup", Phrasing, Global]("sup")
  val Q = Tag["q", Phrasing, Global | "cite"]("q")
  val Font = Tag["font", Phrasing, Global | "size" | "color" | "face"]("font")
  val Basefont = Tag["basefont", Nothing, Global | "size" | "color" | "face"]("basefont")

  val Img = Tag
             ["img",
              Nothing,
              Global | "src" | "alt" | "longdesc" | "height" | "width" | "usemap"
              | "ismap" | "border" | "align" | "hspace" | "vspace" | "name"]
             ("img")

  val Map = Tag["map", Flow, Global | "name"]("map")
  val Area = Tag
              ["area",
               Nothing,
               Global | "shape" | "coords" | "href" | "nohref" | "alt" | "target" | "tabindex"
               | "accesskey"]("area")

  val Object = Tag
                ["object",
                 Flow,
                 Global | "data" | "type" | "classid" | "codebase" | "codetype" | "archive"
                 | "standby" | "height" | "width" | "usemap" | "name" | "tabindex"]
                ("object")

  val Param = Tag["param", Nothing, Global | "name" | "value" | "valuetype" | "type"]("param")

  val Applet = Tag
                ["applet",
                 Phrasing,
                 Global | "code" | "codebase" | "archive" | "object" | "alt" | "name" | "width"
                 | "height" | "align" | "hspace" | "vspace"]
                ("applet")

  val Table = Tag
               ["table",
                "caption" | "col" | "colgroup" | "thead" | "tfoot" | "tbody" | "tr",
                Global | "summary" | "width" | "border" | "frame" | "rules" | "cellspacing"
                | "cellpadding" | "align" | "bgcolor"]
               ("table")

  val Caption = Tag["caption", Flow, Global | "align"]("caption")

  val Colgroup = Tag
                  ["colgroup",
                   Nothing,
                   Global | "span" | "width" | "align" | "char" | "charoff" | "valign"]
                  ("colgroup")

  val Col = Tag["col", Nothing, Global | "span" | "width" | "align" | "char" | "charoff" | "valign"]
             ("col")

  val Thead = Tag["thead", Nothing, Global | "align" | "char" | "charoff" | "valign"]("thead")
  val Tbody = Tag["tbody", Nothing, Global | "align" | "char" | "charoff" | "valign"]("tbody")
  val Tfoot = Tag["tfoot", Nothing, Global | "align" | "char" | "charoff" | "valign"]("tfoot")

  val Tr = Tag["tr", "td" | "th", Global | "align" | "bgcolor" | "char" | "charoff" | "valign"]
            ("tr")

  val Th = Tag
            ["th",
             Flow,
             Global | "abbr" | "axis" | "headers" | "scope" | "rowspan" | "colspan" | "nowrap"
             | "bgcolor" | "width" | "height" | "align" | "char" | "charoff" | "valign"]
            ("th")

  val Td = Tag
            ["td",
             Flow,
             Global | "abbr" | "axis" | "headers" | "scope" | "rowspan" | "colspan" | "nowrap"
             | "bgcolor" | "width" | "height" | "align" | "char" | "charoff" | "valign"]
            ("td")

  val Form = Tag
              ["form",
               Flow,
               Global | "action" | "method" | "enctype" | "accept" | "accept-charset" | "name"
               | "target" | "onsubmit" | "onreset"]
              ("form")

  val Fieldset = Tag["fieldset", Flow, Global]("fieldset")
  val Legend = Tag["legend", Phrasing, Global | "accesskey" | "align"]("legend")
  val Label = Tag["label", Phrasing, Global | "for" | "accesskey" | "onblur" | "onfocus"]("label")

  val Input = Tag
               ["input",
                Nothing,
                Global | "type" | "name" | "value" | "checked" | "disabled" | "readonly" | "size"
                | "maxlength" | "src" | "alt" | "usemap" | "ismap" | "tabindex" | "accesskey"
                | "accept" | "align" | "onselect" | "onchange"]
               ("input")

  val Select = Tag
                ["select",
                 Nothing,
                 Global | "name" | "size" | "multiple" | "disabled" | "tabindex" | "accesskey"
                 | "onchange"]
                ("select")

  val Optgroup = Tag["optgroup", Nothing, Global | "disabled" | "label"]("optgroup")

  val Option = Tag["option", Nothing, Global | "selected" | "disabled" | "label" | "value"]
                ("option")

  val Textarea = Tag
                  ["textarea",
                   Nothing,
                   Global | "name" | "rows" | "cols" | "disabled" | "readonly" | "tabindex"
                   | "accesskey" | "onselect" | "onchange"]
                  ("textarea")

  val Button = Tag
                ["button",
                 Phrasing,
                 Global | "name" | "value" | "type" | "disabled" | "tabindex" | "accesskey"]
                ("button")

  val Isindex = Tag["isindex", Nothing, Global | "prompt"]("isindex")

  val Frameset = Tag["frameset", Nothing, Global | "rows" | "cols" | "onload" | "onunload"]
                  ("frameset")

  val Frame = Tag
               ["frame",
                Nothing,
                Global | "src" | "name" | "longdesc" | "frameborder" | "marginwidth"
                | "marginheight" | "noresize" | "scrolling" | "title"]
               ("frame")

  val Iframe = Tag
                ["iframe",
                 Nothing,
                 Global | "src" | "name" | "longdesc" | "frameborder" | "marginwidth"
                 | "marginheight" | "scrolling" | "title" | "height" | "width"]
                ("iframe")

  val Noframes = Tag["noframes", Flow, Global]("noframes")
