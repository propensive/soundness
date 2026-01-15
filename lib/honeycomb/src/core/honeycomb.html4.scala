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
┃    Soundness, version 0.51.0.                                                                    ┃
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

// object html4:
//   type EventHandlers =
//     "onclick" | "ondblclick" | "onmousedown" | "onmouseup" | "onmouseover" | "onmousemove"
//     | "onmouseout" | "onkeypress" | "onkeydown" | "onkeyup" | "onfocus" | "onblur" | "onchange"
//     | "onselect" | "onsubmit" | "onreset" | "onload" | "onunload" | "onabort" | "onerror"
//     | "onresize" | "onscroll"

//   type Global = "id" | "class" | "title" | "style" | "lang" | "dir" | EventHandlers

//   type Phrasing =
//     "a" | "abbr" | "acronym" | "applet" | "b" | "basefont" | "bdo" | "big" | "br" | "button"
//     | "cite" | "code" | "del" | "dfn" | "em" | "font" | "i" | "iframe" | "img" | "input" | "ins"
//     | "kbd" | "label" | "map" | "object" | "q" | "s" | "samp" | "script" | "select" | "small"
//     | "span" | "strike" | "strong" | "sub" | "sup" | "textarea" | "tt" | "u" | "var"

//   type Flow =
//     Phrasing | "address" | "blockquote" | "center" | "dd" | "dir" | "div" | "dl" | "dt" | "fieldset"
//     | "form" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "hr" | "isindex" | "li" | "menu"
//     | "noframes" | "noscript" | "ol" | "p" | "pre" | "table" | "ul"

//   val Head = OldTag
//               ["head", "title" | "base" | "meta" | "script" | "style" | "link", Global | "profile"]
//               ("head")
//   val Title = OldTag["title", Nothing, Global]("title")
//   val Base = OldTag["base", Nothing, Global | "href" | "target"]("base")

//   val Link = OldTag
//               ["link",
//                Nothing,
//                Global | "href" | "rel" | "rev" | "type" | "media" | "hreflang" | "charset"
//                | "target"]
//               ("link")

//   val Meta = OldTag["meta", Nothing, Global | "http-equiv" | "name" | "content" | "scheme"]("meta")
//   val Style = OldTag["style", Nothing, Global | "type" | "media" | "title"]("style")

//   val Script = OldTag["script", Nothing, Global | "src" | "type" | "charset" | "language" | "defer"]
//                 ("script")

//   val Noscript = OldTag["noscript", Flow, Global]("noscript")

//   val Body = OldTag
//               ["body",
//                Flow,
//                Global | "background" | "bgcolor" | "text" | "link" | "vlink" | "alink" | "onload"
//                | "onunload"]
//               ("body")

//   val Div = OldTag["div", Flow, Global | "align"]("div")
//   val Center = OldTag["center", Flow, Global]("center")
//   val P = OldTag["p", Phrasing, Global | "align"]("p")
//   val Address = OldTag["address", Flow, Global]("address")
//   val Blockquote = OldTag["blockquote", Flow, Global | "cite"]("blockquote")
//   val Pre = OldTag["pre", Phrasing, Global | "width"]("pre")
//   val Hr = OldTag["hr", Nothing, Global | "align" | "noshade" | "size" | "width"]("hr")
//   val Br = OldTag["br", Nothing, Global | "clear"]("br")
//   val Ins = OldTag["ins", Flow, Global | "cite" | "datetime"]("ins")
//   val Del = OldTag["del", Flow, Global | "cite" | "datetime"]("del")

//   val H1 = OldTag["h1", Phrasing, Global | "align"]("h1")
//   val H2 = OldTag["h2", Phrasing, Global | "align"]("h2")
//   val H3 = OldTag["h3", Phrasing, Global | "align"]("h3")
//   val H4 = OldTag["h4", Phrasing, Global | "align"]("h4")
//   val H5 = OldTag["h5", Phrasing, Global | "align"]("h5")
//   val H6 = OldTag["h6", Phrasing, Global | "align"]("h6")

//   val Ul = OldTag["ul", Flow, Global | "type" | "compact"]("ul")
//   val Ol = OldTag["ol", Flow, Global | "type" | "start" | "compact"]("ol")
//   val Li = OldTag["li", Flow, Global | "type" | "value"]("li")
//   val Dl = OldTag["dl", Flow, Global | "compact"]("dl")
//   val Dt = OldTag["dt", Phrasing, Global]("dt")
//   val Dd = OldTag["dd", Flow, Global]("dd")
//   val Dir = OldTag["dir", Flow, Global | "compact"]("dir")
//   val Menu = OldTag["menu", Flow, Global | "compact"]("menu")

//   val A = OldTag
//            ["a",
//             Phrasing,
//             Global | "href" | "target" | "name" | "rel" | "rev" | "shape" | "coords" | "hreflang"
//             | "type" | "charset" | "tabindex" | "accesskey"]
//            ("a")

//   val Abbr = OldTag["abbr", Phrasing, Global]("abbr")
//   val Acronym = OldTag["acronym", Phrasing, Global]("acronym")
//   val Bdo = OldTag["bdo", Phrasing, Global | "dir"]("bdo")
//   val Span = OldTag["span", Phrasing, Global]("span")
//   val Big = OldTag["big", Phrasing, Global]("big")
//   val Small = OldTag["small", Phrasing, Global]("small")
//   val B = OldTag["b", Phrasing, Global]("b")
//   val I = OldTag["i", Phrasing, Global]("i")
//   val U = OldTag["u", Phrasing, Global]("u")
//   val S = OldTag["s", Phrasing, Global]("s")
//   val Strike = OldTag["strike", Phrasing, Global]("strike")
//   val Em = OldTag["em", Phrasing, Global]("em")
//   val Strong = OldTag["strong", Phrasing, Global]("strong")
//   val Dfn = OldTag["dfn", Phrasing, Global]("dfn")
//   val Code = OldTag["code", Phrasing, Global]("code")
//   val Samp = OldTag["samp", Phrasing, Global]("samp")
//   val Kbd = OldTag["kbd", Phrasing, Global]("kbd")
//   val VarT = OldTag["var", Phrasing, Global]("var")
//   val Cite = OldTag["cite", Phrasing, Global]("cite")
//   val Sub = OldTag["sub", Phrasing, Global]("sub")
//   val Sup = OldTag["sup", Phrasing, Global]("sup")
//   val Q = OldTag["q", Phrasing, Global | "cite"]("q")
//   val Font = OldTag["font", Phrasing, Global | "size" | "color" | "face"]("font")
//   val Basefont = OldTag["basefont", Nothing, Global | "size" | "color" | "face"]("basefont")

//   val Img = OldTag
//              ["img",
//               Nothing,
//               Global | "src" | "alt" | "longdesc" | "height" | "width" | "usemap"
//               | "ismap" | "border" | "align" | "hspace" | "vspace" | "name"]
//              ("img")

//   val Map = OldTag["map", Flow, Global | "name"]("map")
//   val Area = OldTag
//               ["area",
//                Nothing,
//                Global | "shape" | "coords" | "href" | "nohref" | "alt" | "target" | "tabindex"
//                | "accesskey"]("area")

//   val Object = OldTag
//                 ["object",
//                  Flow,
//                  Global | "data" | "type" | "classid" | "codebase" | "codetype" | "archive"
//                  | "standby" | "height" | "width" | "usemap" | "name" | "tabindex"]
//                 ("object")

//   val Param = OldTag["param", Nothing, Global | "name" | "value" | "valuetype" | "type"]("param")

//   val Applet = OldTag
//                 ["applet",
//                  Phrasing,
//                  Global | "code" | "codebase" | "archive" | "object" | "alt" | "name" | "width"
//                  | "height" | "align" | "hspace" | "vspace"]
//                 ("applet")

//   val Table = OldTag
//                ["table",
//                 "caption" | "col" | "colgroup" | "thead" | "tfoot" | "tbody" | "tr",
//                 Global | "summary" | "width" | "border" | "frame" | "rules" | "cellspacing"
//                 | "cellpadding" | "align" | "bgcolor"]
//                ("table")

//   val Caption = OldTag["caption", Flow, Global | "align"]("caption")

//   val Colgroup = OldTag
//                   ["colgroup",
//                    Nothing,
//                    Global | "span" | "width" | "align" | "char" | "charoff" | "valign"]
//                   ("colgroup")

//   val Col = OldTag["col", Nothing, Global | "span" | "width" | "align" | "char" | "charoff" | "valign"]
//              ("col")

//   val Thead = OldTag["thead", Nothing, Global | "align" | "char" | "charoff" | "valign"]("thead")
//   val Tbody = OldTag["tbody", Nothing, Global | "align" | "char" | "charoff" | "valign"]("tbody")
//   val Tfoot = OldTag["tfoot", Nothing, Global | "align" | "char" | "charoff" | "valign"]("tfoot")

//   val Tr = OldTag["tr", "td" | "th", Global | "align" | "bgcolor" | "char" | "charoff" | "valign"]
//             ("tr")

//   val Th = OldTag
//             ["th",
//              Flow,
//              Global | "abbr" | "axis" | "headers" | "scope" | "rowspan" | "colspan" | "nowrap"
//              | "bgcolor" | "width" | "height" | "align" | "char" | "charoff" | "valign"]
//             ("th")

//   val Td = OldTag
//             ["td",
//              Flow,
//              Global | "abbr" | "axis" | "headers" | "scope" | "rowspan" | "colspan" | "nowrap"
//              | "bgcolor" | "width" | "height" | "align" | "char" | "charoff" | "valign"]
//             ("td")

//   val Form = OldTag
//               ["form",
//                Flow,
//                Global | "action" | "method" | "enctype" | "accept" | "accept-charset" | "name"
//                | "target" | "onsubmit" | "onreset"]
//               ("form")

//   val Fieldset = OldTag["fieldset", Flow, Global]("fieldset")
//   val Legend = OldTag["legend", Phrasing, Global | "accesskey" | "align"]("legend")
//   val Label = OldTag["label", Phrasing, Global | "for" | "accesskey" | "onblur" | "onfocus"]("label")

//   val Input = OldTag
//                ["input",
//                 Nothing,
//                 Global | "type" | "name" | "value" | "checked" | "disabled" | "readonly" | "size"
//                 | "maxlength" | "src" | "alt" | "usemap" | "ismap" | "tabindex" | "accesskey"
//                 | "accept" | "align" | "onselect" | "onchange"]
//                ("input")

//   val Select = OldTag
//                 ["select",
//                  Nothing,
//                  Global | "name" | "size" | "multiple" | "disabled" | "tabindex" | "accesskey"
//                  | "onchange"]
//                 ("select")

//   val Optgroup = OldTag["optgroup", Nothing, Global | "disabled" | "label"]("optgroup")

//   val Option = OldTag["option", Nothing, Global | "selected" | "disabled" | "label" | "value"]
//                 ("option")

//   val Textarea = OldTag
//                   ["textarea",
//                    Nothing,
//                    Global | "name" | "rows" | "cols" | "disabled" | "readonly" | "tabindex"
//                    | "accesskey" | "onselect" | "onchange"]
//                   ("textarea")

//   val Button = OldTag
//                 ["button",
//                  Phrasing,
//                  Global | "name" | "value" | "type" | "disabled" | "tabindex" | "accesskey"]
//                 ("button")

//   val Isindex = OldTag["isindex", Nothing, Global | "prompt"]("isindex")

//   val Frameset = OldTag["frameset", Nothing, Global | "rows" | "cols" | "onload" | "onunload"]
//                   ("frameset")

//   val Frame = OldTag
//                ["frame",
//                 Nothing,
//                 Global | "src" | "name" | "longdesc" | "frameborder" | "marginwidth"
//                 | "marginheight" | "noresize" | "scrolling" | "title"]
//                ("frame")

//   val Iframe = OldTag
//                 ["iframe",
//                  Nothing,
//                  Global | "src" | "name" | "longdesc" | "frameborder" | "marginwidth"
//                  | "marginheight" | "scrolling" | "title" | "height" | "width"]
//                 ("iframe")

//   val Noframes = OldTag["noframes", Flow, Global]("noframes")
