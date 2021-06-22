package honeycomb

import language.dynamics

val A = TransTag["a", NonInteractive, Global | "href" | "target" | "download" | "ping" | "rel" |
    "hreflang" | "htype" | "referrerpolicy"]("a", inline = true)

val Abbr = Tag["abbr", Phrasing, Global]("abbr", inline = true)
val Address = Tag["address", Flow | Palpable, Global]("address")

val Area = Tag["area", Nothing, Global | "alt" | "coords" | "shape" | "href" | "target" | "download" | "ping" |
    "rel" | "referrerpolicy"]("area", inline = true)

val Article = Tag["article", Flow, Global]("article") // further constraints on descendants
val Aside = Tag["aside", Flow, Global]("aside")

// val Audio = IncludeTag["audio", Label, "track", Global | "src" | "crossorigin" | "preload" | "autoplay" |
//     "loop" | "muted" | "controls"]("audio", inline = true) // complicated!

val B = Tag["b", Phrasing, Global]("b", inline = true)
val Base = Tag["base", Nothing, Global | "href" | "target"]("base", inline = true)
val Bdi = Tag["bdi", Phrasing, Global]("bdi", inline = true)
val Bdo = Tag["bdo", Phrasing, Global]("bdo", inline = true)
val Blockquote = Tag["blockquote", Flow, Global | "cite"]("blockquote")

val Body = Tag["body", Flow, Global | "onafterprint" | "onbeforeprint" | "onbeforeunload" | "onhashchange" |
    "onlanguagechange" | "onmessage" | "onmessageerror" | "onoffline" | "ononline" | "onpagehide" |
    "onpageshow" | "onpopstate" | "onrejectionhandled" | "onstorage" | "onunhandledrejection" |
    "onunload"]("body")

val Br = Tag["br", Nothing, Global]("br", inline = true, unclosed = true)

val Button = Tag["button", Phrasing, Global | "disabled" | "form" | "formaction" | "formenctype" | "formmethod" |
    "formnovalidate" | "formtarget" | "name" | "htype" | "value"]("button", inline = true)

val Canvas = TransTag["canvas", NonInteractive, Global | "width" | "height"]("canvas", inline = true) // complicated
val Caption = Tag["caption", Flow, Global]("caption") // no tables
val Cite = Tag["cite", Phrasing, Global]("cite", inline = true)
val Code = Tag["code", Phrasing, Global]("code", inline = true)
val Col = Tag["col", Nothing, Global | "span"]("col", inline = true)
val Colgroup = Tag["colgroup", "col" | "template", Global | "span"]("colgroup")
val Data = Tag["data", Phrasing, Global | "value"]("data", inline = true)
val Datalist = Tag["datalist", Phrasing | "option" | ScriptSupporting, Global]("datalist", inline = true)
val Dd = Tag["dd", Flow, Global]("dd")
val Del = TransTag["del", Label, Global | "cite" | "datetime"]("del", inline = true)
val Details = Tag["details", "summary" | Flow, Global | "open"]("details")
val Dfn = Tag["dfn", Phrasing, Global]("dfn", inline = true)
val Dialog = Tag["dialog", Flow, Global | "open"]("dialog")
val Div = Tag["div", Flow, Global]("div")
val Dl = Tag["dl", "dt" | "dl" | "div", Global]("dl")
val Dt = Tag["dt", Flow, Global]("dt") // further constraints
val Em = Tag["em", Phrasing, Global]("em", inline = true)
val Embed = Tag["embed", Nothing, Global | "src" | "htype" | "width" | "height"]("embed", inline = true)
val Fieldset = Tag["fieldset", "legend" | Flow, Global | "disabled" | "form" | "name"]("fieldset")
val Figcaption = Tag["figcaption", Flow, Global]("figcaption")
val Figure = Tag["figure", "figcaption" | Flow, Global]("figure") // first or last element may be figcaption, but not both
val Footer = Tag["footer", Flow, Global]("footer")

val Form = Tag["form", Flow, Global | "acceptCharset" | "action" | "autocomplete" | "enctype" | "method" |
    "name" | "novalidate" | "target" | "rel"]("form")

val H1 = Tag["h1", Phrasing, Global]("h1")
val H2 = Tag["h2", Phrasing, Global]("h2")
val H3 = Tag["h3", Phrasing, Global]("h3")
val H4 = Tag["h4", Phrasing, Global]("h4")
val H5 = Tag["h5", Phrasing, Global]("h5")
val H6 = Tag["h6", Phrasing, Global]("h6")
val HMap = TransTag["map", Phrasing | Flow | Palpable, Global | "name"]("map", inline = true)
val Head = Tag["head", Metadata, Global]("head")
val Header = Tag["header", Flow, Global]("header")
val Hgroup = Tag["hgroup", "h1" | "h2" | "h3" | "h4" | "h5" | "h6", Global]("hgroup")
val Hr = Tag["hr", Nothing, Global]("hr", inline = true, unclosed = true)

trait ToHtml[-T, +R <: Label]:
  def convert(value: T): Seq[Content[R]]

val I = Tag["i", Phrasing, Global]("i", inline = true)

val Iframe = Tag["iframe", Nothing, Global | "src" | "srcdoc" | "name" | "sandbox" | "allow" |
    "allowfullscreen" | "width" | "height" | "referrerpolicy" | "loading"]("iframe", unclosed = false,
    inline = true)

val Img = Tag["img", Nothing, Global | "alt" | "src" | "srcset" | "sizes" | "crossorigin" | "usemap" |
    "ismap" | "width" | "height" | "referrerpolicy" | "decoding" | "loading"]("img", inline = true,
    unclosed = true)

val Input = Tag["input", Nothing, Global | "accept" | "alt" | "autocomplete" | "checked" | "dirname" |
    "disabled" | "form" | "formaction" | "formenctype" | "formmethod" | "formnovalidate" | "formtarget" |
    "height" | "list" | "max" | "maxlength" | "min" | "minlength" | "multiple" | "name" | "pattern" |
    "placeholder" | "readonly" | "required" | "size" | "src" | "step" | "htype" | "value" | "width"]
    ("input", inline = true, unclosed = true)

val Ins = TransTag["ins", Label, Global | "cite" | "datetime"]("ins", inline = true)
val Kbd = Tag["kbd", Phrasing, Global]("kbd", inline = true)
val Label = Tag["label", Phrasing, Global | "hfor"]("label", inline = true)
val Legend = Tag["legend", Phrasing | Heading, Global]("legend")
val Li = Tag["li", Flow, Global | "value"]("li")

val Link = Tag["link", Nothing, Global | "href" | "crossorigin" | "rel" | "media" | "integrity" | "hreflang" |
    "htype" | "referrerpolicy" | "sizes" | "imagesrcset" | "imagesizes" | "as" | "color" | "disabled"]
    ("link", inline = true, unclosed = true)

val Main = Tag["main", Flow, Global]("main")
val Mark = Tag["mark", Phrasing, Global]("mark", inline = true)
val Menu = Tag["menu", Flow, Global]("menu")

val Meta = Tag["meta", Nothing, Global | "name" | "httpEquiv" | "content" | "charset"]("meta",
    inline = true, unclosed = true)

val Meter = Tag["meter", Phrasing, Global | "value" | "min" | "max" | "low" | "high" | "optimum"]("meter",
    inline = true)

val Nav = Tag["nav", Flow, Global]("nav")
// val Noscript = IncludeTag["noscript", Label, "link" | "style" | "meta", Global]("noscript", inline = true)

// val Object = IncludeTag["object", Label, "param", Global | "data" | "htype" | "name" | "form" |
//     "width" | "height"]("object", inline = true)

val Ol = Tag["ol", "li" | ScriptSupporting, Global | "reversed" | "start" | "htype"]("ol")
val Optgroup = Tag["optgroup", "option" | ScriptSupporting, Global | "disabled" | "label"]("optgroup")

val Option = Tag["option", Nothing, Global | "disabled" | "label" | "selected" | "value"]("option", unclosed = true)

val Output = Tag["output", Phrasing, Global | "hfor" | "form" | "name"]("output", inline = true)
val P = Tag["p", Phrasing, Global]("p")
val Param = Tag["param", Nothing, Global | "name" | "value"]("param", unclosed = true)
val Picture = Tag["picture", "source" | "img" | ScriptSupporting, Global]("picture", inline = true)
val Pre = Tag["pre", Phrasing, Global]("pre", verbatim = true, inline = true)
val Progress = Tag["progress", Phrasing, Global | "value" | "max"]("progress", inline = true)
val Q = Tag["q", Phrasing, Global | "cite"]("q", inline = true)
val Rb = Tag["rb", Phrasing, Global]("rb")
val Rp = Tag["rp", Nothing, Global]("rp")
val Rt = Tag["rt", Phrasing, Global]("rt")
val Ruby = Tag["ruby", Phrasing | "rp" | "rt", Global]("ruby", inline = true)
val S = Tag["s", Phrasing, Global]("s", inline = true)
val Samp = Tag["samp", Phrasing, Global]("samp", inline = true)

val Script = Tag["script", Nothing, Global | "src" | "htype" | "nomodule" | "async" | "defer" | "crossorigin" |
    "integrity" | "referrerpolicy"]("script", inline = true)

val Section = Tag["section", Flow, Global]("section")

val Select = Tag["select", "option" | "optgroup" | ScriptSupporting, Global | "autocomplete" | "disabled" |
    "form" | "multiple" | "name" | "required" | "size"]("select", inline = true)

val Slot = TransTag["slot", Label, Global | "name"]("slot", inline = true)
val Small = Tag["small", Phrasing, Global]("small", inline = true)

val Source = Tag["source", Nothing, Global | "htype" | "src" | "srcset" | "sizes" | "media" | "width" |
    "height"]("source")

val Span = Tag["span", Phrasing, Global]("span", inline = true)
val Strong = Tag["strong", Phrasing, Global | "media"]("strong", inline = true)
val Style = Tag["style", Nothing, Global]("style")
val Sub = Tag["sub", Phrasing, Global]("sub", inline = true)
val Summary = Tag["summary", Phrasing | Heading, Global]("summary")
val Sup = Tag["sup", Phrasing, Global]("sup", inline = true)
val Table = Tag["table", "caption" | "colgroup" | "thead" | "tbody" | "tr" | "tfoot" | ScriptSupporting, Global]("table")
val Tbody = Tag["tbody", "tr" | ScriptSupporting, Global]("tbody")
val Td = Tag["td", Flow, Global | "colspan" | "rowspan" | "headers"]("td")
val Template = Tag["template", Nothing, Global]("template", unclosed = false, inline = true)

val Textarea = Tag["textarea", Nothing, Global | "autocomplete" | "cols" | "dirname" | "disabled" | "form" |
    "maxlength" | "minlength" | "name" | "placeholder" | "readonly" | "required" | "rows" | "wrap"]("textarea",
    inline = true, verbatim = true)

val Tfoot = Tag["tfoot", "tr" | ScriptSupporting, Global]("tfoot")
val Th = Tag["th", Flow, Global | "colspan" | "rowspan" | "headers" | "scope" | "abbr"]("th")
val Thead = Tag["thead", "tr" | ScriptSupporting, Global]("thead")
val Time = Tag["time", Phrasing, Global | "datetime"]("time", inline = true)
val Title = Tag["title", Nothing, Global]("title")
val Tr = Tag["tr", "td" | "th" | ScriptSupporting, Global]("tr")
val Track = Tag["track", Nothing, Global | "kind" | "src" | "srclang" | "label" | "default"]("track")
val U = Tag["u", Phrasing, Global]("u", inline = true)
val Ul = Tag["ul", "li" | ScriptSupporting, Global]("ul")
val Var = Tag["var", Nothing, Global]("var", inline = true)

// val Video = IncludeTag["video", Label, "track" | "source", Global | "src" | "crossorigin" | "poster" |
//     "preload" | "autoplay" | "playsinline" | "loop" | "muted" | "controls" | "width" | "height"]
//     ("video", inline = true) // complicated!

val Wbr = Tag["wbr", Nothing, Global]("wbr", inline = true)
