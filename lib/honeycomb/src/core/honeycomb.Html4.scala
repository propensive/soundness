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
package honeycomb

import language.dynamics

import scala.collection.immutable as sci

import adversaria.*
import anticipation.*
import fulminate.*
import gossamer.*
import hellenism.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import rudiments.*
import turbulence.*
import typonym.*
import vacuous.*

import charDecoders.utf8
import classloaders.threadContext
import textSanitizers.skip

object Html4Transitional:
  // Reuse WHATWG attribute value type markers (Textual, Url, Color, Presence, etc.)
  // to share Attributive instances. HTML4-specific markers added below.
  type Color = Whatwg.Color
  type Coords = Whatwg.Coords
  type Css = Whatwg.Css
  type CssClassList = Whatwg.CssClassList
  type Datetime = Whatwg.Datetime
  type Dir = Whatwg.Dir
  type Enctype = Whatwg.Enctype
  type HttpEquiv = Whatwg.HttpEquiv
  type Id = Whatwg.Id
  type Integral = Whatwg.Integral
  type Language = Whatwg.Language
  type MediaQueryList = Whatwg.MediaQueryList
  type Method = Whatwg.Method
  type MimeList = Whatwg.MimeList
  type Mime = Whatwg.Mime
  type Name = Whatwg.Name
  type OlType = Whatwg.OlType
  type Presence = Whatwg.Presence
  type PositiveInt = Whatwg.PositiveInt
  type Regex = Whatwg.Regex
  type Shape = Whatwg.Shape
  type Target = Whatwg.Target
  type Textual = Whatwg.Textual
  type Tokens = Whatwg.Tokens
  type Url = Whatwg.Url

  // HTML4-specific attribute value markers — alias to Whatwg.Textual to inherit
  // Attributive instances for Text and String. Distinct names retained for clarity.
  type Align = Whatwg.Textual
  type Charset = Whatwg.Textual
  type Clear = Whatwg.Textual
  type Frame = Whatwg.Textual
  type Rules = Whatwg.Textual
  type Scope = Whatwg.Textual
  type Script = Whatwg.Textual
  type Size = Whatwg.Textual
  type Valign = Whatwg.Textual
  type Valuetype = Whatwg.Textual
  type Wrap = Whatwg.Textual

  type Attribute = honeycomb.Attribute in Html4Transitional

  def attribute[self <: Label: ValueOf, plane <: Label: Reifiable to List[String], topic]()
  :   self is Attribute on plane of topic in Html4Transitional =

    new Attribute(valueOf[self].tt, plane.reify.map(_.tt).to(Set), false)
    . asInstanceOf[self is Attribute on plane of topic in Html4Transitional]


  def globalAttribute[self <: Label: ValueOf, topic]()
  :   self is Attribute of topic in Html4Transitional =

    new Attribute(valueOf[self].tt, Set(), true)
    . asInstanceOf[self is Attribute of topic in Html4Transitional]

  // Core attributes (most elements)
  given id: ("id" is Attribute of Id) = globalAttribute()
  given `class`: ("class" is Attribute of CssClassList) = globalAttribute()
  given style: ("style" is Attribute of Css) = globalAttribute()
  given title: ("title" is Attribute of Textual) = globalAttribute()

  // Internationalization
  given lang: ("lang" is Attribute of Language) = globalAttribute()
  given dir: ("dir" is Attribute of Dir) = globalAttribute()

  // Event attributes (intrinsic events)
  given onclick: ("onclick" is Attribute of Script) = globalAttribute()
  given ondblclick: ("ondblclick" is Attribute of Script) = globalAttribute()
  given onmousedown: ("onmousedown" is Attribute of Script) = globalAttribute()
  given onmouseup: ("onmouseup" is Attribute of Script) = globalAttribute()
  given onmouseover: ("onmouseover" is Attribute of Script) = globalAttribute()
  given onmousemove: ("onmousemove" is Attribute of Script) = globalAttribute()
  given onmouseout: ("onmouseout" is Attribute of Script) = globalAttribute()
  given onkeypress: ("onkeypress" is Attribute of Script) = globalAttribute()
  given onkeydown: ("onkeydown" is Attribute of Script) = globalAttribute()
  given onkeyup: ("onkeyup" is Attribute of Script) = globalAttribute()
  given onload: ("onload" is Attribute on "body" | "frameset" of Script) = attribute()
  given onunload: ("onunload" is Attribute on "body" | "frameset" of Script) = attribute()

  given onfocus: ("onfocus" is Attribute on
    "a" | "area" | "label" | "input" | "select" | "textarea" | "button" of Script) = attribute()

  given onblur: ("onblur" is Attribute on
    "a" | "area" | "label" | "input" | "select" | "textarea" | "button" of Script) = attribute()

  given onsubmit: ("onsubmit" is Attribute on "form" of Script) = attribute()
  given onreset: ("onreset" is Attribute on "form" of Script) = attribute()

  given onchange: ("onchange" is Attribute on "input" | "select" | "textarea" of Script) =
    attribute()

  given onselect: ("onselect" is Attribute on "input" | "textarea" of Script) = attribute()

  // Per-element attributes
  given href: ("href" is Attribute on "a" | "area" | "base" | "link" of Url) = attribute()
  given src: ("src" is Attribute on "img" | "input" | "script" | "iframe" of Url) = attribute()
  given alt: ("alt" is Attribute on "applet" | "area" | "img" | "input" of Textual) = attribute()

  given `type`: ("type" is Attribute on
    "a" | "link" | "object" | "param" | "script" | "style" | "input" | "button" of Mime) = attribute()

  given typeOl: ("type" is Attribute on "ol" | "ul" | "li" of OlType) = attribute()

  given name: ("name" is Attribute on
    "a" | "applet" | "button" | "form" | "iframe" | "img" | "input" | "map" | "meta" | "object"
    | "param" | "select" | "textarea" of Name) = attribute()

  given value: ("value" is Attribute on "input" | "option" | "param" | "button" of Textual) =
    attribute()

  given valueLi: ("value" is Attribute on "li" of PositiveInt) = attribute()
  given rel: ("rel" is Attribute on "a" | "link" of Tokens) = attribute()
  given rev: ("rev" is Attribute on "a" | "link" of Tokens) = attribute()

  given target: ("target" is Attribute on "a" | "area" | "base" | "form" | "link" of Target) =
    attribute()

  given action: ("action" is Attribute on "form" of Url) = attribute()
  given method: ("method" is Attribute on "form" of Method) = attribute()
  given enctype: ("enctype" is Attribute on "form" of Enctype) = attribute()
  given accept: ("accept" is Attribute on "form" | "input" of MimeList) = attribute()
  given acceptCharset: ("accept-charset" is Attribute on "form" of Charset) = attribute()
  given checked: ("checked" is Attribute on "input" of Presence) = attribute()
  given disabled: ("disabled" is Attribute on
    "button" | "input" | "optgroup" | "option" | "select" | "textarea" of Presence) = attribute()
  given readonly: ("readonly" is Attribute on "input" | "textarea" of Presence) = attribute()
  given multiple: ("multiple" is Attribute on "select" of Presence) = attribute()
  given selected: ("selected" is Attribute on "option" of Presence) = attribute()
  given maxlength: ("maxlength" is Attribute on "input" of PositiveInt) = attribute()
  given size: ("size" is Attribute on "input" | "select" of PositiveInt) = attribute()
  given sizeFont: ("size" is Attribute on "basefont" | "font" | "hr" of Size) = attribute()
  given rows: ("rows" is Attribute on "textarea" of PositiveInt) = attribute()
  given cols: ("cols" is Attribute on "textarea" of PositiveInt) = attribute()
  given wrap: ("wrap" is Attribute on "textarea" of Wrap) = attribute()
  given `for`: ("for" is Attribute on "label" of Id) = attribute()
  given usemap: ("usemap" is Attribute on "img" | "input" | "object" of Url) = attribute()
  given ismap: ("ismap" is Attribute on "img" | "input" of Presence) = attribute()
  given longdesc: ("longdesc" is Attribute on "img" | "iframe" of Url) = attribute()
  given shape: ("shape" is Attribute on "a" | "area" of Shape) = attribute()
  given coords: ("coords" is Attribute on "a" | "area" of Coords) = attribute()
  given nohref: ("nohref" is Attribute on "area" of Presence) = attribute()
  given hreflang: ("hreflang" is Attribute on "a" | "link" of Language) = attribute()

  given charset: ("charset" is Attribute on "a" | "link" | "script" | "meta" of Charset) =
    attribute()

  given media: ("media" is Attribute on "link" | "style" of MediaQueryList) = attribute()
  given defer: ("defer" is Attribute on "script" of Presence) = attribute()
  given language: ("language" is Attribute on "script" of Textual) = attribute()
  given httpEquiv: ("http-equiv" is Attribute on "meta" of HttpEquiv) = attribute()
  given content: ("content" is Attribute on "meta" of Textual) = attribute()
  given scheme: ("scheme" is Attribute on "meta" of Textual) = attribute()
  given cite: ("cite" is Attribute on "blockquote" | "del" | "ins" | "q" of Url) = attribute()
  given datetime: ("datetime" is Attribute on "del" | "ins" of Datetime) = attribute()
  given accesskey: ("accesskey" is Attribute on
    "a" | "area" | "button" | "input" | "label" | "legend" | "textarea" of Textual) = attribute()
  given tabindex: ("tabindex" is Attribute on
    "a" | "area" | "button" | "input" | "object" | "select" | "textarea" of Integral) = attribute()
  given profile: ("profile" is Attribute on "head" of Url) = attribute()
  given version: ("version" is Attribute on "html" of Textual) = attribute()
  given prompt: ("prompt" is Attribute on "isindex" of Textual) = attribute()
  given xmlns: ("xmlns" is Attribute on "html" of Url) = attribute()

  // Form attributes
  given label: ("label" is Attribute on "optgroup" | "option" of Textual) = attribute()

  // Object attributes
  given standby: ("standby" is Attribute on "object" of Textual) = attribute()
  given classid: ("classid" is Attribute on "object" of Url) = attribute()
  given codebase: ("codebase" is Attribute on "applet" | "object" of Url) = attribute()
  given codetype: ("codetype" is Attribute on "object" of Mime) = attribute()
  given archive: ("archive" is Attribute on "applet" | "object" of Url) = attribute()
  given declare: ("declare" is Attribute on "object" of Presence) = attribute()
  given data: ("data" is Attribute on "object" of Url) = attribute()

  // Applet attributes
  given code: ("code" is Attribute on "applet" of Textual) = attribute()
  given mayscript: ("mayscript" is Attribute on "applet" of Presence) = attribute()
  given `object`: ("object" is Attribute on "applet" of Textual) = attribute()

  // Param attributes
  given valuetype: ("valuetype" is Attribute on "param" of Valuetype) = attribute()

  // Iframe attributes
  given frameborder: ("frameborder" is Attribute on "iframe" of Integral) = attribute()
  given scrolling: ("scrolling" is Attribute on "iframe" of Textual) = attribute()
  given marginheight: ("marginheight" is Attribute on "iframe" of PositiveInt) = attribute()
  given marginwidth: ("marginwidth" is Attribute on "iframe" of PositiveInt) = attribute()

  // Table attributes
  given summary: ("summary" is Attribute on "table" of Textual) = attribute()
  given border: ("border" is Attribute on "img" | "object" | "table" of PositiveInt) = attribute()
  given cellpadding: ("cellpadding" is Attribute on "table" of PositiveInt) = attribute()
  given cellspacing: ("cellspacing" is Attribute on "table" of PositiveInt) = attribute()
  given frameTable: ("frame" is Attribute on "table" of Frame) = attribute()
  given rules: ("rules" is Attribute on "table" of Rules) = attribute()
  given colspan: ("colspan" is Attribute on "td" | "th" of PositiveInt) = attribute()
  given rowspan: ("rowspan" is Attribute on "td" | "th" of PositiveInt) = attribute()
  given headers: ("headers" is Attribute on "td" | "th" of Tokens) = attribute()
  given abbr: ("abbr" is Attribute on "td" | "th" of Textual) = attribute()
  given axis: ("axis" is Attribute on "td" | "th" of Textual) = attribute()
  given scope: ("scope" is Attribute on "td" | "th" of Scope) = attribute()
  given valign: ("valign" is Attribute on
    "col" | "colgroup" | "tbody" | "td" | "tfoot" | "th" | "thead" | "tr" of Valign) = attribute()
  given char: ("char" is Attribute on
    "col" | "colgroup" | "tbody" | "td" | "tfoot" | "th" | "thead" | "tr" of Textual) = attribute()
  given charoff: ("charoff" is Attribute on
    "col" | "colgroup" | "tbody" | "td" | "tfoot" | "th" | "thead" | "tr" of Textual) = attribute()
  given span: ("span" is Attribute on "col" | "colgroup" of PositiveInt) = attribute()

  // List attributes
  given start: ("start" is Attribute on "ol" of PositiveInt) = attribute()
  given compact: ("compact" is Attribute on
    "dir" | "dl" | "menu" | "ol" | "ul" of Presence) = attribute()

  // Presentational attributes (Transitional permits)
  given align: ("align" is Attribute on
    "applet" | "caption" | "col" | "colgroup" | "div" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6"
    | "hr" | "iframe" | "img" | "input" | "legend" | "object" | "p" | "table" | "tbody" | "td"
    | "tfoot" | "th" | "thead" | "tr" of Align) = attribute()

  given bgcolor: ("bgcolor" is Attribute on
    "body" | "table" | "td" | "th" | "tr" of Color) = attribute()

  given background: ("background" is Attribute on "body" of Url) = attribute()
  given text: ("text" is Attribute on "body" of Color) = attribute()
  given link: ("link" is Attribute on "body" of Color) = attribute()
  given vlink: ("vlink" is Attribute on "body" of Color) = attribute()
  given alink: ("alink" is Attribute on "body" of Color) = attribute()
  given color: ("color" is Attribute on "basefont" | "font" of Color) = attribute()
  given face: ("face" is Attribute on "basefont" | "font" of Textual) = attribute()
  given noshade: ("noshade" is Attribute on "hr" of Presence) = attribute()
  given clear: ("clear" is Attribute on "br" of Clear) = attribute()
  given width: ("width" is Attribute on
    "applet" | "col" | "colgroup" | "hr" | "iframe" | "img" | "object" | "pre" | "table" | "td"
    | "th" of PositiveInt) = attribute()
  given height: ("height" is Attribute on
    "applet" | "iframe" | "img" | "object" | "td" | "th" of PositiveInt) = attribute()
  given hspace: ("hspace" is Attribute on "applet" | "img" | "object" of PositiveInt) = attribute()
  given vspace: ("vspace" is Attribute on "applet" | "img" | "object" of PositiveInt) = attribute()
  given nowrap: ("nowrap" is Attribute on "td" | "th" of Presence) = attribute()


class Html4Transitional() extends Dom:
  import Html4Transitional.*

  def doctype: Doctype =
    Doctype(t"""HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd"""")

  // Content categories
  type Heading = "h1" | "h2" | "h3" | "h4" | "h5" | "h6"

  type Phrase =
    "em" | "strong" | "dfn" | "code" | "samp" | "kbd" | "var" | "cite" | "abbr" | "acronym"

  type FontStyle =
    "tt" | "i" | "b" | "u" | "s" | "strike" | "big" | "small"

  type Special =
    "a" | "img" | "applet" | "object" | "font" | "basefont" | "br" | "script" | "map" | "q"
    | "sub" | "sup" | "span" | "bdo" | "iframe"

  type FormCtrl = "input" | "select" | "textarea" | "label" | "button"

  type Inline = Phrase | FontStyle | Special | FormCtrl | "#text"

  type Lists = "ul" | "ol" | "dir" | "menu"

  type Block =
    Heading | Lists | "p" | "pre" | "dl" | "div" | "noscript" | "blockquote" | "form" | "hr"
    | "table" | "fieldset" | "address" | "center" | "isindex" | "noframes"

  type Flow = Block | Inline

  type Metadata = "title" | "base" | "script" | "style" | "meta" | "link" | "object"

  def insertable(tag: Tag): Set[Tag] =
    tag.admissible.map(elements(_)).compact.filter(_.insertable)

  def infer(parent: Tag, child: Tag): Optional[Tag] =
    def recur(parent: Tag): Boolean =
      parent.admissible.contains(child.label) || insertable(parent).exists(recur(_))

    insertable(parent).find(recur(_)).optional


  // Elements
  val A = Tag.transparent["a", "", Html4Transitional]()
  val Abbr = Tag.container["abbr", Inline, Html4Transitional]()
  val Acronym = Tag.container["acronym", Inline, Html4Transitional]()
  val Address = Tag.container["address", Inline | "p", Html4Transitional]()
  val Applet = Tag.container["applet", "param" | Flow, Html4Transitional]()
  val Area = Tag.void["area", Html4Transitional]()
  val B = Tag.container["b", Inline, Html4Transitional]()
  val Base = Tag.void["base", Html4Transitional]()
  val Basefont = Tag.void["basefont", Html4Transitional]()
  val Bdo = Tag.container["bdo", Inline, Html4Transitional]()
  val Big = Tag.container["big", Inline, Html4Transitional]()
  val Blockquote = Tag.container["blockquote", Flow, Html4Transitional]()
  val Body = Tag.container["body", Flow, Html4Transitional](autoclose = true, insertable = true)
  val Br = Tag.void["br", Html4Transitional]()

  val Button =
    Tag.container["button", Flow, Html4Transitional](boundary = true)

  val Caption = Tag.container["caption", Inline, Html4Transitional](boundary = true)
  val Center = Tag.container["center", Flow, Html4Transitional]()
  val Cite = Tag.container["cite", Inline, Html4Transitional]()
  val Code = Tag.container["code", Inline, Html4Transitional]()
  val Col = Tag.void["col", Html4Transitional]()

  val Colgroup =
    Tag.container["colgroup", "col", Html4Transitional]
      ( mode = Html.Mode.Whitespace, insertable = true )

  val Dd = Tag.container["dd", Flow, Html4Transitional](autoclose = true)
  val Del = Tag.transparent["del", "", Html4Transitional]()
  val Dfn = Tag.container["dfn", Inline, Html4Transitional]()
  val Dir = Tag.container["dir", "li", Html4Transitional](mode = Html.Mode.Whitespace)
  val Div = Tag.container["div", Flow, Html4Transitional]()

  val Dl =
    Tag.container["dl", "dt" | "dd", Html4Transitional]
      ( autoclose = true, mode = Html.Mode.Whitespace )

  val Dt = Tag.container["dt", Inline, Html4Transitional](autoclose = true)
  val Em = Tag.container["em", Inline, Html4Transitional]()
  val Fieldset = Tag.container["fieldset", "legend" | Flow, Html4Transitional]()
  val Font = Tag.container["font", Inline, Html4Transitional]()
  val Form = Tag.container["form", Flow, Html4Transitional]()
  val H1 = Tag.container["h1", Inline, Html4Transitional]()
  val H2 = Tag.container["h2", Inline, Html4Transitional]()
  val H3 = Tag.container["h3", Inline, Html4Transitional]()
  val H4 = Tag.container["h4", Inline, Html4Transitional]()
  val H5 = Tag.container["h5", Inline, Html4Transitional]()
  val H6 = Tag.container["h6", Inline, Html4Transitional]()

  val Head =
    Tag.container["head", Metadata, Html4Transitional]
      ( autoclose = true, mode = Html.Mode.Whitespace, insertable = true )

  val Hr = Tag.void["hr", Html4Transitional]()
  lazy val Html = honeycomb.Html
  val I = Tag.container["i", Inline, Html4Transitional]()
  val Iframe = Tag.container["iframe", Flow, Html4Transitional]()
  val Img = Tag.void["img", Html4Transitional]()

  object Input extends Tag.Void("input", sci.Map(), false):
    type Topic = "input"
    type Transport = ""
    type Form = Html4Transitional

    val Hidden = Tag.void["input", Html4Transitional](presets = sci.Map(t"type" -> t"hidden"))
    val Text = Tag.void["input", Html4Transitional](presets = sci.Map(t"type" -> t"text"))
    val Password = Tag.void["input", Html4Transitional](presets = sci.Map(t"type" -> t"password"))
    val Checkbox = Tag.void["input", Html4Transitional](presets = sci.Map(t"type" -> t"checkbox"))
    val Radio = Tag.void["input", Html4Transitional](presets = sci.Map(t"type" -> t"radio"))
    val Submit = Tag.void["input", Html4Transitional](presets = sci.Map(t"type" -> t"submit"))
    val Image = Tag.void["input", Html4Transitional](presets = sci.Map(t"type" -> t"image"))
    val Reset = Tag.void["input", Html4Transitional](presets = sci.Map(t"type" -> t"reset"))
    val Button = Tag.void["input", Html4Transitional](presets = sci.Map(t"type" -> t"button"))
    val File = Tag.void["input", Html4Transitional](presets = sci.Map(t"type" -> t"file"))

  val Ins = Tag.transparent["ins", "", Html4Transitional]()
  val Isindex = Tag.void["isindex", Html4Transitional]()
  val Kbd = Tag.container["kbd", Inline, Html4Transitional]()
  val Label = Tag.container["label", Inline, Html4Transitional]()

  val Legend = Tag.container["legend", Inline, Html4Transitional]()
  val Li = Tag.container["li", Flow, Html4Transitional](autoclose = true)
  val Link = Tag.void["link", Html4Transitional]()
  val Map = Tag.container["map", Block | "area", Html4Transitional]()
  val Menu = Tag.container["menu", "li", Html4Transitional](mode = Html.Mode.Whitespace)
  val Meta = Tag.void["meta", Html4Transitional]()
  val Noframes = Tag.container["noframes", Flow, Html4Transitional]()
  val Noscript = Tag.container["noscript", Flow, Html4Transitional]()
  val Object = Tag.container["object", "param" | Flow, Html4Transitional](boundary = true)
  val Ol = Tag.container["ol", "li", Html4Transitional](mode = Html.Mode.Whitespace)

  val Optgroup =
    Tag.container["optgroup", "option", Html4Transitional]
      ( autoclose = true, mode = Html.Mode.Whitespace )

  val Option = Tag.container["option", "#text", Html4Transitional](autoclose = true)
  val P = Tag.container["p", Inline, Html4Transitional](autoclose = true)
  val Param = Tag.void["param", Html4Transitional]()
  val Pre = Tag.container["pre", Inline, Html4Transitional]()
  val Q = Tag.container["q", Inline, Html4Transitional]()
  val S = Tag.container["s", Inline, Html4Transitional]()
  val Samp = Tag.container["samp", Inline, Html4Transitional]()
  val Script = Tag.container["script", "#text", Html4Transitional](mode = Html.Mode.Raw)

  val Select =
    Tag.container["select", "option" | "optgroup", Html4Transitional](mode = Html.Mode.Whitespace)

  val Small = Tag.container["small", Inline, Html4Transitional]()
  val Span = Tag.container["span", Inline, Html4Transitional]()
  val Strike = Tag.container["strike", Inline, Html4Transitional]()
  val Strong = Tag.container["strong", Inline, Html4Transitional]()
  val Style = Tag.container["style", "#text", Html4Transitional](mode = Html.Mode.Raw)
  val Sub = Tag.container["sub", Inline, Html4Transitional]()
  val Sup = Tag.container["sup", Inline, Html4Transitional]()

  val Table =
    Tag.container
      [ "table", "caption" | "colgroup" | "thead" | "tbody" | "tfoot", Html4Transitional ]
      ( mode = Html.Mode.Whitespace, boundary = true )

  val Tbody = Tag.container["tbody", "tr", Html4Transitional]
    ( autoclose = true, mode = Html.Mode.Whitespace, insertable = true )

  val Td = Tag.container["td", Flow, Html4Transitional](autoclose = true, boundary = true)
  val Textarea = Tag.container["textarea", "#text", Html4Transitional](mode = Html.Mode.Rcdata)

  val Tfoot = Tag.container["tfoot", "tr", Html4Transitional]
    ( autoclose = true, mode = Html.Mode.Whitespace )

  val Th = Tag.container["th", Flow, Html4Transitional](autoclose = true, boundary = true)

  val Thead = Tag.container["thead", "tr", Html4Transitional]
    ( autoclose = true, mode = Html.Mode.Whitespace )

  val Title = Tag.container["title", "#text", Html4Transitional](mode = Html.Mode.Rcdata)

  val Tr =
    Tag.container["tr", "td" | "th", Html4Transitional]
      ( autoclose = true, mode = Html.Mode.Whitespace, insertable = true )

  val Tt = Tag.container["tt", Inline, Html4Transitional]()
  val U = Tag.container["u", Inline, Html4Transitional]()
  val Ul = Tag.container["ul", "li", Html4Transitional](mode = Html.Mode.Whitespace)
  val Var = Tag.container["var", Inline, Html4Transitional]()

  val elements: Dictionary[Tag] =
    Dictionary(this.membersOfType[Tag].to(Seq).bi.map(_.label -> _)*)

  val entities: Dictionary[Text] =
    val list = cp"/honeycomb/entities-html4.tsv".read[Text].cut(t"\n").map(_.cut(t"\t")).collect:
      case List(key, value) => (key, value)

    Dictionary(list*)

  val attributes: Dictionary[Attribute] =
    val list: List[(Text, Attribute)] =
      Html4Transitional.membersOfType[honeycomb.Attribute]
      . foldLeft(sci.Map[Text, Attribute]()): (map, next) =>
        val coerced = next.asInstanceOf[Attribute]
        map.updated(coerced.label, map.at(coerced.label).let(_.merge(coerced).asInstanceOf[Attribute]).or(coerced))

      . to(List)

    Dictionary(list*)
