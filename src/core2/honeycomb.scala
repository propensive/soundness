package honeycomb

import annotation.*

import language.dynamics

type ContentNode[C] = EmptyTag[_] | Node[C] | String

def flatten[C](nodes: Seq[ContentNode[C] | Seq[ContentNode[C]]]): Seq[ContentNode[C]] =
  nodes.flatMap {
    case node: ContentNode[C]     => Seq(node)
    case seq: Seq[ContentNode[C]] => seq
  }

case class Node[+TagType](tag: EmptyTag[_],
                          attributes: Map[String, String],
                          verbatim: Boolean,
                          children: Seq[ContentNode[_]]):
  lazy val inline: Boolean = children.forall {
    case node: Node[_]      => node.tag.inline && node.inline
    case seq: Seq[_]        => seq.forall {
      case tag: EmptyTag[_]   => tag.inline
      case text: String       => true
      case node: Node[_]      => node.tag.inline && node.inline
    }
    case tag: EmptyTag[_]   => tag.inline
    case text: String       => true
  }

class EmptyTag[Atts <: String]
              (val tagName: String, val inline: Boolean = false, val unclosed: Boolean = true) extends Dynamic:
  def applyDynamicNamed(method: "apply")(attributes: (Atts, String)*): Node[this.type] =
    Node(this, attributes.to(Map), false, Nil)

case class TransparentExtrasTag[Children, Extras, Atts <: String]
                               (name: String,
                                override val unclosed: Boolean = false,
                                override val inline: Boolean = false) extends EmptyTag(name, inline, unclosed):
  def applyDynamic[C <: Children](method: "apply")(children: (ContentNode[C | Extras] | Seq[ContentNode[C | Extras]])*): Node[C] =
    Node(this, Map(), false, flatten(children))
  
  def applyDynamicNamed[C <: Children]
                       (method: "apply")
                       (attributes: (Atts, String)*)
                       (children: (ContentNode[C | Extras] | Seq[ContentNode[C | Extras]])*): Node[C] =
    Node(this, attributes.to(Map), false, flatten(children))

case class TransparentTag[Children, Atts <: String]
                         (name: String,
                          override val unclosed: Boolean = false,
                          override val inline: Boolean = false) extends EmptyTag(name, inline, unclosed):
  def applyDynamic[C <: Children](method: "apply")(children: (ContentNode[C] | Seq[ContentNode[C]])*): Node[C] =
    Node(this, Map(), false, flatten(children))
  
  def applyDynamicNamed[C <: Children]
                       (method: "apply")
                       (attributes: (Atts, String)*)
                       (children: (ContentNode[C] | Seq[ContentNode[C]])*): Node[C] =
    Node(this, attributes.to(Map), false, flatten(children))

case class Tag[Children, Atts <: String]
              (name: String,
               override val unclosed: Boolean = false,
               override val inline: Boolean = false,
               val verbatim: Boolean = false) extends EmptyTag(name, inline, unclosed):
  def applyDynamic(method: "apply")
                  (children: (ContentNode[Children] | Seq[ContentNode[Children]])*): Node[this.type] =
    Node(this, Map(), verbatim, flatten[Children](children))
  
  def applyDynamicNamed(method: "apply")
                       (attributes: (Atts, String)*)
                       (children: (ContentNode[Children] | Seq[ContentNode[Children]])*): Node[this.type] =
    Node(this, attributes.to(Map), verbatim, flatten(children))

object A extends TransparentTag[NonInteractive, Global | "href" | "target" | "download" | "ping" | "rel" |
    "hreflang" | "type" | "referrerpolicy"]("a", inline = true)
object Abbr extends Tag[Phrasing, Global]("abbr", inline = true)
object Address extends Tag[Flow | Palpable, Global]("address")

object Area extends EmptyTag[Global | "alt" | "coords" | "shape" | "href" | "target" | "download" | "ping" |
    "rel" | "referrerpolicy"]("area", inline = true)

object Article extends Tag[Flow, Global]("article") // further constraints on descendants
object Aside extends Tag[Flow, Global]("aside")

object Audio extends TransparentExtrasTag[Any, Track.type, Global | "src" | "crossorigin" | "preload" |
    "autoplay" | "loop" | "muted" | "controls"]("audio", inline = true) // complicated!

object B extends Tag[Phrasing, Global]("b", inline = true)
object Base extends EmptyTag[Global | "href" | "target"]("base")
object Bdi extends Tag[Phrasing, Global]("bdi", inline = true)
object Bdo extends Tag[Phrasing, Global]("bdo", inline = true)
object Blockquote extends Tag[Flow, Global | "cite"]("blockquote")

object Body extends Tag[Flow, Global | "onafterprint" | "onbeforeprint" | "onbeforeunload" | "onhashchange" |
    "onlanguagechange" | "onmessage" | "onmessageerror" | "onoffline" | "ononline" | "onpagehide" |
    "onpageshow" | "onpopstate" | "onrejectionhandled" | "onstorage" | "onunhandledrejection" |
    "onunload"]("body")

object Br extends EmptyTag[Global]("br", inline = true)

object Button extends Tag[Phrasing, Global | "disabled" | "form" | "formaction" | "formenctype" | "formmethod" |
    "formnovalidate" | "formtarget" | "name" | "type" | "value"]("button", inline = true)

object Canvas extends TransparentTag[NonInteractive, Global | "width" | "height"]("canvas", inline = true) // complicated
object Caption extends Tag[Flow, Global]("caption") // no tables
object Cite extends Tag[Phrasing, Global]("cite", inline = true)
object Code extends Tag[Phrasing, Global]("code", inline = true)
object Col extends EmptyTag[Global | "span"]("col")
object Colgroup extends Tag[Col.type | Template.type, Global | "span"]("colgroup")
object Data extends Tag[Phrasing, Global | "value"]("data", inline = true)
object Datalist extends Tag[Phrasing | Option.type | ScriptSupporting, Global]("datalist", inline = true)
object Dd extends Tag[Flow, Global]("dd")
object Del extends TransparentTag[Any, Global | "cite" | "datetime"]("del", inline = true)
object Details extends Tag[Summary.type | Flow, Global | "open"]("details")
object Dfn extends Tag[Phrasing, Global]("dfn", inline = true)
object Dialog extends Tag[Flow, Global | "open"]("dialog")
object Div extends Tag[Flow, Global]("div")
object Dl extends Tag[Dt.type | DlType | Div.type, Global]("dl")
object Dt extends Tag[Flow, Global]("dt") // further constraints
object Em extends Tag[Phrasing, Global]("em", inline = true)
object Embed extends EmptyTag[Global | "src" | "type" | "width" | "height"]("embed", inline = true)
object Fieldset extends Tag[Legend.type | Flow, Global | "disabled" | "form" | "name"]("fieldset")
object Figcaption extends Tag[Flow, Global]("figcaption")
object Figure extends Tag[Figcaption.type | Flow, Global]("figure") // first or last element may be figcaption, but not both
object Footer extends Tag[Flow, Global]("footer")

object Form extends Tag[Flow, Global | "acceptCharset" | "action" | "autocomplete" | "enctype" | "method" |
    "name" | "novalidate" | "target" | "rel"]("form")

object H1 extends Tag[Phrasing, Global]("h1")
object H2 extends Tag[Phrasing, Global]("h2")
object H3 extends Tag[Phrasing, Global]("h3")
object H4 extends Tag[Phrasing, Global]("h4")
object H5 extends Tag[Phrasing, Global]("h5")
object H6 extends Tag[Phrasing, Global]("h6")
object HMap extends TransparentTag[Phrasing | Flow | Palpable, Global | "name"]("map", inline = true)
object Head extends Tag[Metadata, Global]("head")
object Header extends Tag[Flow, Global]("header")
object Hgroup extends Tag[H1.type | H2.type | H3.type | H4.type | H5.type | H6.type, Global]("hgroup")
object Hr extends EmptyTag[Global]("hr")
object Html extends Tag[Head.type | Body.type, Global]("html") // head followed by body
object I extends Tag[Phrasing, Global]("i", inline = true)

object Iframe extends EmptyTag[Global | "src" | "srcdoc" | "name" | "sandbox" | "allow" | "allowfullscreen" |
    "width" | "height" | "referrerpolicy" | "loading"]("iframe", unclosed = false, inline = true)

object Img extends EmptyTag[Global | "alt" | "src" | "srcset" | "sizes" | "crossorigin" | "usemap" | "ismap" |
    "width" | "height" | "referrerpolicy" | "decoding" | "loading"]("img", inline = true)

object Input extends EmptyTag[Global | "accept" | "alt" | "autocomplete" | "checked" | "dirname" | "disabled" |
    "form" | "formaction" | "formenctype" | "formmethod" | "formnovalidate" | "formtarget" | "height" | "list" |
    "max" | "maxlength" | "min" | "minlength" | "multiple" | "name" | "pattern" | "placeholder" | "readonly" |
    "required" | "size" | "src" | "step" | "type" | "value" | "width"]("input", inline = true)

object Ins extends TransparentTag[Any, Global | "cite" | "datetime"]("ins", inline = true)
object Kbd extends Tag[Phrasing, Global]("kbd", inline = true)
object Label extends Tag[Phrasing, Global | "for"]("label", inline = true)
object Legend extends Tag[Phrasing | Heading, Global]("legend")
object Li extends Tag[Flow, Global | "value"]("li")

object Link extends EmptyTag[Global | "href" | "crossorigin" | "rel" | "media" | "integrity" | "hreflang" |
    "type" | "referrerpolicy" | "sizes" | "imagesrcset" | "imagesizes" | "as" | "color" | "disabled"]("link",
    inline = true)

object Main extends Tag[Flow, Global]("main")
object Mark extends Tag[Phrasing, Global]("mark", inline = true)
object Menu extends Tag[Flow, Global]("menu")
object Meta extends EmptyTag[Global | "name" | "httpEquiv" | "content" | "charset"]("meta", inline = true)

object Meter extends Tag[Phrasing, Global | "value" | "min" | "max" | "low" | "high" | "optimum"]("meter",
    inline = true)

object Nav extends Tag[Flow, Global]("nav")
object Noscript extends TransparentExtrasTag[Any, Link.type | Style.type | Meta.type, Global]("noscript", inline = true)

object Object extends TransparentExtrasTag[Any, Param.type, Global | "data" | "type" | "name" | "form" |
    "width" | "height"]("object", inline = true)

object Ol extends Tag[Li.type | ScriptSupporting, Global | "reversed" | "start" | "type"]("ol")
object Optgroup extends Tag[Option.type | ScriptSupporting, Global | "disabled" | "label"]("optgroup")
object Option extends Tag[Nothing, Global | "disabled" | "label" | "selected" | "value"]("option")
object Output extends Tag[Phrasing, Global | "for" | "form" | "name"]("output", inline = true)
object P extends Tag[Phrasing, Global]("p")
object Param extends EmptyTag[Global | "name" | "value"]("param")
object Picture extends Tag[Source.type | Img.type | ScriptSupporting, Global]("picture", inline = true)
object Pre extends Tag[Phrasing, Global]("pre", verbatim = true)
object Progress extends Tag[Phrasing, Global | "value" | "max"]("progress", inline = true)
object Q extends Tag[Phrasing, Global | "cite"]("q", inline = true)
object Rb extends Tag[Phrasing, Global]("rb")
object Rp extends Tag[Nothing, Global]("rp")
object Rt extends Tag[Phrasing, Global]("rt")
object Ruby extends Tag[Phrasing | Rp.type | Rt.type, Global]("ruby", inline = true)
object S extends Tag[Phrasing, Global]("s", inline = true)
object Samp extends Tag[Phrasing, Global]("samp", inline = true)

object Script extends Tag[Nothing, Global | "src" | "type" | "nomodule" | "async" | "defer" | "crossorigin" |
    "integrity" | "referrerpolicy"]("script", inline = true)

object Section extends Tag[Flow, Global]("section")

object Select extends Tag[Option.type | Optgroup.type | ScriptSupporting, Global | "autocomplete" | "disabled" |
    "form" | "multiple" | "name" | "required" | "size"]("select", inline = true)

object Slot extends TransparentTag[Any, Global | "name"]("slot", inline = true)
object Small extends Tag[Phrasing, Global]("small", inline = true)
object Source extends EmptyTag[Global | "type" | "src" | "srcset" | "sizes" | "media" | "width" | "height"]("source")
object Span extends Tag[Phrasing, Global]("span", inline = true)
object Strong extends Tag[Phrasing, Global | "media"]("strong", inline = true)
object Style extends Tag[Nothing, Global]("style")
object Sub extends Tag[Phrasing, Global]("sub", inline = true)
object Summary extends Tag[Phrasing | Heading, Global]("summary")
object Sup extends Tag[Phrasing, Global]("sup", inline = true)
object Table extends Tag[Caption.type | Colgroup.type | Thead.type | Tbody.type | Tr.type | Tfoot.type | ScriptSupporting, Global]("table")
object Tbody extends Tag[Tr.type | ScriptSupporting, Global]("tbody")
object Td extends Tag[Flow, Global | "colspan" | "rowspan" | "headers"]("td")
object Template extends EmptyTag[Global]("template", unclosed = false, inline = true)

object Textarea extends Tag[Nothing, Global | "autocomplete" | "cols" | "dirname" | "disabled" | "form" |
    "maxlength" | "minlength" | "name" | "placeholder" | "readonly" | "required" | "rows" | "wrap"]("textarea",
    inline = true)

object Tfoot extends Tag[Tr.type | ScriptSupporting, Global]("tfoot")
object Th extends Tag[Flow, Global | "colspan" | "rowspan" | "headers" | "scope" | "abbr"]("th")
object Thead extends Tag[Tr.type | ScriptSupporting, Global]("thead")
object Time extends Tag[Phrasing, Global | "datetime"]("time", inline = true)
object Title extends Tag[Nothing, Global]("title")
object Tr extends Tag[Td.type | Th.type | ScriptSupporting, Global]("tr")
object Track extends EmptyTag[Global | "kind" | "src" | "srclang" | "label" | "default"]("track")
object U extends Tag[Phrasing, Global]("u", inline = true)
object Ul extends Tag[Li.type | ScriptSupporting, Global]("ul")
object Var extends Tag[Nothing, Global]("var", inline = true)

object Video extends TransparentExtrasTag[Any, Track.type | Source.type, Global | "src" | "crossorigin" |
    "poster" | "preload" | "autoplay" | "playsinline" | "loop" | "muted" | "controls" | "width" | "height"]
    ("video", inline = true) // complicated!

object Wbr extends EmptyTag[Global]("wbr", inline = true)
type DlType = Dl.type



type Metadata = Base.type | Link.type | Meta.type | Noscript.type | Script.type | Style.type | Template.type |
    Title.type

type Phrasing = A.type | Abbr.type | Area.type | Audio.type | B.type | Bdi.type | Bdo.type | Br.type |
    Button.type | Canvas.type | Cite.type | Code.type | Data.type | Datalist.type | Del.type | Dfn.type |
    Em.type | Embed.type | I.type | Iframe.type | Img.type | Input.type | Ins.type | Kbd.type | Label.type |
    Link.type | HMap.type | Mark.type | Meta.type | Meter.type | Noscript.type | Object.type |
    Output.type | Picture.type | Progress.type | Q.type | Ruby.type | S.type | Samp.type | Script.type |
    Select.type | Slot.type | Small.type | Span.type | Strong.type | Sub.type | Sup.type |
    Template.type | Textarea.type | Time.type | U.type | Var.type | Video.type | Wbr.type

type Flow = A.type | Abbr.type | Address.type | Area.type | Article.type | Aside.type | Audio.type | B.type |
    Bdi.type | Bdo.type | Blockquote.type | Br.type | Button.type | Canvas.type | Cite.type | Code.type |
    Data.type | Datalist.type | Del.type | Details.type | Dfn.type | Dialog.type | Div.type | Dl.type |
    Em.type | Embed.type | Fieldset.type | Figure.type | Footer.type | Form.type | H1.type | H2.type | H3.type |
    H4.type | H5.type | H6.type | Header.type | Hgroup.type | Hr.type | I.type | Iframe.type | Img.type |
    Input.type | Ins.type | Kbd.type | Label.type | Link.type | Main.type | HMap.type | Mark.type |
    Menu.type | Meta.type | Meter.type | Nav.type | Noscript.type | Object.type | Ol.type | Output.type |
    P.type | Picture.type | Pre.type | Progress.type | Q.type | Ruby.type | S.type | Samp.type | Script.type |
    Section.type | Select.type | Slot.type | Small.type | Span.type | Strong.type | Sub.type | Sup.type |
    Table.type | Template.type | Textarea.type | Time.type | U.type | Ul.type | Var.type |
    Video.type | Wbr.type

type Sectioning = Article.type | Aside.type | Nav.type | Section.type

type Heading = H1.type | H2.type | H3.type | H4.type | H5.type | H6.type | Hgroup.type

type Embedded = Audio.type | Canvas.type | Embed.type | Iframe.type | Img.type | Object.type |
    Picture.type | Video.type

type Interactive = A.type | Audio.type | Button.type | Details.type | Embed.type | Iframe.type | Img.type |
    Input.type | Label.type | Select.type | Textarea.type | Video.type

type Palpable = A.type | Abbr.type | Address.type | Article.type | Aside.type | Audio.type | B.type | Bdi.type |
    Bdo.type | Blockquote.type | Button.type | Canvas.type | Cite.type | Code.type | Data.type | Details.type |
    Dfn.type | Div.type | Dl.type | Em.type | Embed.type | Fieldset.type | Figure.type | Footer.type |
    Form.type | H1.type | H2.type | H3.type | H4.type | H5.type | H6.type | Header.type | Hgroup.type | I.type |
    Iframe.type | Img.type | Input.type | Ins.type | Kbd.type | Label.type | Main.type | HMap.type | Mark.type |
    Menu.type | Meter.type | Nav.type | Object.type | Ol.type | Output.type | P.type | Pre.type |
    Progress.type | Q.type | Ruby.type | S.type | Samp.type | Section.type | Select.type | Small.type |
    Span.type | Strong.type | Sub.type | Sup.type | Table.type | Textarea.type | Time.type | U.type |
    Ul.type | Var.type | Video.type

type ScriptSupporting = Script.type | Template.type

type NonInteractive = Abbr.type | Address.type | Area.type | Article.type | Aside.type | Audio.type | B.type |
    Base.type | Bdi.type | Bdo.type | Blockquote.type | Br.type | Canvas.type | Cite.type | Code.type |
    Data.type | Datalist.type | Del.type | Dfn.type | Dialog.type | Div.type | Dl.type | Em.type |
    Fieldset.type | Figure.type | Footer.type | Form.type | H1.type | H2.type | H3.type | H4.type | H5.type |
    H6.type | Header.type | Hgroup.type | Hr.type | I.type | Img.type | Input.type | Ins.type | Kbd.type |
    Link.type | Main.type | HMap.type | Mark.type | Menu.type | Meta.type | Meter.type | Nav.type |
    Noscript.type | Object.type | Ol.type | Output.type | P.type | Picture.type | Pre.type | Progress.type |
    Q.type | Ruby.type | S.type | Samp.type | Script.type | Section.type | Slot.type | Small.type | Span.type |
    Strong.type | Style.type | Sub.type | Sup.type | Table.type | Template.type | Time.type |
    Title.type | U.type | Ul.type | Var.type | Wbr.type


// Attributes

type Global = "accesskey" | "autocapitalize" | "autofocus" | "contenteditable" | "dir" | "draggable" |
    "enterkeyhint" | "hidden" | "inputmode" | "is" | "itemid" | "itemprop" | "itemref" | "itemscope" |
    "itemtype" | "lang" | "nonce" | "spellcheck" | "style" | "tabindex" | "title" | "translate" | EventHandlers

type EventHandlers = "onabort" | "onauxclick" | "oncancel" | "oncanplay" | "oncanplaythrough" | "onchange" | "onclick" | "onclos" | "oncontextmenu" | "oncuechange" | "ondblclick" | "ondrag" | "ondragend" | "ondragenter" | "ondragleave" | "ondragover" | "ondragstart" | "ondrop" | "ondurationchange" | "onemptied" | "onended" | "onformdata" | "oninput" | "oninvalid" | "onkeydown" | "onkeypress" | "onkeyup" | "onloadeddata" | "onloadedmetadata" | "onloadstart" | "onmousedown" | "onmouseenter" | "onmouseleave" | "onmousemove" | "onmouseout" | "onmouseover" | "onmouseup" | "onpause" | "onplay" | "onplaying" | "onprogress" | "onratechange" | "onreset" | "onsecuritypolicyviolation" | "onseeked" | "onseeking" | "onselect" | "onslotchange" | "onstalled" | "onsubmit" | "onsuspend" | "ontimeupdate" | "ontoggle" | "onvolumechange" | "onwaiting" | "onwebkitanimationend" | "onwebkitanimationiteration" | "onwebkitanimationstart" | "onwebkittransitionend" | "onwheel" | "onblur" | "onerror" | "onfocus" | "onload" | "onresize" | "onscroll"

object Example:
  
  val html: Document = Document(Html(
    Head(
      Base(itemref = "bar"),
      Meta(itemscope = "y")
    ),
    Body(
      A(style = "hello")(P("Hello world"))
    ),
    Body(
      Table(
        Thead(style = "value")(Tr()),
        Tbody(
          Tr(
            Td("Text content. The quick brown fox jumps over the lazy dog. ", B(" with bold"), Br, "text",
              P("""Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut
                   labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco
                   laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in
                   voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat
                   cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."""),
              Video(List(
                Source(lang = "hello"),
                Source(nonce = "hello"),
                Track(title = "bar"),
                P("hello"))
              )
            )
          ),
          Tr(
            Td("Text content"),
            Td("Text content ", B("with bold in the middle of this text, limited to a certain length"), " text")
          )
        ),
      )
    )
  ))

case class Document(root: Node[Html.type])

object Document:
  def serialize(doc: Document, maxWidth: Int = -1): String =
    var indent: Int = 0
    var linebreak: Boolean = false
    val buf: StringBuilder = StringBuilder()
    var emptyLine = true
    var pos: Int = 0
    
    def newline(n: Int = 0): Unit =
      indent += n
      linebreak = true

    def append(strings: String*): Unit =
      for str <- strings do
        buf.append(str)
        pos += str.length
      emptyLine = false

    def whitespace(): Unit =
      if linebreak then
        buf.append("\n")
        for i <- 1 to indent do buf.append("  ")
        pos = indent*2
      linebreak = false
      emptyLine = true

    def next(node: ContentNode[_], verbatim: Boolean): Unit = node match
      case tag: EmptyTag[_] =>
        next(Node(tag, Map(), false, Nil), false)
      case node: Node[_] =>
        whitespace()
        append("<", node.tag.tagName)
        for (key, value) <- node.attributes do append(" ", key, "=\"", value, "\"")
        append(">")
        if !node.inline then newline(1)
        for child <- node.children do
          val splitLine = child match
            case node: Node[_] => !node.inline || !node.tag.inline
            case _             => false
          if splitLine then newline()
          next(child, node.verbatim)
          if splitLine then newline()
        if !node.inline then newline(-1)
        if !node.tag.unclosed then
          whitespace()
          append("</", node.tag.tagName, ">")
          if !node.inline then newline(0)
      case text: String =>
        whitespace()
        if maxWidth == -1 then append(text) else
          if verbatim || pos + text.length <= maxWidth then append(text)
          else
            text.split("\\s+").foreach { word =>
              if !(pos + 1 + word.length < maxWidth || emptyLine) then
                linebreak = true
                whitespace()
                append(" ")
              append(if !emptyLine then " " else "", word)
            }
            if text.last.isWhitespace then append(" ")
    
    append("<!DOCTYPE html>\n")
    next(doc.root, false)

    buf.toString


@main def run(): Unit =
  println(Document.serialize(Example.html, 100))